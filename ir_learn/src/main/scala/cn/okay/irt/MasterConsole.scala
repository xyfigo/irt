package cn.okay.irt

import cn.okay.irt.context.{OutputHandler, TaskBuilder}
import cn.okay.irt.learn.{AUCValidation, DataHandler, IRTLearner}
import org.apache.spark.sql.{DataFrame, Row}
import org.slf4j.LoggerFactory

/**
  * Created by zhangyikuo on 2017/4/20.
  */
object MasterConsole {
  private val log = LoggerFactory.getLogger(this.getClass)

  private var lastTime: Long = System.currentTimeMillis()

  /**
    * 将原始response的tuple数据，按照学生和问题进行分组
    *
    * @param rows 原始response的tuple数据
    * @return 格式为(按照学生分组所得数据,按照问题分组所得数据)
    */
  private def extracDataMaps(rows: Iterable[Row]): (Map[Long, Array[(Long, Double)]], Map[Long, Array[(Long, Double)]]) = {
    val recs = rows.map { rec =>
      (rec(0).toString.toLong, rec(1).toString.toLong, rec(2).toString.toDouble)
    }.toSeq

    val sDataMap = recs.groupBy(_._1).map { case (sId, tuples) =>
      (sId, tuples.map(tuple => (tuple._2, tuple._3)).toArray)
    }
    val qDataMap = recs.groupBy(_._2).map { case (qId, tuples) =>
      (qId, tuples.map(tuple => (tuple._1, tuple._3)).toArray)
    }

    (sDataMap, qDataMap)
  }

  private def printInfo(info: String) {
    log.info(info)
    println(info)
  }

  private def recordTime(lastTime: Long, info: String): Long = {
    val curTime = System.currentTimeMillis()
    printInfo(info + " takes: " + (curTime - lastTime) / 1000 + "s")
    curTime
  }

  private def learn(data: DataFrame, superParam: (Array[Double], Array[Double])) = {
    val learnedData = data.rdd.groupBy(_.get(3).toString.toLong).map { case (topicId, rows) =>
      val (sDataMap, qDataMap) = extracDataMaps(rows)

      val sModel = TaskBuilder.getStudentModel
      sModel.initParams(sDataMap)
      val qModel = TaskBuilder.getQuestionModel
      qModel.initParams(qDataMap)

      if (null != superParam) {
        sModel.initSuperParams(superParam._1)
        qModel.initSuperParams(superParam._2)
      }

      val learner = new IRTLearner().setStudentModel(sModel).setStudentDataMap(sDataMap)
        .setQuestionModel(qModel).setQuestionDataMap(qDataMap)
      learner.learn()

      (topicId, learner.sParamMap, learner.qParamMap)
    }.cache()

    // 立即计算，避免laziness
    printInfo("total topics:  " + learnedData.count())
    lastTime = recordTime(lastTime, "train data")

    // 格式为student_id,sParams,topic_id
    val sData = data.sparkSession.createDataFrame(learnedData.flatMap { case (topicId, sParamMap, _) =>
      sParamMap.map { case (sId, sParam) => Row.fromSeq(sId +: sParam :+ topicId) }
    }.cache(), OutputHandler.getStudentOutputDataSchema)
    // 格式为question_id,qParams,topic_id
    val qData = data.sparkSession.createDataFrame(learnedData.flatMap { case (topicId, _, qParamMap) =>
      qParamMap.map { case (qId, qParam) => Row.fromSeq(qId +: qParam :+ topicId) }
    }.cache(), OutputHandler.getQuestionOutputDataSchema)

    // 输出学习后的参数到文件中
    OutputHandler.outputLearnedDataToFile(sData)
    OutputHandler.outputLearnedDataToFile(qData)
    lastTime = recordTime(lastTime, "output trained data to file")

    (sData, qData)
  }

  private def train(data: DataFrame, superParam: (Array[Double], Array[Double])) {
    // train
    val (sData, qData) = learn(data, superParam)

    // 预测
    val predictData = AUCValidation.predict(data, TaskBuilder.getStudentModel, sData, qData)
    // 输出预测数据到文件中
    OutputHandler.outputPredictionDataToFile(predictData)
    lastTime = recordTime(lastTime, "output data's prediction to file")

    // 计算训练auc
    val auc = AUCValidation.auc(predictData.select("prediction", "response")
      .rdd.map(row => (row.getDouble(0), row.getInt(1).toDouble)))
    printInfo("auc:" + auc)
    lastTime = recordTime(lastTime, "calc AUC")

    // 输出学习后的参数到mysql中
    OutputHandler.outputLearnedDataToMysql(sData)
    OutputHandler.outputLearnedDataToMysql(qData)
    lastTime = recordTime(lastTime, "output trained data to mysql")

    // 输出预测数据到mysql中
    OutputHandler.outputPredictionDataToMysql(predictData)
    lastTime = recordTime(lastTime, "output data's prediction to mysql")
  }

  private def cvTrain(data: DataFrame, superParams: Array[(Array[Double], Array[Double])]) = {
    superParams.map { superParam =>
      val (trainData, testData) = DataHandler.baggingData(data)
      // train
      val (sData, qData) = learn(trainData, superParam)

      // 计算训练auc
      val trainAuc = AUCValidation.calcAUC(trainData, TaskBuilder.getStudentModel, sData, qData)
      printInfo("train auc:" + trainAuc)
      lastTime = recordTime(lastTime, "calc train AUC")

      // 计算测试auc
      val testAuc = AUCValidation.calcAUC(testData, TaskBuilder.getStudentModel, sData, qData)
      printInfo("test auc:" + testAuc)
      lastTime = recordTime(lastTime, "calc test AUC")

      (trainAuc, testAuc)
    }
  }

  def main(args: Array[String]) {
    // 初始化
    val sparkSession = TaskBuilder.initContext(args)
    // student_id,question_id,response,topic_id
    val data = TaskBuilder.loadData(sparkSession, args)
    lastTime = System.currentTimeMillis()

    val srcSuperParams = Array((Array(2.0), Array(2.0)), (Array(2.3), Array(2.3)), (Array(2.7), Array(2.7)),
      (Array(3.0), Array(3.0)), (Array(3.5), Array(3.5)), (Array(4.0), Array(4.0)))
    val superParams = srcSuperParams
    val cvs = cvTrain(data, superParams)
    for (i <- superParams.indices) {
      val superParam = superParams(i)
      superParam._1.foreach(superParamEle => print(superParamEle + ","))
      print("\t")
      superParam._2.foreach(superParamEle => print(superParamEle + ","))
      println()

      println("(" + cvs(i)._1 + "," + cvs(i)._2 + ")")
      println("-----------------------------------------------")
    }

    //    train(data, null)
  }
}
