package cn.okay.irt.learn

import cn.okay.irt.learn.model.PredictModelBase
import org.apache.spark.mllib.evaluation.BinaryClassificationMetrics
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{DataFrame, Row}

/**
  * Created by zhangyikuo on 2017/4/27.
  */
object AUCValidation {
  /**
    * 预测得分
    *
    * @param data    原始数据，格式为student_id,question_id,response,topic_id，也可以不包含response
    * @param sModel  IRT学生预测模型
    * @param sParams 学生参数，格式为student_id,sParams,topic_id，注意参数的顺序要和model保持一致
    * @param qParams 题目参数，格式为question_id,qParams,topic_id，注意参数的顺序要和model保持一致
    * @return 原始数据尾部添加一列prediction
    */
  def predict(data: DataFrame, sModel: PredictModelBase, sParams: DataFrame, qParams: DataFrame): DataFrame = {
    val sMixData = data.joinWith(sParams, data("student_id") === sParams("student_id") && data("topic_id") === sParams("topic_id"), "left_outer")
    val predictData = sMixData.joinWith(qParams, sMixData("_1.question_id") === qParams("question_id") && sMixData("_1.topic_id") === qParams("topic_id"))
      .rdd.map { case ((rec, sParamRec), qParamRec) =>
      val sParam = (for (i <- 1 until sParamRec.length - 1) yield sParamRec(i).toString.toDouble).toArray
      val qParam = (for (i <- 1 until qParamRec.length - 1) yield qParamRec(i).toString.toDouble).toArray
      val prediction = sModel.predict(qParam, sParam)

      Row.fromSeq(rec.toSeq :+ prediction)
    }

    data.sparkSession.createDataFrame(predictData, data.schema.add("prediction", "double", nullable = false))
  }

  /**
    * 计算得分，用于计算auc，数据需要包含response值，即label值
    *
    * @param data    原始数据，格式为student_id,question_id,response,topic_id
    * @param sModel  IRT学生预测模型
    * @param sParams 学生参数，格式为student_id,sParams,topic_id，注意参数的顺序要和model保持一致
    * @param qParams 题目参数，格式为question_id,qParams,topic_id，注意参数的顺序要和model保持一致
    * @return 元素格式为(prediction,response)
    */
  def predictWithLabel(data: DataFrame, sModel: PredictModelBase, sParams: DataFrame, qParams: DataFrame): RDD[(Double, Double)] = {
    predict(data, sModel, sParams, qParams).select("prediction", "response").rdd
      .map(row => (row.getDouble(0), row.getInt(1).toDouble))
  }

  /**
    * 计算得分，数据不需要包含response值，即label值
    *
    * @param data    原始数据，格式为student_id,question_id,topic_id
    * @param sModel  IRT学生预测模型
    * @param sParams 学生参数，格式为student_id,sParams,topic_id，注意参数的顺序要和model保持一致
    * @param qParams 题目参数，格式为question_id,qParams,topic_id，注意参数的顺序要和model保持一致
    * @return 元素格式为prediction
    */
  def predictWithoutLabel(data: DataFrame, sModel: PredictModelBase, sParams: DataFrame, qParams: DataFrame): RDD[Double] = {
    predict(data, sModel, sParams, qParams).select("prediction").rdd.map(_.getDouble(0))
  }

  /**
    * 计算AUC
    *
    * @param data    原始数据，格式为student_id,question_id,response,topic_id
    * @param v       IRT学生预测模型
    * @param sParams 学生参数，格式为student_id,sParams,topic_id，注意参数的顺序要和model保持一致
    * @param qParams 题目参数，格式为question_id,qParams,topic_id，注意参数的顺序要和model保持一致
    * @return auc
    */
  def calcAUC(data: DataFrame, sModel: PredictModelBase, sParams: DataFrame, qParams: DataFrame): Double = auc(predictWithLabel(data, sModel, sParams, qParams))

  /**
    * 计算AUC
    *
    * @param scoreAndLabels 得分和label值
    * @return auc
    */
  def auc(scoreAndLabels: RDD[(Double, Double)]): Double = new BinaryClassificationMetrics(scoreAndLabels).areaUnderROC()
}