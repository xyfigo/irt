package cn.okay.irt.learn

import java.util.concurrent.{Callable, FutureTask, RejectedExecutionException}

import cn.okay.irt.context.{LocalRunner, TaskBuilder}
import cn.okay.irt.learn.model.CompoundModel
import org.slf4j.LoggerFactory

/**
  * Created by zhangyikuo on 2017/5/8.
  */
class IRTLearner {
  private val log = LoggerFactory.getLogger(this.getClass)

  // 随机逼近步幅，用于将本次计算参数与上次计算参数进行融合
  private var eta: Double = 0.5

  // 学生模型
  private var sModel: CompoundModel = _
  // 问题模型
  private var qModel: CompoundModel = _

  // key为学生id，value的元素为(题目id,该学生是否做对此题)
  private var sDataMap: Map[Long, Array[(Long, Double)]] = _
  // key为题目id，value的元素为(学生id,该学生是否做对此题)
  private var qDataMap: Map[Long, Array[(Long, Double)]] = _

  def setEta(eta: Double): this.type = {
    this.eta = eta
    this
  }

  def setStudentModel(studentModel: CompoundModel): this.type = {
    this.sModel = studentModel
    this
  }

  def setQuestionModel(questionModel: CompoundModel): this.type = {
    this.qModel = questionModel
    this
  }

  def setStudentDataMap(sDataMap: Map[Long, Array[(Long, Double)]]): this.type = {
    this.sDataMap = sDataMap
    this
  }

  def setQuestionDataMap(qDataMap: Map[Long, Array[(Long, Double)]]): this.type = {
    this.qDataMap = qDataMap
    this
  }

  def calcTarget: Double = {
    sDataMap.map { case (sId, responses) =>
      val sParams = sModel.paramMap(sId)
      val data = extractTrainData(responses, sParams, qModel.paramMap, studentFlag = true, 0)
      sModel.subModels(0).targetFunction(data, sParams(0))
    }.sum
  }

  def learn() {
    val convergeEstimator = new ConvergeEstimator

    do {
      // 交叉进行学习
      for (i <- sModel.subModels.indices) {
        iteration(sModel, sDataMap, qModel.paramMap, studentFlag = true, i)
      }

      for (i <- qModel.subModels.indices) {
        iteration(qModel, qDataMap, sModel.paramMap, studentFlag = false, i)
      }

      sModel.syncParamMap()
      qModel.syncParamMap()
    } while (convergeEstimator.converged(calcTarget)._1)
  }

  private def execute(learnTask: FutureTask[Double]) {
    var success = true
    do {
      try
        LocalRunner.getExecutor.execute(learnTask)
      catch {
        case e: RejectedExecutionException =>
          log.info("too much tasks", e)
          success = false
          Thread.sleep(TaskBuilder.learnInterval)
      }
    } while (!success)
  }

  private def extractTrainData(responses: Array[(Long, Double)], lastParams: Array[Double], lookupParamMap: Map[Long, Array[Double]],
                               studentFlag: Boolean, subModelIndex: Int) = {
    responses.map { case (recId, response) =>
      val lastParamData = lastParams.take(subModelIndex) ++ lastParams.drop(subModelIndex + 1)
      if (studentFlag) lastParamData ++ lookupParamMap(recId) :+ response
      else lookupParamMap(recId) ++ lastParamData :+ response
    }
  }

  /**
    * 单步单参数迭代
    *
    * @param compoundModel  混合模型
    * @param dataMap        三元组数据map，比如sDataMap
    * @param lookupParamMap 同dataMap一起组合成训练数据
    * @param studentFlag    是否是学生模型
    * @param subModelIndex  本次迭代的单参数在compoundModel中的位置索引
    */
  private def iteration(compoundModel: CompoundModel, dataMap: Map[Long, Array[(Long, Double)]], lookupParamMap: Map[Long, Array[Double]],
                        studentFlag: Boolean, subModelIndex: Int) {
    val lastParamMap = compoundModel.paramMap

    // M步
    val learnTasks = dataMap.map { case (dataId, responses) =>
      val lastParams = lastParamMap(dataId)
      val data = extractTrainData(responses, lastParams, lookupParamMap, studentFlag, subModelIndex)

      val model = compoundModel.subModels(subModelIndex)
      val learnTask = new FutureTask[Double](new Callable[Double] {
        override def call: Double = {
          try
            new NewtonResolver(model.targetFunction, model.gradientProvider, model.hessianProvider, model.solutionAdjuster)
              .resolve(data, compoundModel.startParamMap(dataId)(subModelIndex))
          catch {
            case e: Exception =>
              log.warn("【" + dataId + "】 M step learn error", e)
              lastParams(subModelIndex)
          }
        }
      })

      try
        execute(learnTask)
      catch {
        case e: Exception =>
          log.info("【" + dataId + "】 M step execute error", e)
      }

      (dataId, learnTask)
    }
    val curParamMap = learnTasks.map { case (dataId, learnTask) =>
      val curParam = try learnTask.get()
      catch {
        case e: Exception =>
          log.warn("【" + dataId + "】 M step error", e)
          lastParamMap(dataId)(subModelIndex)
      }

      (dataId, curParam)
    }

    // E步
    compoundModel.paramMapUpdated.foreach { case (dataId, lastParam) =>
      curParamMap.get(dataId) match {
        case Some(curParam) => lastParam(subModelIndex) = (1 - eta) * lastParam(subModelIndex) + eta * curParam
        case None =>
      }
    }
  }
}
