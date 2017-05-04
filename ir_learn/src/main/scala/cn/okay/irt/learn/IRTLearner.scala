package cn.okay.irt.learn

import java.util.concurrent.{Callable, FutureTask, RejectedExecutionException}

import cn.okay.irt.context.{LocalRunner, TaskBuilder}
import cn.okay.irt.learn.model.IRTModelBase
import org.slf4j.LoggerFactory

/**
  * Created by zhangyikuo on 2017/4/24.
  */
class IRTLearner {
  private val log = LoggerFactory.getLogger(this.getClass)

  // 随机逼近步幅，用于将本次计算参数与上次计算参数进行融合
  private var eta: Double = 0.5

  // 学生模型
  private var sModel: IRTModelBase = _
  // 问题模型
  private var qModel: IRTModelBase = _

  // 学生参数map，key为学生id，value为学生参数
  var sParamMap: Map[Long, Array[Double]] = _
  // 题目参数map，key为题目id，value为题目参数
  var qParamMap: Map[Long, Array[Double]] = _

  // key为学生id，value的元素为(题目id,该学生是否做对此题)
  private var sDataMap: Map[Long, Array[(Long, Double)]] = _
  // key为题目id，value的元素为(学生id,该学生是否做对此题)
  private var qDataMap: Map[Long, Array[(Long, Double)]] = _

  def setEta(eta: Double): this.type = {
    this.eta = eta
    this
  }

  def setStudentModel(studentModel: IRTModelBase): this.type = {
    this.sModel = studentModel
    sParamMap = studentModel.getStartParams
    this
  }

  def setQuestionModel(questionModel: IRTModelBase): this.type = {
    this.qModel = questionModel
    qParamMap = questionModel.getStartParams
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
    val sTarget = sDataMap.map { case (sId, responses) =>
      val data = responses.map { case (qId, response) => qParamMap(qId) :+ response }
      sModel.targetFunction(data, sParamMap(sId))
    }.sum

    val qTarget = qDataMap.map { case (qId, responses) =>
      val data = responses.map { case (sId, response) => sParamMap(sId) :+ response }
      qModel.targetFunction(data, qParamMap(qId))
    }.sum

    sTarget + qTarget
  }

  def learn() {
    val convergeEstimator = new ConvergeEstimator

    do {
      // 交叉进行学习
      val curSParamMap = sLearnIteration
      val curQParamMap = qLearnIteration

      sParamMap = curSParamMap
      qParamMap = curQParamMap
    } while (convergeEstimator.converged(calcTarget)._1)
  }

  private def sLearnIteration: Map[Long, Array[Double]] = iteration(sModel, sDataMap, qParamMap, sParamMap)

  private def qLearnIteration: Map[Long, Array[Double]] = iteration(qModel, qDataMap, sParamMap, qParamMap)

  private def execute(learnTask: FutureTask[Array[Double]]) {
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

  /**
    * 单步迭代
    *
    * @param model          模型
    * @param dataMap        三元组数据map，比如sDataMap
    * @param lookupParamMap 同dataMap一起组合成训练数据
    * @param lastParamMap   上次迭代后所得参数map
    * @return 本次迭代后所得参数map
    */
  private def iteration(model: IRTModelBase, dataMap: Map[Long, Array[(Long, Double)]], lookupParamMap: Map[Long, Array[Double]],
                        lastParamMap: Map[Long, Array[Double]]): Map[Long, Array[Double]] = {
    // M步
    val learnTasks = dataMap.map { case (dataId, responses) =>
      val data = responses.map { case (recId, response) => lookupParamMap(recId) :+ response }
      val learnTask = new FutureTask[Array[Double]](new Callable[Array[Double]] {
        override def call: Array[Double] = {
          try
            new NewtonResolver(model.targetFunction, model.gradientProvider, model.hessianProvider, model.solutionAdjuster)
              .resolve(data, model.getStartParams(dataId))
          catch {
            case e: Exception =>
              log.warn("【" + dataId + "】 M step learn error", e)
              lastParamMap(dataId)
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
          lastParamMap(dataId)
      }

      (dataId, curParam)
    }

    // E步
    lastParamMap.map { case (dataId, lastParam) =>
      val param = curParamMap.get(dataId) match {
        case None => lastParam
        case Some(curParam) => (for (i <- lastParam.indices) yield (1 - eta) * lastParam(i) + eta * curParam(i)).toArray
      }

      (dataId, param)
    }
  }
}
