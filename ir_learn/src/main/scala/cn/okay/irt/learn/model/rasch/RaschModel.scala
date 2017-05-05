package cn.okay.irt.learn.model.rasch

import cn.okay.irt.learn.model.IRTModelBase

/**
  * Created by zhangyikuo on 2017/4/21.
  */
abstract class RaschModel extends IRTModelBase {
  protected var studentModelFlag: Boolean = true

  // 1参数logistic模型的D，Rasch模型D取1即可
  private var D: Double = 1.704

  private var paramMap: Map[Long, Array[Double]] = _

  private def link(x: Double, param: Double): Double = {
    D * (if (studentModelFlag) param - x
    else x - param)
  }

  private def p(x: Double, param: Double): Double = 1 / (1 + math.exp(-link(x, param)))

  /**
    * 初始化参数
    *
    * @param dataMap 数据三元组，比如key为学生id，value的元素为(题目id,该学生是否做对此题)
    */
  override def initParams(dataMap: Map[Long, Array[(Long, Double)]]) {
    paramMap = dataMap.map { case (dataId, responses) =>
      (dataId, Array(responses.count(1.0 == _._2).toDouble / responses.length.toDouble))
    }
  }

  /**
    * 初始化模型超参数
    *
    * @param superParams 模型超参数
    */
  override def initSuperParams(superParams: Array[Double]) {
    this.D = superParams.head
  }

  /**
    * 获取所有初始化参数
    *
    * @return 所有初始化参数，key为要评估dataId，value为dataId对应实体的初始参数
    */
  override def getStartParams: Map[Long, Array[Double]] = paramMap

  /**
    * 预测一行数据的原始目标函数值（targetFunction一般可能取这个函数值的log值）
    *
    * @param rec    记录，可以不包含label值，也可以包含
    * @param params 参数
    * @return 原始目标函数值
    */
  override def predict(rec: Array[Double], params: Array[Double]): Double = p(rec.head, params.head)

  /**
    * model的目标函数
    *
    * @param data   数据，包含x和y
    * @param params 参数
    * @return 目标值
    */
  override def targetFunction(data: Array[Array[Double]], params: Array[Double]): Double = {
    val param = params.head

    data.map { row =>
      if (1.0 == row(1)) log(p(row(0), param))
      else log(1 - p(row(0), param))
    }.sum
  }

  /**
    * model的gradient函数
    *
    * @param data   数据，包含x和y
    * @param params 参数
    * @return 根据params计算的gradient向量
    */
  override def gradientProvider(data: Array[Array[Double]], params: Array[Double]): Array[Double] = {
    val param = params.head

    val grad = data.map(row => D * (row(1) - p(row(0), param))).sum
    Array(if (studentModelFlag) grad else -grad)
  }

  /**
    * model的hessian函数
    *
    * @param data     数据，包含x和y
    * @param params   参数
    * @param gradient gradient向量，可能加速计算，如无必要可不提供
    * @return 根据params和gradient计算的hessian矩阵
    */
  override def hessianProvider(data: Array[Array[Double]], params: Array[Double], gradient: Array[Double]): Array[Array[Double]] = {
    val param = params.head

    val hessian = data.map { row =>
      val pValue = p(row(0), param)
      D * D * pValue * (pValue - 1)
    }.sum

    Array(Array(hessian))
  }

  /**
    * 解修正调节器，主要为了使解符合问题的限定条件
    *
    * @param params 暂时求得的参数
    * @return 调节修正后的参数
    */
  override def solutionAdjuster(params: Array[Double]): Array[Double] = {
    params.map(param => if (param >= 1.0) 1.0 else if (param <= 0.0) 0.0 else param)
  }
}
