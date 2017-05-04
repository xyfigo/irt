package cn.okay.irt.learn

import cn.okay.irt.util._

/**
  * Created by zhangyikuo on 2017/4/20.
  * 该算子解决的是最大化的问题，如果想最小化目标函数，可以对目标函数进行正负转换
  *
  * @param targetFunction   目标函数，输入为(data,params)，输出为函数值，data包含x和y
  * @param gradientProvider 梯度函数，输入为(data,params)，输出为梯度向量，data包含x和y
  * @param hessianProvider  hessian函数，输入为(data,params,gradient)，输出为hessian matrix，data包含x和y
  * @param solutionAdjuster 解修正调节器，输入为params，输出为调节修正后的params，主要为了使解符合问题的限定条件
  */
class NewtonResolver(private val targetFunction: (Array[Array[Double]], Array[Double]) => Double,
                     private val gradientProvider: (Array[Array[Double]], Array[Double]) => Array[Double],
                     private val hessianProvider: (Array[Array[Double]], Array[Double], Array[Double]) => Array[Array[Double]],
                     private val solutionAdjuster: (Array[Double] => Array[Double]) = null) {
  // 收敛判断通用算子
  private val convergeEstimator: ConvergeEstimator = new ConvergeEstimator
  // 最大迭代次数
  private var maxIterations = 1000

  def setMaxIterations(maxIterations: Int): this.type = {
    this.maxIterations = maxIterations
    this
  }

  /**
    * 设置稳定判断阈值，如果两次target值小于该阈值就认为暂时稳定的
    *
    * @param tolerance target差别阈值
    * @return 本resolver
    */
  def setTolerance(tolerance: Double): this.type = {
    convergeEstimator.tolerance = tolerance
    this
  }

  /**
    * 设置稳态次数阈值，如果系统稳定判断的次数大于该阈值就认为是收敛了
    *
    * @param steadyTimeThreshold 稳态次数阈值
    * @return 本resolver
    */
  def setSteadyTimeThreshold(steadyTimeThreshold: Int): this.type = {
    convergeEstimator.steadyTimeThreshold = steadyTimeThreshold
    this
  }

  /**
    * 设置系统震荡次数阈值，如果系统波动的次数大于该阈值就认为系统正处于震荡状态
    *
    * @param fluctuationTimeThreshold 系统震荡次数阈值
    * @return 本resolver
    */
  def setFluctuationTimeThreshold(fluctuationTimeThreshold: Int): this.type = {
    convergeEstimator.fluctuationTimeThreshold = fluctuationTimeThreshold
    this
  }

  /**
    * 设置系统波动次数衰减系数，如果系统暂时处于稳定状态，对过去波动次数的ease系数
    *
    * @param fluctuationTimeDiveRatio 系统波动次数衰减系数
    * @return
    */
  def setFluctuationTimeDiveRatio(fluctuationTimeDiveRatio: Double): this.type = {
    convergeEstimator.fluctuationTimeDiveRatio = fluctuationTimeDiveRatio
    this
  }

  /**
    * 求解
    *
    * @param data   所有数据，目前不支持分布式和并发，并发和分布式由外部过程提供
    * @param params 要求解的参数的初始值
    * @return 求解后的参数
    */
  def resolve(data: Array[Array[Double]], params: Array[Double]): Array[Double] = {
    convergeEstimator.reset()

    var t = 0
    var learningState = (false, false)
    var curParams = params
    while (!learningState._1 && t <= maxIterations) {
      // 如果系统处于震荡状态就切换到GD方法
      var nextParams = if (learningState._2) gdIteration(data, curParams, 1 / (t + maxIterations))
      else newtonIteration(data, curParams)
      // 由于计算的数值稳定性问题，有可能导致hessian矩阵非负定
      if (null == nextParams) nextParams = gdIteration(data, curParams, 1 / (t + maxIterations))
      nextParams = solutionAdjuster(nextParams)

      learningState = convergeEstimator.converged(targetFunction(data, nextParams))
      curParams = nextParams
      t += 1
    }

    curParams
  }

  /**
    * Newton法单步迭代
    *
    * @param data   数据matrix
    * @param params 上次学习到的参数
    * @return 本次更新的参数
    */
  def newtonIteration(data: Array[Array[Double]], params: Array[Double]): Array[Double] = {
    val grad = gradientProvider(data, params)
    val hessian = hessianProvider(data, params, grad)

    if (MathUtil.isNegative(hessian)) {
      val steps = grad / hessian
      // 由于矩阵除法实现算法的精度问题有可能认为负定的矩阵是奇异的
      if (null != steps) params - steps else null
    } else null
  }

  /**
    * GD法单步迭代
    *
    * @param data   数据matrix
    * @param params 上次学习到的参数
    * @param eta    学习率
    * @return 本次更新的参数
    */
  def gdIteration(data: Array[Array[Double]], params: Array[Double], eta: Double): Array[Double] = {
    val grad = gradientProvider(data, params)
    MathUtil.algebraCombine(params, 1.0, grad, eta)
  }
}
