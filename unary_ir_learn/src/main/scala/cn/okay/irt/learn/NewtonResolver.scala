package cn.okay.irt.learn

/**
  * Created by zhangyikuo on 2017/5/5.
  * 该算子解决的是最大化的问题，如果想最小化目标函数，可以对目标函数进行正负转换
  *
  * @param targetFunction   目标函数，输入为(data,param)，输出为函数值，data包含x和y
  * @param gradientProvider 梯度函数，输入为(data,param)，输出为梯度值，data包含x和y
  * @param hessianProvider  hessian函数，输入为(data,param,gradient)，输出为hessian值，data包含x和y
  * @param solutionAdjuster 解修正调节器，输入为param，输出为调节修正后的param，主要为了使解符合问题的限定条件
  */
class NewtonResolver(private val targetFunction: (Array[Array[Double]], Double) => Double,
                     private val gradientProvider: (Array[Array[Double]], Double) => Double,
                     private val hessianProvider: (Array[Array[Double]], Double, Double) => Double,
                     private val solutionAdjuster: (Double => Double) = null) {
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
    * @param data  所有数据，目前不支持分布式和并发，并发和分布式由外部过程提供
    * @param param 要求解的参数的初始值
    * @return 求解后的参数
    */
  def resolve(data: Array[Array[Double]], param: Double): Double = {
    convergeEstimator.reset()

    var t = 0
    var learningState = (false, false)
    var curParam = param
    while (!learningState._1 && t <= maxIterations) {
      // 如果系统处于震荡状态就切换到GD方法
      var nextParam = if (learningState._2) gdIteration(data, curParam, 1 / (t + maxIterations))
      else newtonIteration(data, curParam)

      if (null != solutionAdjuster) nextParam = solutionAdjuster(nextParam)

      learningState = convergeEstimator.converged(targetFunction(data, nextParam))
      curParam = nextParam
      t += 1
    }

    curParam
  }

  /**
    * Newton法单步迭代
    *
    * @param data  数据matrix
    * @param param 上次学习到的参数
    * @return 本次更新的参数
    */
  def newtonIteration(data: Array[Array[Double]], param: Double): Double = {
    val grad = gradientProvider(data, param)
    val hessian = hessianProvider(data, param, grad)

    if (0.0 == hessian) param else param - grad / hessian
  }

  /**
    * GD法单步迭代
    *
    * @param data  数据matrix
    * @param param 上次学习到的参数
    * @param eta   学习率
    * @return 本次更新的参数
    */
  def gdIteration(data: Array[Array[Double]], param: Double, eta: Double): Double = {
    val grad = gradientProvider(data, param)
    param + eta * grad
  }
}
