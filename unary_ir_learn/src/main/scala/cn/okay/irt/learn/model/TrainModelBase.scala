package cn.okay.irt.learn.model

/**
  * Created by zhangyikuo on 2017/5/5.
  */
trait TrainModelBase {
  /**
    * model的目标函数
    *
    * @param data  数据，包含x和y
    * @param param 参数
    * @return 目标值
    */
  def targetFunction(data: Array[Array[Double]], param: Double): Double

  /**
    * model的gradient函数
    *
    * @param data  数据，包含x和y
    * @param param 参数
    * @return 根据param计算的gradient值
    */
  def gradientProvider(data: Array[Array[Double]], param: Double): Double

  /**
    * model的hessian函数
    *
    * @param data     数据，包含x和y
    * @param param    参数
    * @param gradient gradient，可能加速计算，如无必要可不提供
    * @return 根据param和gradient计算的hessian值
    */
  def hessianProvider(data: Array[Array[Double]], param: Double, gradient: Double): Double

  /**
    * 解修正调节器，主要为了使解符合问题的限定条件
    *
    * @param param 暂时求得的参数
    * @return 调节修正后的参数
    */
  def solutionAdjuster(param: Double): Double
}
