package cn.okay.irt.learn.model

/**
  * Created by zhangyikuo on 2017/4/21.
  */
trait TrainModelBase {
  /**
    * 获取初始化参数
    *
    * @return 初始化参数
    */
  def getStartParam: Array[Double]

  /**
    * model的目标函数
    *
    * @param data   数据，包含x和y
    * @param params 参数
    * @return 目标值
    */
  def targetFunction(data: Array[Array[Double]], params: Array[Double]): Double

  /**
    * model的gradient函数
    *
    * @param data   数据，包含x和y
    * @param params 参数
    * @return 根据params计算的gradient向量
    */
  def gradientProvider(data: Array[Array[Double]], params: Array[Double]): Array[Double]

  /**
    * model的hessian函数
    *
    * @param data     数据，包含x和y
    * @param params   参数
    * @param gradient gradient向量，可能加速计算，如无必要可不提供
    * @return 根据params和gradient计算的hessian矩阵
    */
  def hessianProvider(data: Array[Array[Double]], params: Array[Double], gradient: Array[Double]): Array[Array[Double]]

  /**
    * 解修正调节器，主要为了使解符合问题的限定条件
    *
    * @param params 暂时求得的参数
    * @return 调节修正后的参数
    */
  def solutionAdjuster(params: Array[Double]): Array[Double]
}
