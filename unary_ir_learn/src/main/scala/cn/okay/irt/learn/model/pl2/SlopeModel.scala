package cn.okay.irt.learn.model.pl2

import cn.okay.irt.learn.model.IRTModelBase

/**
  * Created by zhangyikuo on 2017/5/5.
  */
class SlopeModel extends IRTModelBase {
  // 参数logistic模型的D
  private val D: Double = 1.704
  private val minA: Double = 2.3
  private val maxA: Double = 2.7

  /**
    * 模型的link函数
    *
    * @param rec   数据
    * @param param 参数
    * @return link函数值
    */
  override def link(rec: Array[Double], param: Double): Double = param * (rec(0) - rec(1))


  /**
    * 初始化参数
    *
    * @param dataMap    数据三元组，比如key为学生id，value的元素为(题目id,该学生是否做对此题)
    * @param paramMap   要初始化的参数map
    * @param paramIndex 该子模型位于compound model的位置索引
    */
  override def initParams(dataMap: Map[Long, Array[(Long, Double)]], paramMap: Map[Long, Array[Double]], paramIndex: Int) {
    dataMap.foreach(tuple => paramMap(tuple._1)(paramIndex) = D)
  }

  /**
    * 获取参数的类别，主要用于输出
    *
    * @return 参数的类别描述，其元素为(paramName,paramType)
    */
  override def getParamMeta: (String, String) = ("slope", "double")

  /**
    * model的gradient函数
    *
    * @param data  数据，包含x和y
    * @param param 参数
    * @return 根据param计算的gradient值
    */
  override def gradientProvider(data: Array[Array[Double]], param: Double): Double = {
    data.map(row => (row.last - predict(row, param)) * (row(0) - row(1))).sum
  }

  /**
    * model的hessian函数
    *
    * @param data     数据，包含x和y
    * @param param    参数
    * @param gradient gradient，可能加速计算，如无必要可不提供
    * @return 根据param和gradient计算的hessian值
    */
  override def hessianProvider(data: Array[Array[Double]], param: Double, gradient: Double): Double = {
    data.map { row =>
      val pVal = predict(row, param)
      val linkVal = row(0) - row(1)
      -linkVal * linkVal * pVal * (1 - pVal)
    }.sum
  }

  /**
    * 解修正调节器，主要为了使解符合问题的限定条件
    *
    * @param param 暂时求得的参数
    * @return 调节修正后的参数
    */
  override def solutionAdjuster(param: Double): Double = if (param >= maxA) maxA else if (param <= minA) minA else param
}
