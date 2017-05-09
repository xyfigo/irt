package cn.okay.irt.learn.model.pl2

import cn.okay.irt.learn.model.IRTModelBase

/**
  * Created by zhangyikuo on 2017/5/5.
  */
class DifficultyModel extends IRTModelBase {
  /**
    * 模型的link函数
    *
    * @param rec   数据
    * @param param 参数
    * @return link函数值
    */
  override def link(rec: Array[Double], param: Double): Double = rec(1) * (rec(0) - param)

  /**
    * 初始化参数
    *
    * @param dataMap    数据三元组，比如key为学生id，value的元素为(题目id,该学生是否做对此题)
    * @param paramMap   要初始化的参数map
    * @param paramIndex 该子模型位于compound model的位置索引
    */
  override def initParams(dataMap: Map[Long, Array[(Long, Double)]], paramMap: Map[Long, Array[Double]], paramIndex: Int) {
    dataMap.foreach { case (dataId, responses) =>
      paramMap(dataId)(paramIndex) = responses.count(1.0 == _._2).toDouble / responses.length.toDouble
    }
  }

  /**
    * 获取参数的类别，主要用于输出
    *
    * @return 参数的类别描述，其元素为(paramName,paramType)
    */
  override def getParamMeta: (String, String) = ("difficulty", "double")

  /**
    * model的gradient函数
    *
    * @param data  数据，包含x和y
    * @param param 参数
    * @return 根据param计算的gradient值
    */
  override def gradientProvider(data: Array[Array[Double]], param: Double): Double = {
    data.map(row => row(1) * (predict(row, param) - row.last)).sum
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
      val a = row(1)
      -a * a * pVal * (1 - pVal)
    }.sum
  }
}
