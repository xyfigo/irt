package cn.okay.irt.learn.model.pl2

/**
  * Created by zhangyikuo on 2017/5/3.
  */
class PL2StudentModel extends PL2Model {
  /**
    * 模型的link函数
    *
    * @param x     数据
    * @param param 参数
    * @return link函数值
    */
  override protected def link(x: Array[Double], param: Array[Double]): Double = x(0) * (param.head - x(1))

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
    * 获取参数的类别，主要用于输出
    *
    * @return 参数的类别列表，其元素为(paramName,paramType)
    */
  override def getParamMeta: Array[(String, String)] = Array(("master", "double"))

  /**
    * model的gradient函数
    *
    * @param data   数据，包含x和y
    * @param params 参数
    * @return 根据params计算的gradient向量
    */
  override def gradientProvider(data: Array[Array[Double]], params: Array[Double]): Array[Double] = {
    Array(data.map(row => row.head * (row.last - predict(row, params))).sum)
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
    Array(Array(data.map { row =>
      val pVal = predict(row, params)
      val a = row.head
      -a * a * pVal * (1 - pVal)
    }.sum))
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
