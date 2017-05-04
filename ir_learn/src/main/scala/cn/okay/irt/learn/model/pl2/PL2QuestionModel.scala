package cn.okay.irt.learn.model.pl2

/**
  * Created by zhangyikuo on 2017/5/3.
  */
class PL2QuestionModel extends PL2Model {
  private val minA: Double = 0.0
  private val maxA: Double = 2.5
  private val D: Double = 1.704

  /**
    * 模型的link函数
    *
    * @param x     数据
    * @param param 参数
    * @return link函数值
    */
  override protected def link(x: Array[Double], param: Array[Double]): Double = param.head * (x.head - param.last)

  /**
    * 初始化参数
    *
    * @param dataMap 数据三元组，比如key为学生id，value的元素为(题目id,该学生是否做对此题)
    */
  override def initParams(dataMap: Map[Long, Array[(Long, Double)]]) {
    paramMap = dataMap.map { case (dataId, responses) =>
      (dataId, Array(D, responses.count(1.0 == _._2).toDouble / responses.length.toDouble))
    }
  }

  /**
    * 获取参数的类别，主要用于输出
    *
    * @return 参数的类别列表，其元素为(paramName,paramType)
    */
  override def getParamMeta: Array[(String, String)] = Array(("slope", "double"), ("difficulty", "double"))

  /**
    * model的gradient函数
    *
    * @param data   数据，包含x和y
    * @param params 参数
    * @return 根据params计算的gradient向量
    */
  override def gradientProvider(data: Array[Array[Double]], params: Array[Double]): Array[Double] = {
    val grad = data.map { row =>
      val err = row.last - predict(row, params)
      (err * (row.head - params.last), -err * params.head)
    }.reduce((sum1, sum2) => (sum1._1 + sum2._1, sum1._2 + sum2._2))

    Array(grad._1, grad._2)
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
    val hessian = data.map { row =>
      val pVal = predict(row, params)
      val pGrad = pVal * (1 - pVal)
      val linkVal = row.head - params.last
      val a = params.head

      (-linkVal * linkVal * pGrad, pVal - row.last + a * linkVal * pGrad, -a * a * pGrad)
    }.reduce((sum1, sum2) => (sum1._1 + sum2._1, sum1._2 + sum2._2, sum1._3 + sum2._3))

    Array(Array(hessian._1, hessian._2), Array(hessian._2, hessian._3))
  }

  /**
    * 解修正调节器，主要为了使解符合问题的限定条件
    *
    * @param params 暂时求得的参数
    * @return 调节修正后的参数
    */
  override def solutionAdjuster(params: Array[Double]): Array[Double] = {
    val a = params.head
    val b = params.last
    Array(if (a <= minA) minA else if (a >= maxA) maxA else a, if (b >= 1.0) 1.0 else if (b <= 0.0) 0.0 else b)
  }
}
