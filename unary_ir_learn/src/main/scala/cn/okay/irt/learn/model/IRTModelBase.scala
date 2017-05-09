package cn.okay.irt.learn.model

/**
  * Created by zhangyikuo on 2017/5/5.
  */
trait IRTModelBase extends TrainModelBase with PredictModelBase {
  protected val zeroDomain: Double = 1E-20
  protected val zeroLogValue: Double = math.log(zeroDomain)

  protected def log(x: Double): Double = {
    val res = math.log(x)
    if (res <= zeroDomain) zeroLogValue else res
  }

  /**
    * 预测一行数据的原始目标函数值（targetFunction一般可能取这个函数值的log值）
    *
    * @param rec   记录，可以不包含label值，也可以包含
    * @param param 参数
    * @return 原始目标函数值
    */
  override def predict(rec: Array[Double], param: Double): Double = 1 / (1 + math.exp(-link(rec, param)))

  /**
    * model的目标函数
    *
    * @param data  数据，包含x和y
    * @param param 参数
    * @return 目标值
    */
  override def targetFunction(data: Array[Array[Double]], param: Double): Double = {
    data.map { row =>
      if (1.0 == row.last) log(predict(row, param))
      else log(1 - predict(row, param))
    }.sum
  }

  /**
    * 解修正调节器，主要为了使解符合问题的限定条件
    *
    * @param param 暂时求得的参数
    * @return 调节修正后的参数
    */
  override def solutionAdjuster(param: Double): Double = if (param >= 1.0) 1.0 else if (param <= 0.0) 0.0 else param

  /**
    * 模型的link函数
    *
    * @param rec   数据
    * @param param 参数
    * @return link函数值
    */
  def link(rec: Array[Double], param: Double): Double

  /**
    * 初始化参数
    *
    * @param dataMap    数据三元组，比如key为学生id，value的元素为(题目id,该学生是否做对此题)
    * @param paramMap   要初始化的参数map
    * @param paramIndex 该子模型位于compound model的位置索引
    */
  def initParams(dataMap: Map[Long, Array[(Long, Double)]], paramMap: Map[Long, Array[Double]], paramIndex: Int)

  /**
    * 获取参数的类别，主要用于输出
    *
    * @return 参数的类别描述，其元素为(paramName,paramType)
    */
  def getParamMeta: (String, String)
}
