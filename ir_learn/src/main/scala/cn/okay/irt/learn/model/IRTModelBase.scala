package cn.okay.irt.learn.model

/**
  * Created by zhangyikuo on 2017/4/25.
  */
trait IRTModelBase extends TrainModelBase with PredictModelBase {
  protected val zeroDomain: Double = 1E-20
  protected val zeroLogValue: Double = math.log(zeroDomain)

  protected def log(x: Double): Double = {
    val res = math.log(x)
    if (res <= zeroDomain) zeroLogValue else res
  }

  /**
    * 获取初始化参数
    *
    * @return 初始化参数
    */
  override def getStartParam: Array[Double] = throw new RuntimeException("no need")

  /**
    * 初始化参数
    *
    * @param dataMap 数据三元组，比如key为学生id，value的元素为(题目id,该学生是否做对此题)
    */
  def initParams(dataMap: Map[Long, Array[(Long, Double)]])

  /**
    * 获取所有初始化参数
    *
    * @return 所有初始化参数，key为要评估dataId，value为dataId对应实体的初始参数
    */
  def getStartParams: Map[Long, Array[Double]]

  /**
    * 获取参数的类别，主要用于输出
    *
    * @return 参数的类别列表，其元素为(paramName,paramType)
    */
  def getParamMeta: Array[(String, String)]
}
