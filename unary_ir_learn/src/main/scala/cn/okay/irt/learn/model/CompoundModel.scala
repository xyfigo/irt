package cn.okay.irt.learn.model

/**
  * Created by zhangyikuo on 2017/5/8.
  */
class CompoundModel extends Serializable {
  var subModels: Array[IRTModelBase] = _

  var startParamMap: Map[Long, Array[Double]] = _
  var paramMap: Map[Long, Array[Double]] = _
  var paramMapUpdated: Map[Long, Array[Double]] = _

  /**
    * 获取参数的类别，主要用于输出
    *
    * @return 参数的类别描述列表，其元素为(paramName,paramType)
    */
  def getParamMetas: Array[(String, String)] = subModels.map(_.getParamMeta)

  /**
    * 初始化参数
    *
    * @param dataMap 数据三元组，比如key为学生id，value的元素为(题目id,该学生是否做对此题)
    */
  def initParams(dataMap: Map[Long, Array[(Long, Double)]]) {
    startParamMap = dataMap.map(tuple => (tuple._1, new Array[Double](subModels.length)))

    for (i <- subModels.indices) {
      subModels(i).initParams(dataMap, startParamMap, i)
    }

    paramMapUpdated = startParamMap.map(tuple => (tuple._1, tuple._2.clone()))
    syncParamMap()
  }

  def syncParamMap() {
    paramMap = paramMapUpdated.map(tuple => (tuple._1, tuple._2.clone()))
  }
}
