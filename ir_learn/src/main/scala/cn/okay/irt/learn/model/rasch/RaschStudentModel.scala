package cn.okay.irt.learn.model.rasch

/**
  * Created by zhangyikuo on 2017/4/21.
  */
class RaschStudentModel extends RaschModel {
  this.studentModelFlag = true

  /**
    * 获取参数的类别，主要用于输出
    *
    * @return 参数的类别列表，其元素为(paramName,paramType)
    */
  override def getParamMeta: Array[(String, String)] = Array(("master", "double"))
}
