package cn.okay.irt.learn.model.pl2

import cn.okay.irt.learn.model.IRTModelBase

/**
  * Created by zhangyikuo on 2017/5/3.
  */
abstract class PL2Model extends IRTModelBase {
  protected var paramMap: Map[Long, Array[Double]] = _

  /**
    * 模型的link函数
    *
    * @param x     数据
    * @param param 参数
    * @return link函数值
    */
  protected def link(x: Array[Double], param: Array[Double]): Double

  /**
    * 获取所有初始化参数
    *
    * @return 所有初始化参数，key为要评估dataId，value为dataId对应实体的初始参数
    */
  override def getStartParams: Map[Long, Array[Double]] = paramMap

  /**
    * 预测一行数据的原始目标函数值（targetFunction一般可能取这个函数值的log值）
    *
    * @param rec    记录，可以不包含label值，也可以包含
    * @param params 参数
    * @return 原始目标函数值
    */
  override def predict(rec: Array[Double], params: Array[Double]): Double = 1 / (1 + math.exp(-link(rec, params)))

  /**
    * model的目标函数
    *
    * @param data   数据，包含x和y
    * @param params 参数
    * @return 目标值
    */
  override def targetFunction(data: Array[Array[Double]], params: Array[Double]): Double = {
    data.map { row =>
      if (1.0 == row.last) log(predict(row, params))
      else log(1 - predict(row, params))
    }.sum
  }
}
