package cn.okay.irt.learn.model

/**
  * Created by zhangyikuo on 2017/4/28.
  */
trait PredictModelBase extends Serializable {
  /**
    * 预测一行数据的原始目标函数值（targetFunction一般可能取这个函数值的log值）
    *
    * @param rec    记录，可以不包含label值，也可以包含
    * @param params 参数
    * @return 原始目标函数值
    */
  def predict(rec: Array[Double], params: Array[Double]): Double
}
