package cn.okay.irt

/**
  * Created by zhangyikuo on 2017/4/21.
  */
package object util {

  implicit class ArrayCalculator(val x: Array[Double]) {
    def +(y: Array[Double]): Array[Double] = MathUtil.algebraCombine(x, 1.0, y, 1.0)

    def -(y: Array[Double]): Array[Double] = MathUtil.algebraCombine(x, 1.0, y, -1.0)

    def *(y: Array[Double]): Double = MathUtil.dot(x, y)

    def *(X: Array[Array[Double]]): Array[Double] = MathUtil.multiply(X, x)

    def /(X: Array[Array[Double]]): Array[Double] = MathUtil.divide(x, X)
  }

}
