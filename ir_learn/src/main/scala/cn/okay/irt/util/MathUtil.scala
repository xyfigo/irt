package cn.okay.irt.util

import org.apache.commons.math3.linear.{EigenDecomposition, LUDecomposition, MatrixUtils, SingularMatrixException}

/**
  * Created by zhangyikuo on 2017/4/20.
  */
object MathUtil {
  /**
    * 判断矩阵是否负定
    *
    * @param X 矩阵
    * @return 是否负定
    */
  def isNegative(X: Array[Array[Double]]): Boolean = {
    if (X.length <= 1) X(0)(0) < 0
    else {
      val (reals, images) = eigenvalue(X)
      reals.forall(_ < 0) && images.forall(_ == 0.0)
    }
  }

  /**
    * 判断矩阵是否正定
    *
    * @param X 矩阵
    * @return 是否正定
    */
  def isPositive(X: Array[Array[Double]]): Boolean = {
    if (X.length <= 1) X(0)(0) > 0
    else {
      val (reals, images) = eigenvalue(X)
      reals.forall(_ < 0) && images.forall(_ == 0.0)
    }
  }

  /**
    * 计算矩阵特征值
    *
    * @param X 矩阵
    * @return 特征值数组，格式为(realParts,imageParts)
    */
  def eigenvalue(X: Array[Array[Double]]): (Array[Double], Array[Double]) = {
    if (X.length <= 1) (Array(X(0)(0)), Array(0.0))
    else {
      val compositor = new EigenDecomposition(MatrixUtils.createRealMatrix(X))
      (compositor.getRealEigenvalues, compositor.getImagEigenvalues)
    }
  }

  /**
    * 求矩阵的逆
    *
    * @param X 矩阵
    * @return 矩阵的逆
    */
  def inverse(X: Array[Array[Double]]): Array[Array[Double]] = {
    if (X.length <= 1) Array(Array(1.0 / X(0)(0)))
    else {
      try
        MatrixUtils.inverse(MatrixUtils.createRealMatrix(X)).getData
      catch {
        case e: SingularMatrixException =>
          null
      }
    }
  }

  /**
    * 计算两个向量点积
    *
    * @param x1 向量1
    * @param x2 向量2
    * @return 点积
    */
  def dot(x1: Array[Double], x2: Array[Double]): Double = {
    var sum = 0.0
    for (i <- x1.indices) sum += x1(i) * x2(i)
    sum
  }

  /**
    * 矩阵左乘向量
    *
    * @param X 左矩阵
    * @param x 右向量
    * @return 相乘结果向量
    */
  def multiply(X: Array[Array[Double]], x: Array[Double]): Array[Double] = X.map(dot(_, x))

  /**
    * 向量除矩阵
    *
    * @param x 向量
    * @param X 矩阵
    * @return 相除结果向量
    */
  def divide(x: Array[Double], X: Array[Array[Double]]): Array[Double] = {
    if (X.length <= 1) x.map(_ / X(0)(0))
    else {
      try
        new LUDecomposition(MatrixUtils.createRealMatrix(X)).getSolver.solve(MatrixUtils.createRealVector(x)).toArray
      catch {
        case e: SingularMatrixException =>
          null
      }
    }
  }

  /**
    * 向量线性组合
    *
    * @param x1 向量1
    * @param a  x1系数
    * @param x2 向量2
    * @param b  x2系数
    * @return 运算结果向量，a*x1+b*x2
    */
  def algebraCombine(x1: Array[Double], a: Double, x2: Array[Double], b: Double): Array[Double] = (for (i <- x1.indices) yield a * x1(i) + b * x2(i)).toArray
}
