package cn.okay.irt.learn

import org.apache.spark.sql.{DataFrame, Row}

import scala.collection.mutable
import scala.util.Random

/**
  * Created by zhangyikuo on 2017/5/2.
  */
object DataHandler {
  /**
    * bagging数据
    *
    * @param data    原始数据，格式为student_id,question_id,response,topic_id
    * @param abRatio 分成的两份数据的比例
    * @return (aData,bData)
    */
  def baggingData(data: DataFrame, abRatio: Double = 1.0): (DataFrame, DataFrame) = {
    val splitedData = data.rdd.groupBy(_.get(3).toString.toLong).map { case (_, rowIt) =>
      val rows = rowIt.toArray

      val sIDs = mutable.Set.empty[Long]
      val qIDs = mutable.Set.empty[Long]
      val rowSeq = rows.map(row => (row(0).toString.toLong, row(1).toString.toLong))
      val aIndices1 = for (i <- rowSeq.indices; if !sIDs.contains(rowSeq(i)._1) || !qIDs.contains(rowSeq(i)._2)) yield {
        sIDs += rowSeq(i)._1
        qIDs += rowSeq(i)._2
        i
      }

      val (aIndices2, bIndices) = Random.shuffle(rows.indices.diff(aIndices1))
        .splitAt((rows.length * abRatio / (abRatio + 1)).toInt - aIndices1.length)
      val aIndices = aIndices1 ++ aIndices2
      val aRows = new Array[Row](aIndices.length)
      val bRows = new Array[Row](bIndices.length)
      for (i <- aRows.indices) aRows(i) = rows(aIndices(i))
      for (i <- bRows.indices) bRows(i) = rows(bIndices(i))

      (aRows, bRows)
    }

    (data.sparkSession.createDataFrame(splitedData.flatMap(_._1), data.schema).cache(),
      data.sparkSession.createDataFrame(splitedData.flatMap(_._2), data.schema).cache())
  }
}
