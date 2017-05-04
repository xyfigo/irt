package cn.okay.irt.context

import cn.okay.irt.learn.model.IRTModelBase
import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.types.{LongType, StructField, StructType}

/**
  * Created by zhangyikuo on 2017/4/26.
  */
object OutputHandler {
  private def appendParamOutputSchema(model: IRTModelBase, schema: StructType): StructType = {
    var newSchema = schema
    for (meta <- model.getParamMeta) newSchema = newSchema.add(meta._1, meta._2, nullable = false)
    newSchema.add(StructField("topic_id", LongType, nullable = false))
  }

  /**
    * 获取学生学习能力表schema
    *
    * @return 学生学习能力表schema
    */
  def getStudentOutputDataSchema: StructType = {
    appendParamOutputSchema(TaskBuilder.getStudentModel, new StructType(Array(StructField("student_id", LongType, nullable = false))))
  }

  /**
    * 获取题目难度表schema
    *
    * @return 题目难度表schema
    */
  def getQuestionOutputDataSchema: StructType = {
    appendParamOutputSchema(TaskBuilder.getQuestionModel, new StructType(Array(StructField("question_id", LongType, nullable = false))))
  }

  /**
    * 输出数据到mysql中
    *
    * @param data    数据
    * @param dbtable 数据库中对应表名
    * @param mode    写入数据的模式(overwrite,append,ignore,error)，默认为overwrite
    */
  def outputToMysql(data: DataFrame, dbtable: String, mode: String = "overwrite") {
    data.write.format("jdbc").mode(mode)
      .option("url", TaskBuilder.getProperty("db.url"))
      .option("dbtable", dbtable)
      .option("user", TaskBuilder.getProperty("db.user"))
      .option("password", TaskBuilder.getProperty("db.password"))
      .option("batchsize", TaskBuilder.getProperty("db.batchsize"))
      .save
  }

  /**
    * 将学习后的数据输出到mysql中
    *
    * @param data 学习后的数据
    * @param mode 写入数据的模式(overwrite,append,ignore,error)，默认为overwrite
    */
  def outputLearnedDataToMysql(data: DataFrame, mode: String = "overwrite") {
    val dbtable = if ("student_id" == data.schema.head.name) TaskBuilder.getProperty("db.dbtable.student")
    else TaskBuilder.getProperty("db.dbtable.question")

    outputToMysql(data, dbtable, mode)
  }

  /**
    * 将学习后的数据输出到文件中
    *
    * @param data 学习后的数据
    * @param mode 写入数据的模式(overwrite,append,ignore,error)，默认为overwrite
    */
  def outputLearnedDataToFile(data: DataFrame, mode: String = "overwrite") {
    val dbtable = if ("student_id" == data.schema.head.name) "student" else "question"
    data.write.mode(mode).csv(TaskBuilder.getProperty("output.local.dir") + dbtable)
  }

  /**
    * 将预测数据输出到mysql中
    *
    * @param data 预测数据
    * @param mode 写入数据的模式(overwrite,append,ignore,error)，默认为overwrite
    */
  def outputPredictionDataToMysql(data: DataFrame, mode: String = "overwrite") {
    outputToMysql(data, TaskBuilder.getProperty("db.dbtable.prediction"), mode)
  }

  /**
    * 将预测数据输出到文件中
    *
    * @param data 预测数据
    * @param mode 写入数据的模式(overwrite,append,ignore,error)，默认为overwrite
    */
  def outputPredictionDataToFile(data: DataFrame, mode: String = "overwrite") {
    data.write.mode(mode).csv(TaskBuilder.getProperty("output.local.dir") + "predict")
  }
}
