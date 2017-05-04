package cn.okay.irt.context

import java.util.Properties

import cn.okay.irt.learn.model.IRTModelBase
import cn.okay.irt.learn.model.rasch.{RaschQuestionModel, RaschStudentModel}
import org.apache.spark.sql.{DataFrame, SparkSession}


/**
  * Created by zhangyikuo on 2017/4/20.
  */
object TaskBuilder {
  // 每个M步的学习间隔，经验估计值
  val learnInterval: Int = 10 * 1000

  private var config: Properties = _

  def initContext(args: Array[String]): SparkSession = {
    SparkSession.builder.master("local[3]").appName("ir_learn")
      .config("spark.worker.timeout", "20").config("spark.executor.memory", "2g")
      .getOrCreate()
  }

  def loadData(sparkSession: SparkSession, args: Array[String]): DataFrame = {
    sparkSession.read.option("header", value = true).option("inferSchema", value = true)
      .csv("E:\\work\\irt\\dev_answer_20170504.csv").cache()
  }

  def getProperty(propName: String): String = {
    if (null == config) {
      config = new Properties
      config.load(ClassLoader.getSystemResourceAsStream("config.properties"))
    }
    config.getProperty(propName)
  }

  def getLearnerThreadPoolSize: Int = 100

  def getStudentModel: IRTModelBase = new RaschStudentModel

  def getQuestionModel: IRTModelBase = new RaschQuestionModel
}
