package com.myrecsys.ALS

import org.apache.log4j.{Level, Logger}
import org.apache.spark.ml.evaluation.RegressionEvaluator
import org.apache.spark.ml.recommendation.ALS
import org.apache.spark.sql.SparkSession

object ALSRecSys {
  case class Rating(userId: Int, movieId: Int, rating: Double, timestamp: Long)

  def parseRating(str: String): Rating = {
    val fields = str.split("\t")
    Rating(fields(0).toInt, fields(1).toInt, fields(2).toFloat, fields(3).toLong)
  }

  def main(args: Array[String]): Unit = {

    Logger.getLogger("org").setLevel(Level.ERROR)

    val spark = SparkSession
      .builder
      .appName("ALSExample").master("local[*]")
      .config("spark.executor.cores", '4')
      .getOrCreate()


    println("Loading rating...")
    val data = spark.read.textFile("../ml-100k/u.data").rdd
    val ratings = spark.createDataFrame(data.map(parseRating))
    val Array(training, test) = ratings.randomSplit(Array(0.8, 0.2))
    ratings.unpersist(blocking = false)

    val als = new ALS()
      .setMaxIter(10)
      .setRegParam(0.01)
      .setUserCol("userId")
      .setItemCol("movieId")
      .setRatingCol("rating")
    val model = als.fit(training)
    model.setColdStartStrategy("drop")
    val predictions = model.transform(test)


    val evaluator = new RegressionEvaluator()
      .setMetricName("rmse")
      .setLabelCol("rating")
      .setPredictionCol("prediction")
    val rmse = evaluator.evaluate(predictions)
    println(s"Root-mean-square error = $rmse")

    val userRecs = model.recommendForAllUsers(10)
    val users = userRecs.filter(x => x(0) == 1).collect()
    for (user <- users) {
      println(user)
      println(user(0) + " : " + user(1))
    }

    //    val movieRecs = model.recommendForAllItems(10)
  }
}
