package com.myrecsys.spark

import com.myrecsys.{MovieLense, Metrics}
import org.apache.log4j.{Level, Logger}
import org.apache.spark.mllib.recommendation.{ALS, Rating}
import org.apache.spark.{SparkConf, SparkContext}

object ALSReSys2 {

  def main(args: Array[String]): Unit = {

    Logger.getLogger("org").setLevel(Level.ERROR)
    val conf = new SparkConf()
      .setMaster("local[*]")
      .setAppName("MovieRecommendationsALS")
      .set("spark.executor.memory", "2g")
    val sc = new SparkContext(conf)

    println("Loading rating...")
    val data = sc.textFile(MovieLense.ratingsPath)
    val splits = data.randomSplit(Array(0.8, 0.2), 0L)
    val train_data= splits(0).cache()
    val test_data = splits(1).cache()
    val numTraining = train_data.count()
    val numTest = test_data.count()
    println(s"Training: $numTraining, test: $numTest.")

    val train_ratings = train_data.map(x => x.split('\t')).map(x => Rating(x(0).toInt, x(1).toInt, x(2).toDouble)).cache()
    val test_orig_ratings = test_data.map(x => x.split('\t')).map(x => Rating(x(0).toInt, x(1).toInt, x(2).toDouble)).cache()
    val test_predict_ratings = test_data.map(x => x.split('\t')).map(x => (x(0).toInt, x(1).toInt)).cache()

    train_ratings.unpersist(blocking = false)

    println("\nTraining model...")

    val rank = 8
    val numIterations = 10

    val train_model = ALS.train(train_ratings, rank, numIterations)
    val test_model = ALS.train(test_orig_ratings, rank, numIterations)

    val predictions = train_model.predict(test_predict_ratings)
    println("(movieId, userId) : ratings ")
    for( predict <- predictions){
      println("("+predict.user+", "+predict.product+") : "+predict.rating)
    }
    val mse = Metrics.rmseRDD(test_model, predictions)
    println("RMSE: "+ mse)

    val recommendedNumber = 10
    TopNRecommender.rddTopNPrint(train_ratings, train_model, recommendedNumber)
    sc.stop()
  }

}
