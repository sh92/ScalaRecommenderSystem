package com.myrecsys

import breeze.numerics.{abs, sqrt}
import org.apache.spark.mllib.recommendation.{MatrixFactorizationModel, Rating}
import org.apache.spark.rdd.RDD

object Metrics {

  def maeRDD(model: MatrixFactorizationModel, data: RDD[Rating]): Double ={
    val usersProducts = data.map { case Rating(userId, movieId, rating) =>
      (userId, movieId)
    }
    val predictions = model.predict(usersProducts).map {
      case Rating(userId, movieId, rating) => ((userId, movieId), rating)
    }
    val originAndPredict = data.map { case Rating(userId, movieId, rating) =>
      ((userId, movieId), rating)
    }.join(predictions)
    val MAE = originAndPredict.map { case ((userId, movieId), (r1, r2)) =>
      abs(r1 - r2)
    }.mean()
    MAE
  }

  def rmseRDD(model: MatrixFactorizationModel, data: RDD[Rating]):Double = {
    val usersProducts = data.map { case Rating(userId, movieId, rating) =>
      (userId, movieId)
    }
    val predictions = model.predict(usersProducts).map {
      case Rating(userId, movieId, rating) => ((userId, movieId), rating)
    }
    val originAndPredict = data.map { case Rating(userId, movieId, rating) =>
      ((userId, movieId), rating)
    }.join(predictions)
    val MSE = originAndPredict.map { case ((userId, movieId), (r1, r2)) =>
      val err = (r1 - r2)
      err * err
    }.mean()
    sqrt(MSE)
  }


  def mae(actualList: List[Double], predictList: List[Double]):Double = {
    var errsum:Double = 0.0
    for ((x,y) <- actualList zip predictList){
      errsum += abs(x-y)
    }
    println(errsum, actualList.length)
    errsum / actualList.length
  }

  def rmse(actualSeq: List[Double], predictSeq: List[Double]):Double = {
    var errsum:Double = 0.0
    for ((x,y) <- actualSeq zip predictSeq){
      errsum += (x-y)*(x-y)
    }
    println(errsum, actualSeq.length)
    sqrt(errsum / actualSeq.length)
  }

}
