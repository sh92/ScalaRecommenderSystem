package com.myrecsys.custom

import com.myrecsys.Metrics
import com.myrecsys.myrecsys._

import scala.collection.mutable.ArrayBuffer

object Main {
  def main(args: Array[String]){

    val matrix = algorithm match {
      case 1 =>
        println("Running SVD algorithm")
        println
        new SVD
      case 2 =>
        println("Running SVD++ algorithm")
        println
        new SVDPlusPlus
      case 3 =>
        println("Running SVD and SVD++ Hybrid algorithm")
        println
        new HybridAlgorithms(Seq(new SVD, new SVDPlusPlus),Seq(0.3, 0.7))
    }

    val actualList=ArrayBuffer[Double]()
    val predictList=ArrayBuffer[Double]()

    for(test <- testset.toList){
      val actualRating = test.rating
      val predictRating = matrix.reAdjust(test.userId-1, test.movieId - 1)
      println("User " + test.userId + " with movie " + test.movieId + " : ")
      println(" Predicted rating " + "%.3f".format(predictRating) )
      println(" Actual rating " + actualRating)
      println
      actualList += actualRating
      predictList += predictRating

    }
    println("Ratings : ",ratingObj.ratings.length)
    println("num of Movie: " + numOfMovie )
    println("num of User: " + numOfUser )
    println("MAE = " + "%.3f".format(Metrics.mae(actualList.toList,predictList.toList)))
    println("RMSE = " + "%.3f".format(Metrics.rmse(actualList.toList,predictList.toList)) )

  }
}
