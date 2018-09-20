package com.myrecsys.spark

import com.myrecsys.{MovieLense, customRating}
import org.apache.spark.mllib.recommendation.{MatrixFactorizationModel, Rating}
import org.apache.spark.rdd.RDD

object TopNRecommender{

  def getTopN(actual:RDD[customRating], predictions: RDD[Rating], n:Int=10, minimumRating:Double=4.0):Unit={
    var topN:Map[(Int,Int), Double] = Map()
    print(predictions)
    for(x <- predictions)
    {
      if (x.rating >= minimumRating){
        topN += ((x.user,x.product) -> x.rating)
      }
    }
    val sorted_topN = topN.toList.sortWith(_._2 > _._2)
    sorted_topN
  }

  def rddTopNPrint(rating: RDD[Rating], model: MatrixFactorizationModel, recommendedNumber: Int) = {
    val nameDict = MovieLense.getMovieName()
    println(nameDict)
    val userID = 1
    println("\nRatings for userID " + userID + " : ")
    val userRatings = rating.filter(x => x.user == userID)
    val myRatings = userRatings.collect()
    for (rating <- myRatings) {
      println(nameDict(rating.product.toInt) + " : " + rating.rating.toString)
    }
    println("\nTop "+ recommendedNumber+" recommendations:")
    val recommendedItems = model.recommendProducts(userID, recommendedNumber)
    for (item <- recommendedItems) {
      println(nameDict(item.product.toInt) + " : " + item.rating)
    }
  }
}
