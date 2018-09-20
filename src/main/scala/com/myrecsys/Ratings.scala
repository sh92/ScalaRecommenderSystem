package com.myrecsys

import scala.collection.mutable.ListBuffer
import scala.util.Random
class Ratings() {


  def Decomposition(testMatrix: Array[Array[Double]]):(Array[Array[Int]], Array[Array[Int]]) = {
    val userMovieMatrix:Array[Array[Int]] = Array.fill(maxUserId)(Array.fill(maxMovieId)(0))
    val userRatingMatrix:Array[Array[Int]] = Array.fill(maxUserId)(Array.fill(6)(0))

//    println(testMatrix.length, testMatrix(0).length)
//    println(userMovieMatrix.length, userMovieMatrix(0).length)
//    println(userRatingMatrix.length, userRatingMatrix(0).length)
    for(x <- 0 until testMatrix.length){
      for(y <- 0 until testMatrix(0).length){
        if(testMatrix(x)(y) != 0)
          userMovieMatrix(x)(y) = 1
        val idx = testMatrix(x)(y).toInt
        userRatingMatrix(x)(idx) = 1
      }
    }
    (userMovieMatrix,userRatingMatrix)
  }


  var ratings:ListBuffer[customRating] = ListBuffer[customRating]()
  def init(_ratings: ListBuffer[customRating])={
    ratings = _ratings
  }


  var maxUserId:Int = 0
  var maxMovieId:Int = 0


  def createAndFillMatrix():Array[Array[Double]] ={
    val ratingMatrix = Array.fill(maxUserId)(Array.fill(maxMovieId)(0.0))
    ratings.foreach{ x => ratingMatrix(x.userId - 1)(x.movieId - 1) =  x.rating }
    ratingMatrix
  }


  def addRating(rating:customRating): Unit ={
    if(rating.userId>maxUserId)
      maxUserId = rating.userId
    if(rating.movieId>maxMovieId)
      maxMovieId = rating.movieId
    ratings += rating
  }


  def randomSplit(weight:Seq[Double]): (ListBuffer[customRating], ListBuffer[customRating]) ={
    val trainSize:Int = (ratings.length * weight(0)).toInt
    val testSize:Int = (ratings.length* weight(1)).toInt

    var trainset = ratings.clone()
    var testset:ListBuffer[customRating] = ListBuffer()
    println("Split Start")

    for(cnt <- 1 to testSize){
      if(cnt%1000==0){
        println(cnt+"/"+testSize)
      }
      val i = Random.nextInt(trainset.length)
      testset += trainset(i)
      trainset.remove(i)
    }
    println("Train size: ",trainset.length, " Test Size:", testset.length)
    (trainset,testset)
  }
}
