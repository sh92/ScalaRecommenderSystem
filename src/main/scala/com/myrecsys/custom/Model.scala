package com.myrecsys.custom

import com.myrecsys.myrecsys._

import scala.util.Random

abstract class Model {


  protected val P = Array.fill(numOfUser)(Array.fill[Double](numOfFactor)(Random.nextDouble))
  protected val Q = Array.fill(numOfFactor)(Array.fill[Double](numOfMovie)(Random.nextDouble))
  protected def dotProduct(A:Array[Array[Double]], B:Array[Array[Double]], j:Int, i:Int, f:Int): Double ={
    var sum = 0.0
    for(h <- 0 until f){
        sum += A(j)(h) * B(h)(i)
    }
    sum
  }

  protected def dotProduct(userIndex: Int, movieIndex: Int) = {
    var sum = 0.0
    for(h <- 0 until numOfFactor) {
      sum += + P(userIndex)(h) * Q(h)(movieIndex)
    }
    sum
  }

  val avg:Double = {
    var count = 0
    var sum = 0.0
    for (u <- 0 until numOfUser) {
      for (i <- 0 until numOfMovie) {
        if (ratingMatrix(u)(i) > 0.0) {
          sum += ratingMatrix(u)(i)
          count += 1
        }
      }
    }
    sum / count
  }

  def uniform(min: Double, max: Double, rn:Int): Double = {
    val random = new Random(rn)
    random.nextDouble() * (max - min) + min
  }

  def binomial(n: Int, p: Double, rn:Int ): Int = {
    val random = new Random(rn)
    if(p < 0 || p > 1) return 0

    var c: Int = 0
    var r: Double = 0

    var i: Int = 0
    for(i <- 0 until n) {
      r = random.nextDouble()
      if(r < p) c += 1
    }

    c
  }

  def sigmoid(x: Double): Double = 1.0 / (1.0 + math.pow(math.E, -x))

  def softmax(x: Array[Double], n_out:Int) {
    var max: Double = 0.0
    var sum: Double = 0.0

    var i: Int = 0
    for(i <- 0 until n_out) if(max < x(i)) max = x(i)

    for(i <- 0 until n_out) {
      x(i) = math.exp(x(i) - max)
      sum += x(i)
    }

    for(i <- 0 until n_out) x(i) /= sum
  }

  def reAdjust(userIndex: Int, movieIndex: Int) = {

    val value = estimate(userIndex, movieIndex)

    value match {
      case v if v < 0.0 => 0.0
      case v if v > 5.0 => 5.0
      case _ => value
    }
  }

  def estimate(userIndex: Int, movieIndex: Int): Double{}
}
