package com.myrecsys.custom

import com.myrecsys.myrecsys._

import scala.util.Random

class SVDPlusPlus extends Model {

  val (gamma1, gamma2, gamma3, gamma4, gamma5) = (0.001, 0.001, 0.0005, 0.03, 0.03)
  val (lambda1, lambda2, lambda3, lambda4, lambda5, lambda6, lambda7, lambda8) = (0.001, 0.02, 0.03, 0.02, 0.005, 0.0005, 0.001, 0.001)
  val bu = Array.fill(numOfUser)(0.0)
  val bi = Array.fill(numOfMovie)(0.0)
  val y = Array.fill(numOfMovie)(Array.fill[Double](numOfFactor)(Random.nextGaussian() * 0.1))
  val w = Array.fill(numOfUser)(Array.fill[Double](numOfMovie)(Random.nextGaussian() * 0.1))
  val c = Array.fill(numOfUser)(Array.fill[Double](numOfMovie)(Random.nextGaussian() * 0.1))
  val ratedMovieOfUserMatrix = ratingMatrix.map{ x =>
    for(i <- 0 until x.length if x(i) > 0.0 )
        yield i
  }

  def estimate(userIndex: Int, movieIndex: Int) = {
    var dotProductSum1 = 0.0
    var dotProductSum2 = 0.0
    var dotProductSum3 = 0.0
    for(j <- 0 until N(userIndex))
      dotProductSum1 += dotProduct(y, Q, j, movieIndex, numOfFactor)

    for (j <- 0 until Rk(userIndex, movieIndex))
      dotProductSum2 += (ratingMatrix(userIndex)(j)-bui(userIndex,j))*w(userIndex)(j)

//    for (j <- 0 until Nk(userIndex, movieIndex))
//      dotProductSum3 += c(userIndex)(j)

    bui(userIndex,movieIndex)
      +dotProduct(P, Q, userIndex, movieIndex, numOfFactor)
      +dotProductSum1 / Nsqrt(userIndex)
      +dotProductSum2 / math.sqrt(Rk(userIndex, movieIndex))
//      +dotProductSum3 / math.sqrt(Nk(userIndex, movieIndex))
  }

  private def Nsqrt(userIndex: Int) = {
    math.sqrt(N(userIndex))
  }

  private def R(userIndex: Int) = {
    ratedMovieOfUserMatrix(userIndex).size
  }

  private def Rk(userIndex:Int, movieIndex: Int) = {
    ratedMovieOfUserMatrix(userIndex).size
  }

  private def N(userIndex: Int) = {
    ratedMovieOfUserMatrix(userIndex).size
  }

  private def Nk(userIndex:Int, movieIndex: Int) = {
    ratedMovieOfUserMatrix(userIndex).size
  }
  private def bui(u:Int, i:Int): Double ={
    avg+bu(u)+bi(i)
  }

  private def gradientDescent() = {
    for(u <- 0 until numOfUser ; i <- 0 until numOfMovie){
      if (ratingMatrix(u)(i) > 0){
        val eui = ratingMatrix(u)(i) - estimate(u,i)
        bu(u) += gamma1 * (eui - lambda6 * bu(u))
        bi(i) += gamma1 * (eui - lambda6 * bi(i))

        var ysumf = 0.0
        val Nsqrtu=Nsqrt(u)
        for(j <- ratedMovieOfUserMatrix(u)){
          if(j < numOfFactor) {
            ysumf += y(u)(j)/Nsqrtu
          }
        }

        for(f <- 0 until numOfFactor){
          Q(f)(i) += gamma2 * (eui*(P(u)(f)+ysumf-lambda7*Q(f)(i)))
          P(u)(f) += gamma2 * (eui*(Q(f)(i)-lambda7*P(u)(f)))

          for(j <- ratedMovieOfUserMatrix(u)){
            y(j)(f) += gamma2 * (eui/Nsqrtu*Q(f)(i) - lambda7*y(j)(f))
            y(j)(f) = if (y(j)(f) > 0.5) 1 else 0
          }
        }

        for(j <- 0 until Rk(u,i)){
          w(u)(j) += gamma3 * ((eui * (ratingMatrix(u)(j) - bui(u,j))) / math.sqrt(Rk(u,j)) - lambda8 * w(u)(j))
        }

//        for(j <- ratedMovieOfUserMatrix(u)) {
//          c(u)(j) += gamma3 * (eui / math.sqrt(Nk(u,j)) - lambda8*c(u)(j))
//        }
      }
    }

    var error = 0.0

    for(u <- 0 until numOfUser ; i <- 0 until numOfMovie){

      if (ratingMatrix(u)(i) > 0) {
        val eui = ratingMatrix(u)(i) - estimate(u, i)
        val buv = bu(u)
        val biv = bi(i)
        error += eui * eui + (lambda6 / 2.0) * (buv * buv + biv * biv)

        for (f <- 0 until numOfFactor) {
          error += (lambda7 / 2.0) * (P(u)(f) * P(u)(f) + Q(f)(i) * Q(f)(i))
          for(j <- ratedMovieOfUserMatrix(u))
            error += (lambda7/2.0) * y(j)(f) * y(j)(f)
        }
        for (j <- ratedMovieOfUserMatrix(u)) {
          error += (lambda8 / 2.0) * w(u)(j) * w(u)(j)
        }
        //        for( j <- 0 until Rk(u,i)){
        //          error += (lambda8 / 2.0) * c(u)(j) * c(u)(j)
        //        }
      }
    }
    error
  }
  var min = math.abs(gradientDescent())
  var step = 0
  for (i <- 1 to steps) {
    val err = math.abs(gradientDescent())
    println("step:  " + i + " : error = " + err)
    if (err < min) {
      min = err
      step = i
    }
  }
  println("min error : " + min )

}
