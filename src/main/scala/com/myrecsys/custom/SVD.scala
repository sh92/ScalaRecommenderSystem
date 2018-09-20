package com.myrecsys.custom

import com.myrecsys.myrecsys._

import scala.util.Random

class SVD extends Model {

  val (gamma1, gamma2) = (0.02, 0.02)
  val (lambda6, lambda7) = ( 0.05, 0.01)
  val bu = Array.fill(numOfUser)(0.0)
  val bi = Array.fill(numOfMovie)(0.0)
  val y = Array.fill(numOfMovie)(Array.fill[Double](numOfFactor)(Random.nextGaussian() * 0.1))

  def estimate(userIndex: Int, movieIndex: Int) = {
    bui(userIndex,movieIndex)
      +dotProduct(P, Q, userIndex, movieIndex, numOfFactor)
  }

  private def bui(u:Int, i:Int): Double ={
    avg+bu(u)+bi(i)
  }

  private def gradientDescent(): Double = {
    for(u <- 0 until numOfUser ; i <- 0 until numOfMovie){
      if (ratingMatrix(u)(i) > 0){
        val eui = ratingMatrix(u)(i) - estimate(u,i)
        bu(u) += gamma1 * (eui - lambda6 * bu(u))
        bi(i) += gamma1 * (eui - lambda6 * bi(i))


        for(f <- 0 until numOfFactor) {
          Q(f)(i) += gamma2 * (eui * (P(u)(f) - lambda7 * Q(f)(i)))
          P(u)(f) += gamma2 * (eui * (Q(f)(i) - lambda7 * P(u)(f)))
        }

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
          error += (lambda7) * (P(u)(f) * P(u)(f) + Q(f)(i) * Q(f)(i))
        }
      }
    }

    error
  }

  var min = math.abs(gradientDescent())
  var step = 0
  for(i <- 1 to steps){
    val err = math.abs(gradientDescent())
    println("step " + i + " : error = " + err)
    if(err < min){
      min = err
      step = i
    }
  }
  println("min error : " + min)

}
