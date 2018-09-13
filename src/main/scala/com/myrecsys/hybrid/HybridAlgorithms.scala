package com.myrecsys.hybrid

class HybridAlgorithms(algorithms: List[Algorithm], weights:List[Double]) {

  def fit(): Unit ={

  }

  def estimate(a:Algorithm, b:Algorithm): Double ={
    var sumScores:Double = 0.0
    var sumWeights:Double = 0.0
    for ( idx <- 1 to algorithms.length ){
      sumScores = sumScores + algorithms.apply(idx).estimate(a, b) *  weights.apply(idx)
      sumWeights = sumWeights + weights.apply(idx)
    }
    return sumScores / sumWeights
  }
}

object HybridAlgorithms {


}
