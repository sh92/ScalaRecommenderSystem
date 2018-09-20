package com.myrecsys.custom

class HybridAlgorithms(algorithms: Seq[Model], weights:Seq[Double]) extends Model{

  override def estimate(userIndex: Int, movieIndex: Int): Double ={
    var sumScores:Double = 0.0
    var sumWeights:Double = 0.0
    for ( idx <- 1 to algorithms.length ){
      sumScores = sumScores + algorithms(idx-1).estimate(userIndex, movieIndex) *  weights(idx-1)
      sumWeights = sumWeights + weights(idx-1)
    }
    return sumScores / sumWeights
  }
}
