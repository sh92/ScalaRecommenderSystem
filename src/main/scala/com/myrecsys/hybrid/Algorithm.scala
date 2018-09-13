package com.myrecsys.hybrid

abstract class Algorithm() {
   def estimate(a: Algorithm, b: Algorithm): Double
   def fit(trainset:Unit): Unit
}
