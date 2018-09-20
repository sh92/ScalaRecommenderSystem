package com.myrecsys

import scala.io.Source

package object myrecsys {

	val ratingObj:Ratings = new Ratings()

	val (algorithm, steps, numOfFactor, k) = (3, 5, 50, 30)
	val (filePath, splitStr) = (MovieLense.ratingsPath, MovieLense.ratingsSplit)
	Source.fromFile(filePath)
		.getLines
		.toList
		.map{line =>
			val fields = line.split(splitStr)
			val rating = customRating(fields(0).toInt, fields(1).toInt, fields(2).toDouble, fields(3).toLong)
			ratingObj.addRating(rating)
			}

	val numOfUser = ratingObj.maxUserId
	val numOfMovie = ratingObj.maxMovieId


	val (trainset, testset) = ratingObj.randomSplit(Seq(0.7, 0.3))
	val trainObj = new Ratings()
	trainObj.init(trainset)
	val testObj = new Ratings()
	testObj.init(testset)

	trainObj.maxMovieId = numOfMovie
	trainObj.maxUserId = numOfUser

	testObj.maxMovieId = numOfMovie
	testObj.maxUserId = numOfUser


	val ratingMatrix = trainObj.createAndFillMatrix()
	val testMatrix = testObj.createAndFillMatrix()

	val (trainX, trainY) = trainObj.Decomposition(ratingMatrix)
	val (testX, testY) = testObj.Decomposition(testMatrix)


}
