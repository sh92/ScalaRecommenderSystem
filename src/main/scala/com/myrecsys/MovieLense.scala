package com.myrecsys

import java.nio.charset.CodingErrorAction

import scala.io.{Codec, Source}

object MovieLense {

  val ratingsPath = "../ml-100k/u.data"
  val moviesPath = "../ml-100k/u.item"

  var movieDict : Map[Int,String]= Map()
  var ratingDict : Map[(Int,Int),Double ]= Map()

  def getMovieName() : Map[Int, String] = {

    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    val lines = Source.fromFile(moviesPath).getLines()
    for (line <- lines) {
      val fields = line.split('|')
      if (fields.length > 1) {
        movieDict += (fields(0).toInt -> fields(1))
      }
    }
    return movieDict
  }

  def getRatings() : Map[(Int,Int),Double ] = {

    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    val lines = Source.fromFile(ratingsPath).getLines()
    for (line <- lines) {
      val fields = line.split('|')
      if (fields.length > 1) {
        ratingDict += ( (fields(0).toInt,fields(1).toInt) -> fields(2).toDouble )
      }
    }
    return ratingDict
  }
}
