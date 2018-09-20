package com.myrecsys

import java.nio.charset.CodingErrorAction

import scala.io.{Codec, Source}

object MovieLense {

  val data_dir = "../ml-100k/"
  val ratingsPath = data_dir+"u.data"
  val ratingsSplit = "\t"
  val moviesPath = data_dir+"u.item"
  val moviesSplit = "|"

  var movieDict : Map[Int,String]= Map()
  var ratingDict : Map[(Int,Int),Double ]= Map()

  def getMovieName() : Map[Int, String] = {

    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)

    val lines = Source.fromFile(moviesPath).getLines()
    for (line <- lines) {
      val fields = line.split(moviesSplit)
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
      val fields = line.split(ratingsSplit)
      if (fields.length > 1) {
        ratingDict += ( (fields(0).toInt,fields(1).toInt) -> fields(2).toDouble )
      }
    }
    return ratingDict
  }
}
