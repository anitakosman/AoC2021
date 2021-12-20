package anita.aoc

import scala.collection.{immutable, mutable}

class Coordinate(val x: Int, val y: Int){
  lazy val squareAround: List[Coordinate] = for {
    dy <- immutable.List(-1, 0, 1)
    dx <- immutable.List(-1, 0, 1)
  } yield Coordinate(x + dx, y + dy)
}

object Coordinate {
  private val cache = new mutable.HashMap[(Int, Int), Coordinate]()

  def apply(x: Int, y: Int): Coordinate = cache.getOrElseUpdate((x,y), new Coordinate(x, y))
}
