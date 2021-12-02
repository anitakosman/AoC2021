package anita.aoc

import scala.io.Source

object Util {
  def getInput(day: Int): List[String] = {
    val resource = Source.fromResource(s"day$day.txt")
    val input = resource.getLines().toList
    resource.close()
    input
  }

  def zipWithNext[A](l: List[A]): List[(A,A)] = l zip l.tail
}
