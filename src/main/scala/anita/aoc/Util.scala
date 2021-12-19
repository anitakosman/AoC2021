package anita.aoc

import scala.io.Source

object Util {
  def getInput(number: Int, prefix: String = "day"): List[String] = {
    val resource = Source.fromResource(s"$prefix$number.txt")
    val input = resource.getLines().toList
    resource.close()
    input
  }

  def zipWithNext[A](l: List[A]): List[(A,A)] = l zip l.tail

  def triangularNumber(n: Int): Int = n * (n + 1) / 2
}
