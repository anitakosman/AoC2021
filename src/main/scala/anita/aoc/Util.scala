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

  implicit class StringExtension(val s: String) {
    def toInt(radix: Int): Int = Integer.parseInt(s, radix)

    def toLong(radix: Int): Long = java.lang.Long.parseLong(s, radix)

    def padStart(len: Int, c: Char): String = List.fill(len - s.length)(c).mkString + s
  }

  implicit class IntExtension(val n: Int) {
    def myMod(m: Int): Int = if (n % m == 0) m else n % m
  }
}
