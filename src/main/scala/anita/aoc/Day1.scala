package anita.aoc

import anita.aoc.Util._
import scala.io.Source

object Day1 {
  def part1(): Int = {
    val resource = Source.fromResource("day1.txt")
    val depths = resource.getLines().toList.map(_.toInt)
    resource.close()
    countIncreases(depths)
  }

  def part2(): Int = {
    val resource = Source.fromResource("day1.txt")
    val depths = resource.getLines().toList.map(_.toInt)
    resource.close()
    val triples = depths.sliding(3).toList.map(_.sum)
    countIncreases(triples)
  }

  def main(args: Array[String]): Unit ={
    println(part1())
    println(part2())
  }

  def countIncreases(l: List[Int]): Int = zipWithNext(l).map { case (a, b) => b - a }.count(d => d > 0)
}
