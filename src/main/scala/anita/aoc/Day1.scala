package anita.aoc

import anita.aoc.Util._
import scala.io.Source

object Day1 {
  def part1(): Int = {
    val resource = Source.fromResource("day1.txt")
    val depths = resource.getLines().toList.map(_.toInt)
    val differences = zipWithNext(depths).map(d => d._2 - d._1)
    resource.close()
    differences.count(d => d > 0)
  }

  def part2(): Int = {
    val resource = Source.fromResource("day1.txt")
    val depths = resource.getLines().toList.map(_.toInt)
    val triples = zipWithNextTwo(depths).map(a => a._1 + a._2 + a._3)
    val differences = zipWithNext(triples).map(d => d._2 - d._1)
    resource.close()
    differences.count(d => d > 0)
  }

  def main(args: Array[String]): Unit ={
    println(part1())
    println(part2())
  }
}
