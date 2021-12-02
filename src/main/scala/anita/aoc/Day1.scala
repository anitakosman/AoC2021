package anita.aoc

import anita.aoc.Util._

object Day1 {
  def part1(): Int = {
    val input = getInput(1)
    countIncreases(input.map(_.toInt))
  }

  def part2(): Int = {
    val input = getInput(1)
    countIncreases(input.map(_.toInt).sliding(3).toList.map(_.sum))
  }

  def main(args: Array[String]): Unit ={
    println(part1())
    println(part2())
  }

  def countIncreases(l: List[Int]): Int = zipWithNext(l).map { case (a, b) => b - a }.count(d => d > 0)
}
