package anita.aoc

import anita.aoc.Util._

object Day1 {
  def part1(input: List[Int]): Int = {
    countIncreases(input)
  }

  def part2(input: List[Int]): Int = {
    countIncreases(input.sliding(3).toList.map(_.sum))
  }

  def main(args: Array[String]): Unit ={
    val input = getInput(1).map(_.toInt)
    println(part1(input))
    println(part2(input))
  }

  def countIncreases(l: List[Int]): Int = zipWithNext(l).map { case (a, b) => b - a }.count(d => d > 0)
}
