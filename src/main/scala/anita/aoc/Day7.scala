package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec

object Day7 {
  def part1(crabs: Array[Int], min: Int, max: Int): Int = {
    (min to max).map(x => crabs.map(c => Math.abs(c - x)).sum).minBy(identity)
  }

  def part2(crabs: Array[Int], min: Int, max: Int): Int = {
    (min to max).map(x => crabs.map(c => triangularNumber(Math.abs(c - x))).sum).minBy(identity)
  }

  def main(args: Array[String]): Unit ={
    val crabs = parseInput()
    val n = crabs.length
    val average = crabs.sum / n
    val median = if (n % 2 == 1) crabs.drop(n / 2).head else (crabs.drop((n / 2) - 1).head + crabs.drop(n / 2).head) / 2
    val (a,b) = if (median <= average) (median, average) else (average, median)
    println(part1(crabs, a, b))
    println(part2(crabs, a, b))
  }

  def parseInput():  Array[Int] = {
    getInput(7).head.split(",").map(_.toInt).sorted
  }


}
