package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec

object Day7 {
  def part1(crabs: Array[Int], n: Int): Int = {
    val median = if (n % 2 == 1) crabs.drop(n / 2).head else (crabs.drop((n / 2) - 1).head + crabs.drop(n / 2).head) / 2
    crabs.map(c => Math.abs(c - median)).sum
  }

  def part2(crabs: Array[Int], n: Int): Int = {
    val average = crabs.sum / n
    crabs.map(c => triangularNumber(Math.abs(c - average))).sum
  }

  def main(args: Array[String]): Unit ={
    val crabs = parseInput()
    val n = crabs.length
    println(part1(crabs, n))
    println(part2(crabs, n))
  }

  def parseInput():  Array[Int] = {
    getInput(7).head.split(",").map(_.toInt).sorted
  }


}
