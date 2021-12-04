package anita.aoc

import anita.aoc.Util._

object Day3 {
  def part1(): Int = {
    val input = getInput(3).map(s => s.split("").map(_.toInt))
    val perBit = input.transpose
    val gamma = Integer.parseInt(perBit.map(_.groupBy(identity).maxBy(_._2.size)._1).mkString, 2)
    gamma * (4095 ^ gamma)
  }

  def part2(): Int = {
    val input = getInput(3).map(s => s.split("").map(_.toInt))
    ???
  }

  def main(args: Array[String]): Unit ={
    println(part1())
    println(part2())
  }
}
