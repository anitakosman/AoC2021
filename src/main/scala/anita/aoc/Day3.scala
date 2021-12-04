package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec

object Day3 {
  def part1(): Int = {
    val input = getInput(3).map(s => s.split("").map(_.toInt))
    val perBit = input.transpose
    val gamma = binaryToInt(perBit.map(_.groupBy(identity).maxBy(_._2.size)._1))
    gamma * (4095 ^ gamma)
  }

  def part2(): Int = {
    val input = getInput(3).map(s => s.split("").map(_.toInt))
    val oxygen = binaryToInt(getTheOne(input, filterOnMostCommonBit))
    val co2 = binaryToInt(getTheOne(input, filterOnLeastCommonBit))
    oxygen * co2
  }

  def main(args: Array[String]): Unit ={
    println(part1())
    println(part2())
  }

  def binaryToInt(b: List[Int]) = {
    Integer.parseInt(b.mkString, 2)
  }

  def getTheOne(input: List[Array[Int]], bitFilter: (List[Array[Int]], Int) => List[Array[Int]]): List[Int] = {
    @tailrec
    def go(l: List[Array[Int]], i: Int): Array[Int] =
      if (l.size == 1) l.head
      else {
        go(bitFilter(l, i), i + 1)
      }

    go(input, 0).toList
  }

  def filterOnMostCommonBit(l: List[Array[Int]], i: Int): List[Array[Int]] = {
    val mostCommon = l.transpose.drop(i).head.groupBy(identity).toList.sortBy(g => g._2.size).reverse.maxBy(_._2.size)._1
    l.filter(_.drop(i).head == mostCommon)
  }

  def filterOnLeastCommonBit(l: List[Array[Int]], i: Int): List[Array[Int]] = {
    val leastCommon = l.transpose.drop(i).head.groupBy(identity).toList.sortBy(g => g._2.size).minBy(_._2.size)._1
    l.filter(_.drop(i).head == leastCommon)
  }
}
