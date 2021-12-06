package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day6 {
  def part1(): BigInt = {
    val fish = parseInput()
    live(fish, 80)
  }

  def part2(): BigInt = {
    val fish = parseInput()
    live(fish, 256)
  }

  def main(args: Array[String]): Unit ={
    println(part1())
    println(part2())
  }

  def parseInput():  Map[Int, BigInt] = {
    val input = getInput(6).head.split(",").map(_.toInt)
    input.groupBy(identity).map(e => (e._1,e._2.length))
  }

  @tailrec
  def live(fish: Map[Int, BigInt], days: Int): BigInt = {
    if (days <= 0) fish.values.sum
    else {
      val newFish = fish.map { case (k, v) => (k - 1, v) }.toList.flatMap(e => e match {
        case (-1, v) => List((6,v), (8,v))
        case _ => List(e)
      }).groupBy(_._1).map{ case(k, v) => (k, v.map(_._2).sum)}
      live(newFish, days - 1)
    }
  }
}
