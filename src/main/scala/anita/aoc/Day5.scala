package anita.aoc

import anita.aoc.Util._

import scala.util.matching.Regex

object Day5 {
  type Line = Iterable[(Int,Int)]

  def part1(input: List[String]): Int = {
    val lines = parseInput(input, false)
    lines.flatten.groupBy(identity).count(_._2.size > 1)
  }

  def part2(input: List[String]): Int = {
    val lines = parseInput(input, true)
    lines.flatten.groupBy(identity).count(_._2.size > 1)
  }

  def main(args: Array[String]): Unit ={
    val input = getInput(5)
    println(part1(input))
    println(part2(input))
  }

  def getLine(x1: Int, y1: Int, x2: Int, y2: Int, alsoDiagonals: Boolean): Line = {
    if (x1 > x2) getLine(x2, y2, x1, y1, alsoDiagonals)
    else if (y1 == y2) (x1 to x2).map((_,y1))
    else if (x1 == x2 && y1 > y2) getLine(x2, y2, x1, y1, alsoDiagonals)
    else if (x1 == x2) (y1 to y2).map((x1,_))
    else if (alsoDiagonals && y1 < y2) (x1 to x2).zip(y1 to y2)
    else if (alsoDiagonals) (x1 to x2).zip(y1 to y2 by -1)
    else List.empty
  }

  def parseInput(input: List[String], alsoDiagonals: Boolean):  List[Line] = {
    val lineRegex = new Regex("(\\d+),(\\d+) -> (\\d+),(\\d+)")
    input.map{ case lineRegex(x1,y1,x2,y2) => getLine(x1.toInt,y1.toInt,x2.toInt,y2.toInt, alsoDiagonals)}
  }
}