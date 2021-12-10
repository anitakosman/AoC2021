package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec

object Day10 {
  def part1(lines: List[String]): Long = {
    val points = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
    lines.map(parseLines(_, _ => 0, points)).sum
  }

  def part2(lines: List[String]): Long = {
    val points = Map(')' -> 1, ']' -> 2, '}' -> 3, '>' -> 4)
    def score(parsed: List[Char]): Long = parsed.foldLeft(0L)((n, c) => n * 5 + points(c))

    val scores = lines.map(parseLines(_, score ,_ => 0)).filter(_ != 0).sorted
    scores.drop(scores.size / 2).head //always an odd number
  }

  def main(args: Array[String]): Unit ={
    val lines = parseInput()
    println(part1(lines))
    println(part2(lines))
  }

  def parseInput(): List[String] = {
    getInput(10)
  }

  val closing = Map('(' -> ')', '[' -> ']', '{' -> '}', '<' -> '>')

  def parseLines(chars: String, incomplete: List[Char] => Long, corrupt: Char => Int): Long = {
    @tailrec
    def go(chars: String, parsed: List[Char]): Long = chars.headOption match {
      case None => incomplete(parsed)
      case Some(x) if "{([<".contains(x) => go(chars.tail, closing(x) :: parsed)
      case Some(x) if x == parsed.head => go(chars.tail, parsed.tail)
      case Some(x) => corrupt(x)
    }

    go(chars, List.empty)
  }
}
