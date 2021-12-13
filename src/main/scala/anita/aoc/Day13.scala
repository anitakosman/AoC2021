package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day13 {
  sealed trait Axis
  case object X extends Axis
  case object Y extends Axis

  def part1(dots: Set[(Int, Int)], instructions: List[(Axis, Int)]): Int = {
    fold(dots, List(instructions.head)).size
  }

  def part2(dots: Set[(Int, Int)], instructions: List[(Axis, Int)]): String = {
    val finalDots = fold(dots, instructions)
    (0 to finalDots.maxBy(_._2)._2).map(y =>
      (0 to finalDots.maxBy(_._1)._1).map(x => if (finalDots((x,y))) "#" else ".").mkString
    ).mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    val (dots, instructions) = parseInput()
    println(part1(dots, instructions))
    println(part2(dots, instructions))
  }

  def parseInput(): (Set[(Int, Int)], List[(Axis, Int)]) = {
    val input = getInput(13)

    val pointRegex = new Regex("(\\d+),(\\d+)")
    val dots = input.takeWhile(_ != "").map { case pointRegex(a, b) => (a.toInt, b.toInt)}.toSet

    val instructionRegex = new Regex("fold along (x|y)=(\\d+)")
    val instructions = input.dropWhile(_ != "").drop(1).map {
      case instructionRegex("x", n) => (X, n.toInt)
      case instructionRegex("y", n) => (Y, n.toInt)
    }

    (dots, instructions)
  }

  @tailrec
  def fold(dots: Set[(Int, Int)], instructions: List[(Axis, Int)]): Set[(Int, Int)] = instructions match {
    case (X, n) :: tail => fold(dots.map{ case (x,y) => if (x < n) (x, y) else (2 * n - x, y) } , tail)
    case (Y, n) :: tail => fold(dots.map{ case (x,y) => if (y < n) (x, y) else (x, 2 * n - y) } , tail)
    case Nil => dots
  }
}
