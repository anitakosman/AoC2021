package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day2 {
  def part1(): Int = {
    val input = getInput(2)
    val (x,d) = followMadeUpInstructions(input, 0, 0)
    x * d
  }

  def part2(): Int = {
    val input = getInput(2)
    val (x,d) = followActualInstructions(input, 0, 0, 0)
    x * d
  }

  val instruction = new Regex("(forward|down|up) (\\d+)")
  @tailrec
  def followMadeUpInstructions(instructions: List[String], x: Int, d: Int): (Int, Int) = instructions match {
    case instruction("forward", n) :: tail => followMadeUpInstructions(tail, x + n.toInt, d)
    case instruction("down", n) :: tail => followMadeUpInstructions(tail, x, d + n.toInt)
    case instruction("up", n) :: tail => followMadeUpInstructions(tail, x, d - n.toInt)
    case Nil => (x, d)
  }

  @tailrec
  def followActualInstructions(instructions: List[String], x: Int, d: Int, a: Int): (Int, Int) = instructions match {
    case instruction("forward", n) :: tail => followActualInstructions(tail, x + n.toInt, d + (n.toInt * a), a)
    case instruction("down", n) :: tail => followActualInstructions(tail, x, d, a + n.toInt)
    case instruction("up", n) :: tail => followActualInstructions(tail, x, d, a - n.toInt)
    case Nil => (x, d)
  }

  def main(args: Array[String]): Unit ={
    println(part1())
    println(part2())
  }
}
