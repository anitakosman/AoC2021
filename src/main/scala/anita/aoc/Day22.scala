package anita.aoc

import anita.aoc.Util.getInput

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day22 {
  case class Instruction(on: Boolean, fromX: Int, toX: Int, fromY: Int, toY: Int, fromZ: Int, toZ: Int)
  class Cube(val x: Int, val y: Int, val z: Int)

  def part1(instructions: List[Instruction]): Int = {
    countOn(instructions, -50, 50, -50, 50, -50, 50)
  }

  def part2(instructions: List[Instruction]): Long = {
    val minX = instructions.map(_.fromX).min
    val maxX = instructions.map(_.toX).max
    val minY = instructions.map(_.fromY).min
    val maxY = instructions.map(_.toY).max
    val minZ = instructions.map(_.fromZ).min
    val maxZ = instructions.map(_.toZ).max
    countOn(instructions, minX, maxX, minY, maxY, minZ, maxZ)
  }

  def main(args: Array[String]): Unit = {
    val input = parseInput()
    println(part1(input))
    println(part2(input))
  }

  def parseInput(): List[Instruction] = {
    val instructionRegex = new Regex("(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)")
    getInput(22).map { case instructionRegex(onOff, fromX, toX, fromY, toY, fromZ, toZ) =>
      Instruction(onOff == "on", fromX.toInt, toX.toInt, fromY.toInt, toY.toInt, fromZ.toInt, toZ.toInt)
    }.reverse
  }

  private def countOn(instructions: List[Instruction], fromX: Int, toX: Int, fromY: Int, toY: Int, fromZ: Int, toZ: Int): Int = {
    (for {
      x <- fromX to toX
      y <- fromY to toY
      z <- fromZ to toZ
      if isCubeOn(new Cube(x, y, z), instructions)
    } yield null).size
  }

  @tailrec
  def isCubeOn(cube: Cube, instructions: List[Instruction]): Boolean = instructions.headOption match {
    case None => false
    case Some(Instruction(on, fromX, toX, fromY, toY, fromZ, toZ))
      if fromX <= cube.x && cube.x <= toX && fromY <= cube.y && cube.y <= toY && fromZ <= cube.z && cube.z <= toZ => on
    case _ => isCubeOn(cube, instructions.tail)
  }
}
