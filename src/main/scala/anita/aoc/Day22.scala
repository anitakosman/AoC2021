package anita.aoc

import anita.aoc.Util.getInput

import scala.collection.mutable
import scala.util.matching.Regex

object Day22 {
  case class Instruction(on: Boolean, cube: Cuboid)

  case class Cuboid(fromX: Long, toX: Long, fromY: Long, toY: Long, fromZ: Long, toZ: Long) {
    def size: Long = if (toX < fromX || toY < fromY || toZ < fromZ) 0L
    else (toX - fromX + 1L) * (toY - fromY + 1L) * (toZ - fromZ + 1L)

    def intersect(other: Cuboid): Cuboid =
      Cuboid(math.max(fromX, other.fromX), math.min(toX, other.toX),
        math.max(fromY, other.fromY), math.min(toY, other.toY),
        math.max(fromZ, other.fromZ), math.min(toZ, other.toZ))
  }

  def part1(instructions: List[Instruction]): Long = {
    countOn(instructions, Cuboid(-50, 50, -50, 50, -50, 50))
  }

  def part2(instructions: List[Instruction]): Long = {
    countOn(instructions, Cuboid(-99999, 99999, -99999, 99999, -99999, 99999))
  }

  def main(args: Array[String]): Unit = {
    val input = parseInput()
    println(part1(input))
    println(part2(input))
  }

  def parseInput(): List[Instruction] = {
    val instructionRegex = new Regex("(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)")
    getInput(22).map { case instructionRegex(onOff, fromX, toX, fromY, toY, fromZ, toZ) =>
      Instruction(onOff == "on", Cuboid(fromX.toLong, toX.toLong, fromY.toLong, toY.toLong, fromZ.toLong, toZ.toLong))
    }.reverse
  }

  private def countOn(instructions: List[Instruction], cube: Cuboid): Long = {
    val cache = new mutable.HashMap[(Long, Cuboid), Long]()

    def go(instructions: List[Instruction], regionCube: Cuboid): Long = {
      if (regionCube.size == 0) 0L
      else cache.getOrElseUpdate((instructions.size, regionCube),
        instructions.headOption match {
          case None => 0L
          case Some(Instruction(true, instructionCube)) =>
            val alreadyOn = countOn(instructions.tail, regionCube)
            val intersection = instructionCube.intersect(regionCube)
            val newOn = intersection.size
            val doubleOn = countOn(instructions.tail, intersection)
            alreadyOn + newOn - doubleOn
          case Some(Instruction(false, instructionCube)) =>
            val alreadyOn = countOn(instructions.tail, regionCube)
            val intersection = instructionCube.intersect(regionCube)
            val newOff = countOn(instructions.tail, intersection)
            alreadyOn - newOff
        }
      )
    }

    go(instructions, cube)
  }
}
