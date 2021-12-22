package anita.aoc

import anita.aoc.Util.getInput

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day22 {
  type Cuboid = Set[(Int, Int, Int)]

  def part1(input: List[String]): Int = {
    onCubes(input, toLimitedCuboid(-50, 50, -50, 50, -50, 50)).size
  }

  def part2(input: List[String]): Long = {
    (for {
      minX <- (-100000 to 99900 by 100)
      minY <- (-100000 to 99900 by 100)
      minZ <- (-100000 to 99900 by 100)
    } yield onCubes(input, toLimitedCuboid(minX, minX + 100, minY, minY + 100, minZ, minZ + 100)).size.toLong).sum
  }

  def main(args: Array[String]): Unit = {
    val input = parseInput()
    println(part1(input))
    println(part2(input))
  }

  def parseInput(): List[String] = {
    getInput(22)
  }

  def onCubes(input: List[String], toCuboid: (String, String, String, String, String, String) => Seq[(Int, Int, Int)]): Cuboid = {
    val instructionRegex = new Regex("(on|off) x=(-?\\d+)..(-?\\d+),y=(-?\\d+)..(-?\\d+),z=(-?\\d+)..(-?\\d+)")
    @tailrec
    def go(cubes: Cuboid, instructions: List[String]): Cuboid = instructions match {
      case Nil => cubes
      case instructionRegex("on", fromX, toX, fromY, toY, fromZ, toZ) :: tail =>
        val cubesToAdd = toCuboid(fromX, toX, fromY, toY, fromZ, toZ)
        go(cubes ++ cubesToAdd, tail)
      case instructionRegex("off", fromX, toX, fromY, toY, fromZ, toZ) :: tail =>
        val cubesToRemove = toCuboid(fromX, toX, fromY, toY, fromZ, toZ)
        go(cubes -- cubesToRemove, tail)
    }

    go(Set.empty, input)
  }

  private def toLimitedCuboid(minX: Int, maxX: Int, minY: Int, maxY: Int, minZ: Int, maxZ: Int)(fromX: String, toX: String, fromY: String, toY: String, fromZ: String, toZ: String): Seq[(Int, Int, Int)] = {
    for {
      x <- math.max(minX, fromX.toInt) to math.min(maxX, toX.toInt)
      y <- math.max(minY, fromY.toInt) to math.min(maxY, toY.toInt)
      z <- math.max(minZ, fromZ.toInt) to math.min(maxZ, toZ.toInt)
    } yield (x, y, z)
  }
}
