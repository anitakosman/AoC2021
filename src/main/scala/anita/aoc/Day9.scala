package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec

object Day9 {
  type Cave = Map[(Int, Int), Int]
  type Basin = Set[(Int, Int)]

  def part1(cave: Cave): Int = {
    lowestPoints(cave).map(cave(_) + 1).sum
  }

  def part2(cave: Cave): Int = {
    lowestPoints(cave).map(p => basinSize(cave, p)).sorted.reverse.take(3).product
  }

  def main(args: Array[String]): Unit ={
    val cave = parseInput()
    println(part1(cave))
    println(part2(cave))
  }

  def parseInput(): Cave = {
    getInput(9).zipWithIndex.flatMap{ case (line, y) => line.zipWithIndex.map{ case (c, x) => ((x,y), c.toString.toInt) } }.toMap
  }

  def lowestPoints(cave: Cave): List[(Int, Int)] = {
    val m = cave.keys.map(_._1).max
    val n = cave.keys.map(_._2).max
    (0 to m).flatMap(x => (0 to n).map(y => (x,y))).filter{ case (x, y) =>
      (x == 0 || cave((x - 1, y)) > cave((x,y))) &&
        (y == 0 || cave((x, y - 1)) > cave((x,y))) &&
        (x == m || cave((x + 1, y)) > cave((x,y))) &&
        (y == n || cave((x, y + 1)) > cave((x,y)))
    }.toList
  }

  def basinSize(cave: Cave, p: (Int, Int)): Int = {
    val m = cave.keys.map(_._1).max
    val n = cave.keys.map(_._2).max
    def go(basin: Basin, prev: Basin): Basin = {
      if (basin == prev) basin
      else go(exploreBasin(basin, cave, m, n) , basin)
    }

    go(Set(p), Set.empty).size
  }

  def exploreBasin(basin: Basin, cave: Cave, m: Int, n:Int): Basin = {
    basin ++ basin.flatMap{ case (x, y) =>
      (x - 1 to 0 by -1).takeWhile(x2 => !basin.contains(x2,y) && cave((x2, y)) != 9).map((_, y)) ++
        (x + 1 to m).takeWhile(x2 => !basin.contains(x2,y) && cave((x2, y)) != 9).map((_, y)) ++
        (y - 1 to 0 by -1).takeWhile(y2 => !basin.contains(x,y2) && cave((x, y2)) != 9).map((x, _)) ++
        (y + 1 to n).takeWhile(y2 => !basin.contains(x,y2) && cave((x, y2)) != 9).map((x, _))
    }
  }
}
