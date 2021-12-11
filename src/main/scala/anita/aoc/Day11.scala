package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec

object Day11 {
  type Grid = List[List[Int]]
  val n = 10

  def part1(grid: Grid): Int = {
    live(grid, 100)
  }

  def part2(grid: Grid): Int = {
    synchronize(grid)
  }

  def main(args: Array[String]): Unit = {
    val grid = parseInput()
    println(part1(grid))
    println(part2(grid))
  }

  def parseInput(): Grid = {
    getInput(11).map(s => s.map(_.toString.toInt).toList)
  }

  def live(startingGrid: Grid, days: Int): Int = {
    @tailrec
    def go(grid: Grid, n: Int, flashes: Int): Int = {
      if (n <= 0) flashes
      else {
        val lightedUp = lightUp(grid)
        go(lightedUp.map(_.map(x => if (x >= 10) 0 else x)), n - 1, flashes + lightedUp.map(_.count(_ >= 10)).sum)
      }
    }

    go(startingGrid, days, 0)
  }

  def synchronize(startingGrid: Grid): Int = {
    @tailrec
    def go(grid: Grid, n: Int): Int = {
      if (grid.forall(_.forall(_ == 0))) n
      else {
        go(lightUp(grid).map(_.map(x => if (x >= 10) 0 else x)), n + 1)
      }
    }

    go(startingGrid, 0)
  }

  def lightUp(startingGrid: Grid): Grid = {
    @tailrec
    def go(grid: Grid): Grid = {
      if (grid.map(_.count(_ == 10)).sum == 0) grid
      else go(powerUpNeighbors(grid))
    }

    go(startingGrid.map(_.map(_ + 1)))
  }

  def neighbors(x: Int, y: Int): Seq[(Int, Int)] = {
    for {
      dx <- -1 to 1
      dy <- -1 to 1
    } yield (x + dx, y + dy)
  }

  def powerUpNeighbors(grid: Grid): Grid = {
    val flashing = for {
      x <- 0 until n
      y <- 0 until n
      if grid(y)(x) == 10
    } yield (x, y)

    grid.zipWithIndex.map { case (row, y) => row.zipWithIndex.map { case (i, x) =>
      if (i < 10) Math.min(10, i + neighbors(x, y).count(flashing.contains(_))) else i + 1
    }}
  }
}
