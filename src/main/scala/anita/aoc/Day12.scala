package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day12 {
  type Path = List[String]

  def part1(caveSystem: Map[String, List[String]]): Int = {
    findPaths(caveSystem, (path, option) => option.forall(_.isUpper) || !path.contains(option))
  }

  def part2(caveSystem: Map[String, List[String]]): Int = {
    findPaths(caveSystem, (path, option) => option.forall(_.isUpper) || option != "start" &&
      (if (containsDoubleSmallCave(path)) !path.contains(option)
      else path.count(_ == option) <= 1)
    )
  }

  def main(args: Array[String]): Unit = {
    val caveSystem = parseInput()
    println(part1(caveSystem))
    println(part2(caveSystem))
  }

  def parseInput(): Map[String, List[String]] = {
    val lineRegex = new Regex("([a-zA-Z]*)-([a-zA-Z]*)")
    getInput(12).flatMap { case lineRegex(a, b) => List((a, b), (b,a)) }.groupBy(_._1).map { case (k, v) => (k, v.map(_._2)) }
  }

  def findPaths(caveSystem: Map[String, List[String]], permittedNextCave: (Path, String) => Boolean): Int = {
    @tailrec
    def go(currentPaths: List[Path]): List[Path] = {
      if (currentPaths.forall(_.head == "end")) currentPaths
      else {
        val oneCaveFurther = currentPaths.flatMap(path =>
          if (path.head == "end") List(path)
          else if(caveSystem.contains(path.head)) caveSystem(path.head).map(option =>
            if (!permittedNextCave(path, option)) List.empty
            else option :: path
          ).filter(_ != List.empty)
          else List.empty
        )
        go(oneCaveFurther.distinct.filter(_ != List.empty))
      }
    }

    go(List(List("start"))).size
  }

  def containsDoubleSmallCave(path: Path): Boolean = path.filter(_.forall(_.isLower)).groupBy(identity).exists(_._2.size > 1)
}
