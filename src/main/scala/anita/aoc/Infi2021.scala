package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec
import scala.util.matching.Regex

object Infi2021 {
  def part1(partsMap: Map[String, Map[String, Int]]): Int = {
    takeApart(partsMap).maxBy(_._2.values.sum)._2.values.sum
  }

  def part2(missing: Int, partsMap: Map[String, Map[String, Int]]): String = {
    val parts = partsMap.flatMap(_._2.keys).toSet
    val toys = takeApart(partsMap).filter(e => !parts.contains(e._1))
    val counts = toys.map { case (k, v) => k -> v.values.sum }
    findWrappedGifts(20, missing, counts)
  }

  def main(args: Array[String]): Unit = {
    val (missing, parts) = parseInput()
    println(part1(parts))
    println(part2(missing, parts))
  }

  def parseInput(): (Int, Map[String, Map[String, Int]]) = {
    val input = getInput(2021, "infi")
    val missing = input.head.takeWhile(_ != ' ').toInt
    val lineRegex = new Regex("(\\w+): (.+)")
    val partsRegex = new Regex("(\\d+) (\\w+)")
    val partsMap = input.tail.map {
      case lineRegex(toy, parts) => toy -> parts.split(", ").map { case partsRegex(n, part) => part -> n.toInt }.toMap
    }.toMap
    (missing, partsMap)
  }

  @tailrec
  def takeApart(partsMap: Map[String, Map[String, Int]]): Map[String, Map[String, Int]] =
    if (!partsMap.values.flatMap(_.keys).exists(partsMap.contains)) partsMap
    else takeApart(partsMap.map { case (toy, parts) => toy -> parts.toList.flatMap {
      case (part, n) =>
        if (partsMap.contains(part))
          partsMap(part).map { case (innerPart, m) => innerPart -> n * m }
        else Map(part -> n)
    }.groupBy(_._1).map { case (k, v) => k -> v.map(_._2).sum } })

  def findWrappedGifts(toys: Int, parts: Int, partsInToys: Map[String, Int]): String = {
    val lists: List[List[Int]] = (0 to partsInToys.size).foldRight(List(List[Int]()))((_, acc) => for {
      l <- acc
      n <- 0 to (20 - l.sum)
    } yield n :: l)
    val possibilities = lists.filter(_.sum == 20)

    possibilities.map(_.zip(partsInToys)).filter(l => l.map{ case (nrToys, (_, nrParts)) => nrToys * nrParts}.sum == parts)
      .head.map{ case (nrToys, (toy, _)) => List.fill(nrToys)(toy.head).mkString }.sorted.mkString
  }
}
