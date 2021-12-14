package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec
import scala.collection.immutable.Map.WithDefault
import scala.util.matching.Regex

object Day14 {
  def solve(template: Map[String, Long], rules: Map[String, List[String]], start: Char, end: Char, steps: Int): Long = {
    val counts = insert(template, rules, steps).toList
      .flatMap{ case (s, count) => List((s.charAt(0), count), (s.charAt(1), count)) }
      .groupBy(_._1)
      .map{
        case (k, v) if k == start || k == end => (v.map(_._2).sum + 1) / 2
        case (_, v) => v.map(_._2).sum / 2
      }
    counts.max - counts.min
  }

  def main(args: Array[String]): Unit = {
    val (template, rules, start, end) = parseInput()
    println(solve(template, rules, start, end, 10))
    println(solve(template, rules, start, end, 40))
  }

  def parseInput(): (Map[String, Long], Map[String, List[String]], Char, Char) = {
    val input = getInput(14)

    val template = input.head.sliding(2).toList.groupBy(identity).map{ case (k, v) => (k, v.size.toLong) }
    val ruleRegex = new Regex("([A-Z])([A-Z]) -> ([A-Z])")
    val rules = input.drop(2).map{ case ruleRegex(a, b, c) => (s"$a$b", List(s"$a$c", s"$c$b")) }.toMap
    (template, new WithDefault(rules, List(_)), input.head.charAt(0), input.head.charAt(input.head.length-1))
  }

  @tailrec
  def insert(template: Map[String, Long], rules: Map[String, List[String]], n: Int): Map[String, Long] = {
    if (n <= 0) template
    else {
      val inserted = template.toList.flatMap { case (s, count) => rules(s).map((_, count)) }
        .groupBy(_._1).map{ case(s, counts) => (s, counts.map(_._2).sum) }
      insert(inserted, rules, n - 1)
    }
  }
}
