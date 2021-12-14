package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec
import scala.collection.immutable.Map.WithDefault
import scala.util.matching.Regex

object Day14 {
  def part1(template: String, rules: Map[String, String]): Int = {
    val counts = insert(template, rules, 10).groupBy(identity).map{ case (k, v) => v.length }
    counts.max - counts.min
  }

  def part2(template: String, rules: Map[String, String]): Int = {
    ???
  }

  def main(args: Array[String]): Unit = {
    val (template, rules) = parseInput()
    println(part1(template, rules))
    println(part2(template, rules))
  }

  def parseInput(): (String, WithDefault[String, String]) = {
    val input = getInput(14)

    val template = input.head
    val ruleRegex = new Regex("([A-Z])([A-Z]) -> ([A-Z])")
    val rules = input.drop(2).map{ case ruleRegex(a, b, c) => (s"$a$b", s"$a$c$b") }.toMap
    (template, new WithDefault(rules, identity[String]))
  }

  @tailrec
  def insert(template: String, rules: Map[String, String], n: Int): String = {
    if (n <= 0) template
    else {
      val inserted = template.sliding(2).map(rules(_).dropRight(1)).mkString + template.takeRight(1)
      insert(inserted, rules, n - 1)
    }
  }
}
