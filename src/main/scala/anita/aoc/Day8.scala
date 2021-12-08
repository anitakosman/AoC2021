package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day8 {
  def part1(input: List[(List[String], Array[String])]): Int = {
    input.map { case (_, outputValues) => outputValues.count(s => List(2,3,4,7).contains(s.length)) }.sum
  }

  def part2(input: List[(List[String], Array[String])]): Int = {
    input.map { case (signalPatterns, outputValues) => readOutput(signalPatterns, outputValues) }.sum
  }

  def main(args: Array[String]): Unit ={
    val input = parseInput()
    println(part1(input))
    println(part2(input))
  }

  def parseInput():  List[(List[String], Array[String])] = {
    val input = getInput(8)
    val lineRegex = new Regex("(.*) \\| (.*)")
    input.map{ case lineRegex(signalPatterns, outputValue) => (signalPatterns.split(" ").toList, outputValue.split(" "))}
  }

  def readOutput(signalPatterns: List[String], outputValues: Array[String]): Int = {
    val decoder = decipher(signalPatterns)
    decode(outputValues, decoder)
  }

  def decipher(signalPatterns: List[String]): Map[String, String] = {
    @tailrec
    def go(patterns: List[String], decoder: Map[String, String]): Map[String, String] = patterns.length match {
      case 10 => val one = patterns.find(_.length == 2).get; go(patterns.filter(_ != one), decoder + (("1", one.sorted)))
      case 9 => val seven = patterns.find(_.length == 3).get; go(patterns.filter(_ != seven), decoder + (("7", seven.sorted)))
      case 8 => val four = patterns.find(_.length == 4).get; go(patterns.filter(_ != four), decoder + (("4", four.sorted)))
      case 7 => val eight = patterns.find(_.length == 7).get; go(patterns.filter(_ != eight), decoder + (("8", eight.sorted)))
      case 6 => val nine = patterns.find(p => p.length == 6 && p.count(c => decoder("4").contains(c)) == 4).get; go(patterns.filter(_ != nine), decoder + (("9", nine.sorted)))
      case 5 => val zero = patterns.find(p => p.length == 6 && p.count(c => decoder("1").contains(c)) == 2).get; go(patterns.filter(_ != zero), decoder + (("0", zero.sorted)))
      case 4 => val six = patterns.find(_.length == 6).get; go(patterns.filter(_ != six), decoder + (("6", six.sorted)))
      case 3 => val three = patterns.find(_.count(c => decoder("1").contains(c)) == 2).get; go(patterns.filter(_ != three), decoder + (("3", three.sorted)))
      case 2 => val five = patterns.find(_.count(c => decoder("6").contains(c)) == 5).get; go(patterns.filter(_ != five), decoder + (("5", five.sorted)))
      case 1 => decoder + (("2", patterns.head.sorted))
    }

    go(signalPatterns, Map.empty).map{ case (k, v) => (v, k) }
  }

  def decode(outputValues: Array[String], decoder: Map[String, String]): Int = {
    outputValues.map(v => decoder(v.sorted)).mkString.toInt
  }
}