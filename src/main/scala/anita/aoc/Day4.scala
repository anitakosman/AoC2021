package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec

object Day4 {
  def part1(): Int = {
    val (numbers, boards) = parseInput
    playGame(numbers, boards, _ => true, _ => true)
  }

  def part2(): Int = {
    val (numbers, boards) = parseInput
    playGame(numbers, boards, _.size == 1, board => !board(Set.empty[Int]))
  }

  def main(args: Array[String]): Unit ={
    println(part1())
    println(part2())
  }

  private def parseInput: (Array[Int], Set[Set[Set[Int]]]) = {
    val input = getInput(4)
    val numbers = input.head.split(",").map(_.toInt)
    val boards = input.drop(1).grouped(6).map(_.drop(1).map(_.trim().split(" +").map(_.toInt)))
    (numbers, boards.map(boardToWinPosibilities).toSet)
  }

  def boardToWinPosibilities(l: List[Array[Int]]): Set[Set[Int]] = {
    l.map(_.toSet).toSet ++ l.transpose.map(_.toSet).toSet
  }

  @tailrec
  def playGame(numbers: Array[Int], boards: Set[Set[Set[Int]]], canWin: Set[Set[Set[Int]]] => Boolean, filter: Set[Set[Int]] => Boolean): Int = {
    val board = if (canWin(boards)) boards.find(_(Set(numbers.head))) else None
    if (board.isDefined) return numbers.head * (board.get.flatten.sum - numbers.head)
    playGame(numbers.tail, boards.map(board => board.map(_ - numbers.head)).filter(filter), canWin, filter)
  }
}
