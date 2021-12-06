package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec

object Day4 {
  type Board = Set[Set[Int]]

  def part1(numbers: Array[Int], boards: Set[Board]): Int = {
    playGame(numbers, boards, _ => true, _ => true)
  }

  def part2(numbers: Array[Int], boards: Set[Board]): Int = {
    playGame(numbers, boards, _.size == 1, board => !board(Set.empty[Int]))
  }

  def main(args: Array[String]): Unit ={
    val (numbers, boards) = parseInput
    println(part1(numbers, boards))
    println(part2(numbers, boards))
  }

  def parseInput: (Array[Int], Set[Board]) = {
    val input = getInput(4)
    val numbers = input.head.split(",").map(_.toInt)
    val boards = input.drop(1).grouped(6).map(_.drop(1).map(_.trim().split(" +").map(_.toInt)))
    (numbers, boards.map(boardToWinPosibilities).toSet)
  }

  def boardToWinPosibilities(l: List[Array[Int]]): Board = {
    l.map(_.toSet).toSet ++ l.transpose.map(_.toSet).toSet
  }

  @tailrec
  def playGame(numbers: Array[Int], boards: Set[Board], mayWin: Set[Board] => Boolean, filter: Board => Boolean): Int = {
    val board = if (mayWin(boards)) boards.find(_(Set(numbers.head))) else None
    if (board.isDefined) numbers.head * (board.get.flatten.sum - numbers.head)
    else playGame(numbers.tail, boards.map(board => board.map(_ - numbers.head)).filter(filter), mayWin, filter)
  }
}
