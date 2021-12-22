package anita.aoc

import anita.aoc.Util.{IntExtension, getInput}

import scala.annotation.tailrec
import scala.collection.mutable.HashMap

object Day21 {
  def part1(player1: Int, player2: Int): Int = {
    playGame(player1, player2)
  }

  def part2(player1: Int, player2: Int): Long = {
    playQuantumGame(player1, player2)
  }

  def main(args: Array[String]): Unit = {
    val (player1, player2) = parseInput()
    println(part1(player1, player2))
    println(part2(player1, player2))
  }

  def parseInput(): (Int, Int) = {
    val input = getInput(21)

    (input.head.last.toString.toInt, input.last.last.toString.toInt)
  }

  def playGame(player1: Int, player2: Int): Int = {
    @tailrec
    def go(player1Space: Int, player1Score: Int, player2Space: Int, player2Score: Int, player1Turn: Boolean, die: Int, rolls: Int): Int ={
      if (player1Score >= 1000)
        player2Score * rolls
      else if (player2Score >= 1000)
        player1Score * rolls
      else {
        val result = (die until die + 3).sum myMod 10
        val end = die + 3 myMod 10
        if (player1Turn) {
          val newPLayer1Space = (player1Space + result) myMod 10
          go(newPLayer1Space, player1Score + newPLayer1Space, player2Space, player2Score, false, end, rolls + 3)
        }
        else {
          val newPLayer2Space = (player2Space + result) myMod 10
          go(player1Space, player1Score, newPLayer2Space, player2Score + newPLayer2Space, true, end, rolls + 3)
        }
      }
    }

    go(player1, 0, player2, 0, true, 1, 0);
  }

  def playQuantumGame(player1: Int, player2: Int): Long = {
    val cache = new HashMap[(Int, Int, Int, Int, Boolean), (Long, Long)]
    def go(player1Space: Int, player1Score: Int, player2Space: Int, player2Score: Int, player1Turn: Boolean): (Long, Long) = {
      if (cache.contains(player1Space, player1Score, player2Space, player2Score, player1Turn))
        cache(player1Space, player1Score, player2Space, player2Score, player1Turn)
      else if (player1Score >= 21)
        (1L,0L)
      else if (player2Score >= 21)
        (0L,1L)
      else {
        if (player1Turn) {
          val newSpaceWith3 = player1Space + 3 myMod 10
          val(wins1with3, wins2with3) = go(newSpaceWith3, player1Score + newSpaceWith3, player2Space, player2Score, false)
          val newSpaceWith4 = player1Space + 4 myMod 10
          val(wins1with4, wins2with4) = go(newSpaceWith4, player1Score + newSpaceWith4, player2Space, player2Score, false)
          val newSpaceWith5 = player1Space + 5 myMod 10
          val(wins1with5, wins2with5) = go(newSpaceWith5, player1Score + newSpaceWith5, player2Space, player2Score, false)
          val newSpaceWith6 = player1Space + 6 myMod 10
          val(wins1with6, wins2with6) = go(newSpaceWith6, player1Score + newSpaceWith6, player2Space, player2Score, false)
          val newSpaceWith7 = player1Space + 7 myMod 10
          val(wins1with7, wins2with7) = go(newSpaceWith7, player1Score + newSpaceWith7, player2Space, player2Score, false)
          val newSpaceWith8 = player1Space + 8 myMod 10
          val(wins1with8, wins2with8) = go(newSpaceWith8, player1Score + newSpaceWith8, player2Space, player2Score, false)
          val newSpaceWith9 = player1Space + 9 myMod 10
          val(wins1with9, wins2with9) = go(newSpaceWith9, player1Score + newSpaceWith9, player2Space, player2Score, false)
          val wins1 = wins1with3 + (3 * wins1with4) + (6 * wins1with5) + (7 * wins1with6) + (6 * wins1with7) + (3 * wins1with8) + wins1with9
          val wins2 = wins2with3 + (3 * wins2with4) + (6 * wins2with5) + (7 * wins2with6) + (6 * wins2with7) + (3 * wins2with8) + wins2with9
          cache.put((player1Space, player1Score, player2Space, player2Score, player1Turn),(wins1, wins2))
          (wins1, wins2)
        } else {
          val newSpaceWith3 = player2Space + 3 myMod 10
          val (wins1with3, wins2with3) = go(player1Space, player1Score, newSpaceWith3, player2Score + newSpaceWith3, true)
          val newSpaceWith4 = player2Space + 4 myMod 10
          val (wins1with4, wins2with4) = go(player1Space, player1Score, newSpaceWith4, player2Score + newSpaceWith4, true)
          val newSpaceWith5 = player2Space + 5 myMod 10
          val (wins1with5, wins2with5) = go(player1Space, player1Score, newSpaceWith5, player2Score + newSpaceWith5, true)
          val newSpaceWith6 = player2Space + 6 myMod 10
          val (wins1with6, wins2with6) = go(player1Space, player1Score, newSpaceWith6, player2Score + newSpaceWith6, true)
          val newSpaceWith7 = player2Space + 7 myMod 10
          val (wins1with7, wins2with7) = go(player1Space, player1Score, newSpaceWith7, player2Score + newSpaceWith7, true)
          val newSpaceWith8 = player2Space + 8 myMod 10
          val (wins1with8, wins2with8) = go(player1Space, player1Score, newSpaceWith8, player2Score + newSpaceWith8, true)
          val newSpaceWith9 = player2Space + 9 myMod 10
          val (wins1with9, wins2with9) = go(player1Space, player1Score, newSpaceWith9, player2Score + newSpaceWith9, true)
          val wins1 = wins1with3 + (3 * wins1with4) + (6 * wins1with5) + (7 * wins1with6) + (6 * wins1with7) + (3 * wins1with8) + wins1with9
          val wins2 = wins2with3 + (3 * wins2with4) + (6 * wins2with5) + (7 * wins2with6) + (6 * wins2with7) + (3 * wins2with8) + wins2with9
          cache.put((player1Space, player1Score, player2Space, player2Score, player1Turn),(wins1, wins2))
          (wins1, wins2)
        }
      }
    }

    val (a,b) = go(player1, 0, player2, 0, true)
    math.max(a,b)
  }
}
