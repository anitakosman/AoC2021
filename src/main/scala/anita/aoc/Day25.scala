package anita.aoc

import anita.aoc.Util.getInput

import scala.annotation.tailrec

object Day25 {
  trait SeaCucumber {
    val x: Int
    val y: Int

    def move: SeaCucumber
  }

  case class EastHerdSeaCucumber(override val x: Int, override val y: Int, val width: Int) extends SeaCucumber {
    override def move: SeaCucumber = if (x + 1 == width) EastHerdSeaCucumber(0, y, width) else EastHerdSeaCucumber(x + 1, y, width)

    override def equals(obj: Any): Boolean = obj match {
      case cucumber: SeaCucumber => x == cucumber.x && y == cucumber.y
      case _ => false
    }

    override def hashCode(): Int = 1000 * y + x
  }

  case class SouthHerdSeaCucumber(override val x: Int, override val y: Int, val height: Int) extends SeaCucumber {
    override def move: SeaCucumber = if (y + 1 == height) SouthHerdSeaCucumber(x, 0, height) else SouthHerdSeaCucumber(x, y + 1, height)

    override def equals(obj: Any): Boolean = obj match {
      case cucumber: SeaCucumber => x == cucumber.x && y == cucumber.y
      case _ => false
    }

    override def hashCode(): Int = 1000 * y + x
  }

  def part1(eastHerd: Set[SeaCucumber], southHerd: Set[SeaCucumber]): Int = {
    @tailrec
    def go(easts: Set[SeaCucumber], souths: Set[SeaCucumber], steps: Int): Int = {
      val allCucumbers = easts ++ souths
      val moveEast = easts.filter(cucumber => {!allCucumbers.contains(cucumber.move)})
      val movedEast = easts -- moveEast ++ moveEast.map(_.move)
      val allCucumbersAfterMoveEast = movedEast ++ souths
      val moveSouth = souths.filter(cucumber => {!allCucumbersAfterMoveEast.contains(cucumber.move)})
      val movedSouth = souths -- moveSouth ++ moveSouth.map(_.move)
      if (moveEast.isEmpty && moveSouth.isEmpty)
        steps
      else go(movedEast, movedSouth, steps + 1)
    }

    go(eastHerd, southHerd, 1)
  }

  def main(args: Array[String]): Unit = {
    val (eastHerd, southHerd) = parseInput()
    println(part1(eastHerd, southHerd))
  }

  def parseInput(): (Set[SeaCucumber], Set[SeaCucumber]) = {
    val input = getInput(25)
    val width = input.head.length
    val height = input.length
    val herds = input.zipWithIndex.flatMap { case (line, y) => line.zipWithIndex.map { case (c, x) => (c, (x, y)) } }
      .groupMap(_._1) { case (c, (x, y)) => if (c == '>') EastHerdSeaCucumber(x, y, width) else SouthHerdSeaCucumber(x, y, height) }
    (herds('>').toSet, herds('v').toSet)
  }
}
