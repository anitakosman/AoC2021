package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec

object Day18 {
  sealed trait SnailfishNumber {
    def magnitude: Int = this match {
      case Leaf(value) => value
      case Node(left, right) => 3 * left.magnitude + 2 * right.magnitude
    }

    def reduce: SnailfishNumber = this match {
      case Node(Node(Node(Node(Node(Leaf(_), Leaf(b)), c), d), e), right) => Node(Node(Node(Node(Leaf(0), c addLeft b), d), e), right).reduce
      case Node(Node(Node(Node(a, Node(Leaf(b), Leaf(c))), d), e), right) => Node(Node(Node(Node(a addRight b, Leaf(0)), d addLeft c), e), right).reduce
      case Node(Node(Node(a, Node(Node(Leaf(b), Leaf(c)), d)), e), right) => Node(Node(Node(a addRight b, Node(Leaf(0), d addLeft c)), e), right).reduce
      case Node(Node(Node(a, Node(b, Node(Leaf(c), Leaf(d)))), e), right) => Node(Node(Node(a, Node(b addRight c, Leaf(0))), e addLeft d), right).reduce
      case Node(Node(a, Node(Node(Node(Leaf(b), Leaf(c)), d), e)), right) => Node(Node(a addRight b, Node(Node(Leaf(0), d addLeft c), e)), right).reduce
      case Node(Node(a, Node(Node(b, Node(Leaf(c), Leaf(d))), e)), right) => Node(Node(a, Node(Node(b addRight c, Leaf(0)), e addLeft d)), right).reduce
      case Node(Node(a, Node(b, Node(Node(Leaf(c), Leaf(d)), e))), right) => Node(Node(a, Node(b addRight c, Node(Leaf(0), e addLeft d))), right).reduce
      case Node(Node(a, Node(b, Node(c, Node(Leaf(d), Leaf(e))))), right) => Node(Node(a, Node(b, Node(c addRight d, Leaf(0)))), right addLeft e).reduce
      case Node(left, Node(Node(Node(Node(Leaf(a), Leaf(b)), c), d), e)) => Node(left addRight a, Node(Node(Node(Leaf(0), c addLeft b), d), e)).reduce
      case Node(left, Node(Node(Node(a, Node(Leaf(b), Leaf(c))), d), e)) => Node(left, Node(Node(Node(a addRight b, Leaf(0)), d addLeft c), e)).reduce
      case Node(left, Node(Node(a, Node(Node(Leaf(b), Leaf(c)), d)), e)) => Node(left, Node(Node(a addRight b, Node(Leaf(0), d addLeft c)), e)).reduce
      case Node(left, Node(Node(a, Node(b, Node(Leaf(c), Leaf(d)))), e)) => Node(left, Node(Node(a, Node(b addRight c, Leaf(0))), e addLeft d)).reduce
      case Node(left, Node(a, Node(Node(Node(Leaf(b), Leaf(c)), d), e))) => Node(left, Node(a addRight b, Node(Node(Leaf(0), d addLeft c), e))).reduce
      case Node(left, Node(a, Node(Node(b, Node(Leaf(c), Leaf(d))), e))) => Node(left, Node(a, Node(Node(b addRight c, Leaf(0)), e addLeft d))).reduce
      case Node(left, Node(a, Node(b, Node(Node(Leaf(c), Leaf(d)), e)))) => Node(left, Node(a, Node(b addRight c, Node(Leaf(0), e addLeft d)))).reduce
      case Node(left, Node(a, Node(b, Node(c, Node(Leaf(d), Leaf(_)))))) => Node(left, Node(a, Node(b, Node(c addRight d, Leaf(0))))).reduce
      case x if x.exists(_ > 9) => this.split.reduce
      case _ => this
    }

    def split: SnailfishNumber = this match {
      case Leaf(n) if n > 9 => Node(Leaf(n / 2), Leaf((n + 1) / 2))
      case Leaf(_) => this
      case Node(left, right) if left.exists(_ > 9) => Node(left.split, right)
      case Node(left, right) => Node(left, right.split)
    }

    def addLeft(n: Int): SnailfishNumber = this match {
      case Leaf(value) => Leaf(value + n)
      case Node(left, right) => Node(left addLeft n, right)
    }

    def addRight(n: Int): SnailfishNumber = this match {
      case Leaf(value) => Leaf(value + n)
      case Node(left, right) => Node(left, right addRight n)
    }

    def exists(p: Int => Boolean): Boolean = this match {
      case Leaf(value) => p(value)
      case Node(left, right) => left.exists(p) || right.exists(p)
    }
  }

  case class Leaf(value: Int) extends SnailfishNumber

  case class Node(left: SnailfishNumber, right: SnailfishNumber) extends SnailfishNumber

  def part1(input: List[SnailfishNumber]): Int = {
    val sum = input.reduce((a, b) => reduceNumber(Node(a,b)))
    sum.magnitude
  }

  def part2(input: List[SnailfishNumber]): Int = {
    val sums = for {
      a <- input
      b <- input
      if (a != b)
    } yield reduceNumber(Node(a,b)).magnitude
    sums.max
  }

  def main(args: Array[String]): Unit = {
    val input = parseInput()
    println(part1(input))
    println(part2(input))
  }

  def parseInput(): List[SnailfishNumber] = {
    val input = getInput(18)

    input.map(toSnailfishNumber)
  }

  def toSnailfishNumber(s: String): SnailfishNumber = {
    parseSnailfishNumber(s)._1
  }

  def parseSnailfishNumber(s: String): (SnailfishNumber, String) = s.charAt(0) match {
    case '[' =>
      val (left, unparsed1) = parseSnailfishNumber(s.tail)
      val (right, unparsed2) = parseSnailfishNumber(unparsed1)
      (Node(left, right), unparsed2)
    case n => (Leaf(n.toString.toInt), s.tail.dropWhile(c => c == ']' || c == ','))
  }

  @tailrec
  def reduceNumber(snailfishNumber: SnailfishNumber): SnailfishNumber = snailfishNumber match {
      case Node(Node(Node(Node(Node(Leaf(_), Leaf(b)), c), d), e), right) => reduceNumber(Node(Node(Node(Node(Leaf(0), c addLeft b), d), e), right))
      case Node(Node(Node(Node(a, Node(Leaf(b), Leaf(c))), d), e), right) => reduceNumber(Node(Node(Node(Node(a addRight b, Leaf(0)), d addLeft c), e), right))
      case Node(Node(Node(a, Node(Node(Leaf(b), Leaf(c)), d)), e), right) => reduceNumber(Node(Node(Node(a addRight b, Node(Leaf(0), d addLeft c)), e), right))
      case Node(Node(Node(a, Node(b, Node(Leaf(c), Leaf(d)))), e), right) => reduceNumber(Node(Node(Node(a, Node(b addRight c, Leaf(0))), e addLeft d), right))
      case Node(Node(a, Node(Node(Node(Leaf(b), Leaf(c)), d), e)), right) => reduceNumber(Node(Node(a addRight b, Node(Node(Leaf(0), d addLeft c), e)), right))
      case Node(Node(a, Node(Node(b, Node(Leaf(c), Leaf(d))), e)), right) => reduceNumber(Node(Node(a, Node(Node(b addRight c, Leaf(0)), e addLeft d)), right))
      case Node(Node(a, Node(b, Node(Node(Leaf(c), Leaf(d)), e))), right) => reduceNumber(Node(Node(a, Node(b addRight c, Node(Leaf(0), e addLeft d))), right))
      case Node(Node(a, Node(b, Node(c, Node(Leaf(d), Leaf(e))))), right) => reduceNumber(Node(Node(a, Node(b, Node(c addRight d, Leaf(0)))), right addLeft e))
      case Node(left, Node(Node(Node(Node(Leaf(a), Leaf(b)), c), d), e)) => reduceNumber(Node(left addRight a, Node(Node(Node(Leaf(0), c addLeft b), d), e)))
      case Node(left, Node(Node(Node(a, Node(Leaf(b), Leaf(c))), d), e)) => reduceNumber(Node(left, Node(Node(Node(a addRight b, Leaf(0)), d addLeft c), e)))
      case Node(left, Node(Node(a, Node(Node(Leaf(b), Leaf(c)), d)), e)) => reduceNumber(Node(left, Node(Node(a addRight b, Node(Leaf(0), d addLeft c)), e)))
      case Node(left, Node(Node(a, Node(b, Node(Leaf(c), Leaf(d)))), e)) => reduceNumber(Node(left, Node(Node(a, Node(b addRight c, Leaf(0))), e addLeft d)))
      case Node(left, Node(a, Node(Node(Node(Leaf(b), Leaf(c)), d), e))) => reduceNumber(Node(left, Node(a addRight b, Node(Node(Leaf(0), d addLeft c), e))))
      case Node(left, Node(a, Node(Node(b, Node(Leaf(c), Leaf(d))), e))) => reduceNumber(Node(left, Node(a, Node(Node(b addRight c, Leaf(0)), e addLeft d))))
      case Node(left, Node(a, Node(b, Node(Node(Leaf(c), Leaf(d)), e)))) => reduceNumber(Node(left, Node(a, Node(b addRight c, Node(Leaf(0), e addLeft d)))))
      case Node(left, Node(a, Node(b, Node(c, Node(Leaf(d), Leaf(_)))))) => reduceNumber(Node(left, Node(a, Node(b, Node(c addRight d, Leaf(0))))))
      case x if x.exists(_ > 9) => reduceNumber(snailfishNumber.split)
      case _ => snailfishNumber
    }

}
