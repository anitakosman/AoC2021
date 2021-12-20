package anita.aoc

import anita.aoc.Util._

import scala.annotation.tailrec

object Day20 {
  type Picture = Set[Coordinate]

  def part1(picture: Picture, enhancement: List[Boolean]): Int = {
    enhancePicture(picture, enhancement, 2, false).size
  }

  def part2(picture: Picture, enhancement: List[Boolean]): Int = {
    enhancePicture(picture, enhancement, 50, false).size
  }

  def main(args: Array[String]): Unit = {
    val (picture, enhancement) = parseInput()
    println(part1(picture, enhancement))
    println(part2(picture, enhancement))
  }

  def parseInput(): (Picture, List[Boolean]) = {
    val input = getInput(20)

    val enhancement = input.head.map(_ == '#').toList
    val picture = input.drop(2).zipWithIndex.flatMap { case (line, y) => line.zipWithIndex.map { case (c, x) => (Coordinate(x, y), c == '#') } }
      .filter(_._2).map(_._1).toSet

    (picture, enhancement)
  }

  @tailrec
  def enhancePicture(picture: Picture, enhancement: List[Boolean], n: Int, outsideFrameLit: Boolean): Picture = {
    if (n <= 0) picture
    else {
      val minX = picture.minBy(_.x).x
      val maxX = picture.maxBy(_.x).x
      val minY = picture.minBy(_.y).y
      val maxY = picture.maxBy(_.y).y

      val canvas = (for {
        x <- minX to maxX
        y <- minY to maxY
      } yield Coordinate(x, y)).toSet

      val frame = (for {
        x <- minX -1 to maxX + 1
      } yield Coordinate(x, minY - 1)).toSet ++ (for {
        x <- minX -1 to maxX + 1
      } yield Coordinate(x, maxY + 1)).toSet ++ (for {
        y <- minY to maxY
      } yield Coordinate(minX - 1, y)).toSet ++ (for {
        y <- minY to maxY
      } yield Coordinate(maxX + 1, y)).toSet

      val enhancedPicture = (canvas ++ frame).filter(center =>
        enhancement(Integer.parseInt(center.squareAround.map(c =>
          if (canvas.contains(c) && picture(c) || !canvas.contains(c) && outsideFrameLit) "1" else "0"
        ).mkString, 2))
      )

      enhancePicture(enhancedPicture, enhancement, n - 1, if(outsideFrameLit) enhancement.last else enhancement.head)
    }
  }
}
