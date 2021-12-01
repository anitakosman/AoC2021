package anita.aoc

object Util {
  def zipWithNext[A](l: List[A]): List[(A,A)] = l zip l.tail
}
