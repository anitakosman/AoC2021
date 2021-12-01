package anita.aoc

object Util {
  def zipWithNext[A](depths: List[A]): List[(A,A)] = depths match {
    case a :: b :: tail => (a, b) :: zipWithNext(b :: tail)
    case _ => Nil
  }

  def zipWithNextTwo[A](depths: List[A]): List[(A,A,A)] = depths match {
    case a :: b :: c :: tail => (a, b, c) :: zipWithNextTwo(b :: c :: tail)
    case _ => Nil
  }
}
