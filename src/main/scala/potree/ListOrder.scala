package potree

import cats.kernel.Order
import cats.kernel.Comparison

/*
 * The intention here is to be reminiscient of a hilbert curve
 * across all possible sets, such that if a set is ordered,
 * it can be compared to other sets using a partial order
 * with increased efficiency. Whenever a partial order would
 * compare two sets, this class should compare two sets and give
 * the same results. However, this class may give false positives.
 * See the test suite for examples that have false positives.
 *
 * The second use for this class is to generate a canonical representation
 * for a given set. This can be useful for caching.
 */
class ListOrder[A](
  ev: Order[A]
) extends Order[List[A]] {
  import scala.annotation.tailrec

  implicit val order = ev

  import cats.syntax.order._

  def compare(a: List[A], b: List[A]): Int =
    compareRecursive(a, b, 0)

  @tailrec
  private def compareRecursive(a: List[A], b: List[A], c: Int): Int = (a, b) match {
    case (Nil, Nil) => c
    case (Nil, r :: rs) => println(s"r: $r"); -1
    case (l :: ls, Nil) => println(s"l: $l"); 1
    case (l :: ls, r :: rs) => {
      println(s"l: $l, r: $r")
      if (l < r) {
        compareRecursive(ls, r :: rs, 1)
      } else if (l > r) {
        compareRecursive(l :: ls, rs, -1)
      } else {
        compareRecursive(ls, rs, c)
      }
    }
  }
}
