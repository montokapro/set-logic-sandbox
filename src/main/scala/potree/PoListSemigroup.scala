package potree

import cats.kernel.PartialOrder
import cats.kernel.Semigroup

// A will be a Tree here, in practice
class PoListSemigroup[A](
  po: PartialOrder[A]
) extends Semigroup[List[A]] {
  import cats.syntax.eq._

  def empty = List.empty

  def combine(l: List[A], r: List[A]): List[A] = {
    r.foldLeft(l)((fa, a) => fa.filterNot(po.gt(_, a))).union(
      l.foldLeft(r)((fa, a) => fa.filterNot(po.gteqv(_, a)))
    )
  }
}

// or(1, 2) -> or(and(1, -2), and(2, -1), and(1, 2))

// or(1, and(2, 3)) -> or(and(1, or(-2, -3)), and(2, 3, -1), and(1, 2, 3))

// or(1, 2a, 2b) -> or(1, and(2, or(a, b))) how do we achieve this? Factoring

// def join(a: List[A], b: List[A]): List[A] = {
//   a.foldLeft(b)((c, d) => c.filterNot(po.gt(_, d))).union(
//     b.foldLeft(a)((c, d) => c.filterNot(po.gteqv(_, d)))
//   )
// }
