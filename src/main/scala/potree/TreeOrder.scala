package potree

import cats.kernel.Order
import cats.kernel.Comparison

// This ordering is an upper bound on partial ordering
class TreeOrderInstances[A](
  ev: Order[A]
) {
  private def treeListO = new ListOrder(Order.reverse(treeO))

  val leafO: Order[Leaf[A]] =
    Order.by((leaf: Leaf[A]) => leaf.value)(ev)

  val orO: Order[Or[A]] =
    new Order[Or[A]] {
      def compare(a: Or[A], b: Or[A]): Int =
        treeListO.compare(a.values, b.values)
    }

  val andO: Order[And[A]] =
    new Order[And[A]] {
      def compare(a: And[A], b: And[A]): Int =
        treeListO.compare(a.values, b.values)
    }

  val treeO: Order[Tree[A]] =
    new Order[Tree[A]] {
      def compare(x: Tree[A], y: Tree[A]): Int =
        (x, y) match {
          case (l: Leaf[A], r: Leaf[A]) =>
            leafO.compare(l, r)
          case (l: ToOr, r: ToOr) =>
            orO.compare(Or.create(l), Or.create(r))
          case (l: ToAnd, r: ToAnd) =>
            andO.compare(And.create(l), And.create(r))
          case (l: ToOr, r: ToAnd) => -1
          case (l: ToAnd, r: ToOr) => 1
        }
    }
}
