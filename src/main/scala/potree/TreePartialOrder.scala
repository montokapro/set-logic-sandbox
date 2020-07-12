package potree

import cats.kernel.PartialOrder

class TreePartialOrderInstances[A](
  ev: PartialOrder[A]
) {
  val leafPo: PartialOrder[Leaf[A]] =
    PartialOrder.by((leaf: Leaf[A]) => leaf.value)(ev)

  val orPo: PartialOrder[Or[A]] =
    new PartialOrder[Or[A]] {
      def partialCompare(a: Or[A], b: Or[A]): Double = {
        def reduce(fa: List[Tree[A]], a: Tree[A]): List[Tree[A]] =
          fa.filterNot(treePo.gteqv(_, a))

        def excludes(a: Or[A], b: Or[A]): Boolean =
          a.values.foldLeft(b.values)(reduce).isEmpty

        (excludes(b, a), excludes(a, b)) match {
          case (true, true) => 0.0
          case (false, false) => Double.NaN
          case (true, false) => -1.0
          case (false, true) => 1.0
        }
      }
    }

  val andPo: PartialOrder[And[A]] =
    new PartialOrder[And[A]] {
      def partialCompare(a: And[A], b: And[A]): Double = {
        def reduce(fa: List[Tree[A]], a: Tree[A]): List[Tree[A]] =
          fa.filterNot(treePo.gteqv(_, a))

        def excludes(a: And[A], b: And[A]): Boolean =
          a.values.foldLeft(b.values)(reduce).isEmpty

        (excludes(b, a), excludes(a, b)) match {
          case (true, true) => 0.0
          case (false, false) => Double.NaN
          case (true, false) => -1.0
          case (false, true) => 1.0
        }
      }
    }

  val treePo: PartialOrder[Tree[A]] =
    new PartialOrder[Tree[A]] {
      def partialCompare(x: Tree[A], y: Tree[A]): Double =
        (x, y) match {
          case (l: Leaf[A], r: Leaf[A]) =>
            leafPo.partialCompare(l, r)
          case (l: ToOr, r: ToOr) =>
            orPo.partialCompare(Or.create(l), Or.create(r))
          case (l: ToAnd, r: ToAnd) =>
            andPo.partialCompare(And.create(l), And.create(r))
          case _ =>
            Double.NaN
        }
    }
}
