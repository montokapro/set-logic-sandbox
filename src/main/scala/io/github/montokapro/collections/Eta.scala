package io.github.montokapro.collections

import cats.Functor
import cats.kernel.PartialOrder
import algebra.lattice.Lattice
import scala.annotation.tailrec

object Eta {
  import scala.{specialized => sp}
  trait Inverse[@sp(Int, Long, Float, Double) A] extends Any {
    def inverse(a: A): A
  }

  object Inverse {
    def by[F[_], @sp A](
      implicit
      functor: Functor[F],
      ev: Inverse[A]
    ): Inverse[F[A]] =
      new Inverse[F[A]] {
        def inverse(a: F[A]): F[A] = {
          functor.map(a)(ev.inverse)
        }
      }
  }

  sealed abstract trait Dual[A] {
    def inverse: Dual[A]
  }

  case class Neg[A](value: A) extends Dual[A] {
    def inverse = Pos(value)
  }

  case class Pos[A](value: A) extends Dual[A] {
    def inverse = Neg(value)
  }

  class DualInverse[A] extends Inverse[Dual[A]] {
    def inverse(dual: Dual[A]): Dual[A] = dual.inverse
  }

  trait DualInstances[A] {
    implicit val inverseForDual: Inverse[Dual[A]] = new DualInverse

    implicit val functor: Functor[Dual] =
      new Functor[Dual] {
        def map[A, B](dual: Dual[A])(f: A => B): Dual[B] =
          dual match {
            case Neg(value) =>
              Neg(f(value))
            case Pos(value) =>
              Pos(f(value))
          }
      }
  }

  // Join and meet are two subtly different ways of finding common factors
  class PosetLattice[A](
    po: PartialOrder[A],
  ) extends Lattice[List[A]] {
    import cats.instances.list._

    def join(a: List[A], b: List[A]): List[A] = {
      a.foldLeft(b)((c, d) => c.filterNot(po.gt(_, d))).union(
        b.foldLeft(a)((c, d) => c.filterNot(po.gteqv(_, d)))
      )
    }

    def meet(a: List[A], b: List[A]): List[A] = {
      a.foldLeft(b)((c, d) => c.filter(po.lt(_, d))).union(
        b.foldLeft(a)((c, d) => c.filter(po.lteqv(_, d)))
      )
    }
  }

  // This is probably not a real group... run the laws suite
  import cats.kernel.Group
  import algebra.lattice.GenBool
  class PosetGroup[A](
    inverse: Inverse[A],
    bool: GenBool[A]
  ) extends Group[List[A]] with Inverse[List[A]] {
    import cats.instances.list._

    def inverse(a: List[A]): List[A] =
      Functor[List].map(a)(inverse.inverse)

    def empty: List[A] = List.empty

    // TODO: is this associative? Otherwise consider a loop or quasigroup.
    def combine(a: List[A], b: List[A]): List[A] = {
      a.foldLeft(b)((fc, c) => fc.map(bool.without(_, inverse.inverse(c))))
    }
  }

  // Using a Set has become prohibitive.
  // For now we use a list, which is admittedly inefficient.
  //
  // In the future, we could use a custom poset class:
  // https://hackage.haskell.org/package/pomaps-0.0.2.1/docs/Data-POSet.html
  //
  // However, performance degrades for large anti-chains,
  // which is a likely use case for a "linter" where reductions are rare
  //
  // Ideally, the new Poset class would accept two PartialOrders:
  // The first would define lattice behavior.
  // The second would allow an optimization by defining deterministic memory layout.
  //   (this could be an Order as well, if we wanted to enforce efficiency)
  import cats.kernel.Eq
  import algebra.lattice.GenBool
  class InversePosetLattice[A](
    inverse: Inverse[A],
    bool: GenBool[A],
    eq: Eq[A]
  ) extends Lattice[List[A]] {
    import cats.instances.list._

    private val po = bool.joinSemilattice.asJoinPartialOrder(eq)
    private val posetLattice = new PosetLattice(po)
    private val posetGroup = new PosetGroup(inverse, bool)

    def join(a: List[A], b: List[A]): List[A] = {
      posetLattice.join(
        posetGroup.combine(a, b),
        posetGroup.combine(b, a)
      )
    }

    def meet(a: List[A], b: List[A]): List[A] = {
      posetLattice.meet(
        posetGroup.combine(a, b),
        posetGroup.combine(b, a)
      )
    }
  }

  sealed abstract trait Tree[A]

  sealed abstract trait ToOr

  sealed abstract trait ToAnd

  final case class Leaf[A](value: A) extends Tree[A] with ToOr with ToAnd

  final case class Or[A](values: List[Tree[A]]) extends Tree[A] with ToOr

  final case class And[A](values: List[Tree[A]]) extends Tree[A] with ToAnd

  object Or {
    def create[A](a: Tree[A]) = a match {
      case or: Or[A] => or
      case tree => Or(List(tree))
    }
  }

  object And {
    def create[A](a: Tree[A]) = a match {
      case and: And[A] => and
      case tree => And(List(tree))
    }
  }

  class TreeInverse[A](
    ev: Inverse[A]
  ) extends Inverse[Tree[A]] {
    def inverse(tree: Tree[A]): Tree[A] = {
      tree match {
        case Or(values) => Or(values.map(inverse))
        case And(values) => And(values.map(inverse))
        case Leaf(value) => Leaf(ev.inverse(value))
      }
    }
  }

  // Assumes trees are in a normalized form; uses strict equality
  class TreeEq[A](
    ev: Eq[A]
  ) extends Eq[Tree[A]] {
    import cats.instances.list._
    import cats.implicits._

    private val listEq = new cats.kernel.instances.ListEq[Tree[A]]()(this)

    def eqv(x: Tree[A], y: Tree[A]): Boolean = (x, y) match {
      case (Or(l), Or(r)) => listEq.eqv(l, r)
      case (And(l), And(r)) => listEq.eqv(l, r)
      case (Leaf(l), Leaf(r)) => ev.eqv(l, r)
      case _ => false
    }
  }

  // Assumes trees are in a normalized form; uses strict equality
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
          // Consider:
          // def reduce(fa: List[Tree[A]], a: Tree[A]): List[Tree[A]] =
          //   fa.filter(treePo.lt(_, a))

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

  // Assumes trees are in a normalized form; uses strict equality
  import algebra.lattice.Bool
  class PoTreeBool[A](
    in: Inverse[Tree[A]],
    po: PartialOrder[Tree[A]]
  ) extends Bool[Tree[A]] {
    import cats.kernel.Comparison._

    private val lattice = new PosetLattice[Tree[A]](po)

    def zero = Or(List.empty) // And?

    def one = And(List.empty) // Or?

    def or(a: Tree[A], b: Tree[A]): Tree[A] = (a, b) match {
      case (l: ToOr, r: ToOr) => Or(lattice.join(
        Or.create(l).values, Or.create(r).values
      ))
      case (l, r) => And(lattice.meet(
        And.create(l).values, And.create(r).values
      ))
    }

    def and(a: Tree[A], b: Tree[A]): Tree[A] = (a, b) match {
      case (l: ToAnd, r: ToAnd) => And(lattice.meet(
        And.create(l).values, And.create(r).values
      ))
      case (l, r) => Or(lattice.join(
        Or.create(l).values, Or.create(r).values
      ))
    }

    def complement(a: Tree[A]): Tree[A] = in.inverse(a)
  }

  import algebra.lattice.Bool
  class InversePoTreeBool[A](
    in: Inverse[A],
    po: PartialOrder[A]
  ) extends Bool[Tree[A]] {
    import cats.kernel.Comparison._

    private val treeIn = new TreeInverse(in)
    private val treePo = new TreePartialOrderInstances(po).treePo
    private val bool = new PoTreeBool(treeIn, treePo)
    private val lattice = new InversePosetLattice[Tree[A]](treeIn, bool, treePo)

    def zero = bool.zero

    def one = bool.one

    def or(a: Tree[A], b: Tree[A]): Tree[A] = (a, b) match {
      case (l: ToOr, r: ToOr) => Or(lattice.join(
        Or.create(l).values, Or.create(r).values
      ))
      case (l, r) => And(lattice.meet(
        And.create(l).values, And.create(r).values
      ))
    }

    def and(a: Tree[A], b: Tree[A]): Tree[A] = (a, b) match {
      case (l: ToAnd, r: ToAnd) => And(lattice.meet(
        And.create(l).values, And.create(r).values
      ))
      case (l, r) => Or(lattice.join(
        Or.create(l).values, Or.create(r).values
      ))
    }

    def complement(a: Tree[A]): Tree[A] = treeIn.inverse(a)
  }
}
