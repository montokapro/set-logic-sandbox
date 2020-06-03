package io.github.montokapro.collections

import algebra.lattice.BoundedLattice
import algebra.lattice.Lattice
import algebra.lattice.GenBool
import cats.kernel.BoundedSemilattice
import cats.kernel.Eq
import cats.kernel.PartialOrder
import cats.kernel.Semilattice
import scala.annotation.tailrec

object Gamma {
  abstract trait Dual[A]

  abstract trait Tree[A]

  abstract trait Leaf[A] extends Tree[A] with Dual[A] {
    def value: A
  }

  abstract trait Branch[A] extends Tree[A] with Dual[A] {
    def values: Set[Tree[A]]
  }

  case class Neg[A](value: A) extends Leaf[A]

  case class Pos[A](value: A) extends Leaf[A]

  case class Or[A](values: Set[Tree[A]]) extends Branch[A]

  case class And[A](values: Set[Tree[A]]) extends Branch[A]

  object Or {
    def create[A]: Tree[A] => Or[A] = {
      case or@Or(_) => or
      case tree => Or(Set(tree))
    }

    def flatten[A](values: Set[Tree[A]]): Set[Tree[A]] = {
      @tailrec
      def loop(
        open: List[Tree[A]],
        closed: Set[Tree[A]]): Set[Tree[A]] =
        open match {
          case Or(values) :: next =>
            loop(values.toList ++ next, closed)
          case head :: next =>
            loop(next, closed + head)
          case Nil =>
            closed
        }
      loop(values.toList, Set.empty)
    }
  }

  object And {
    def create[A]: Tree[A] => And[A] = {
      case and@And(_) => and
      case tree => And(Set(tree))
    }

    def flatten[A](values: Set[Tree[A]]): Set[Tree[A]] = {
      @tailrec
      def loop(
        open: List[Tree[A]],
        closed: Set[Tree[A]]): Set[Tree[A]] =
        open match {
          case And(values) :: next =>
            loop(values.toList ++ next, closed)
          case head :: next =>
            loop(next, closed + head)
          case Nil =>
            closed
        }
      loop(values.toList, Set.empty)
    }
  }

  object Tree {
    def neg[A](value: A): Tree[A] = Neg(value)
    def pos[A](value: A): Tree[A] = Pos(value)
    def or[A](values: Set[Tree[A]]): Tree[A] = Or(values)
    def and[A](values: Set[Tree[A]]): Tree[A] = And(values)
  }

  // TODO: compose this with toSet functions
  private def compress[A](a: A, b: A)(
    implicit
    semilattice: Semilattice[A],
    partialOrder: PartialOrder[A]
  ): A = {
    partialOrder.pmin(a, b).getOrElse(semilattice.combine(a, b))
  }

  class BranchLattice[A](
    val evidence: Eq[A]
  ) extends BoundedLattice[Tree[A]] {
    private val setLattice = algebra.instances.set.setLattice[Tree[A]]
    private val setEq = cats.kernel.instances.set.catsKernelStdPartialOrderForSet[Tree[A]]

    private def compress[A](a: A, b: A)(
      implicit
      semilattice: Semilattice[A],
      partialOrder: PartialOrder[A]
    ): A = {
      partialOrder.pmin(a, b).getOrElse(semilattice.combine(a, b))
    }

    import evidence._

    def one: Tree[A] = And(setLattice.zero) // Everything matches
    def zero: Tree[A] = Or(setLattice.zero) // Nothing matches

    def join(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
      implicit val semilattice = setLattice.joinSemilattice
      implicit val partialOrder = setLattice.meetSemilattice.asJoinPartialOrder(setEq)

      Or(compress(And.create(lhs).values, And.create(rhs).values))
    }

    def meet(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
      implicit val semilattice = setLattice.joinSemilattice
      implicit val partialOrder = setLattice.joinSemilattice.asJoinPartialOrder(setEq)

      And(compress(Or.create(lhs).values, Or.create(rhs).values))
    }
  }

  class OrLattice[A](
    val evidence: Eq[A]
  ) extends GenBool[Or[A]] {
    private val setLattice = algebra.instances.set.setLattice[Tree[A]]
    private val setEq = cats.kernel.instances.set.catsKernelStdPartialOrderForSet[Tree[A]]

    import evidence._

    def zero: Or[A] = Or(setLattice.zero)

    def or(lhs: Or[A], rhs: Or[A]): Or[A] = {
      implicit val semilattice = setLattice.meetSemilattice
      implicit val partialOrder = setLattice.meetPartialOrder(setEq)

      Or(compress(lhs.values, rhs.values))
    }

    def and(lhs: Or[A], rhs: Or[A]): Or[A] = {
      implicit val semilattice = setLattice.joinSemilattice
      implicit val partialOrder = setLattice.joinPartialOrder(setEq)

      Or(compress(lhs.values, rhs.values))
    }

    def without(lhs: Or[A], rhs: Or[A]): Or[A] = ???
  }

  class AndLattice[A](
    val evidence: Eq[A]
  ) extends GenBool[And[A]] {
    private val setLattice = algebra.instances.set.setLattice[Tree[A]]
    private val setEq = cats.kernel.instances.set.catsKernelStdPartialOrderForSet[Tree[A]]

    import evidence._

    def zero: And[A] = And(setLattice.zero)

    def or(lhs: And[A], rhs: And[A]): And[A] = {
      implicit val semilattice = setLattice.joinSemilattice
      implicit val partialOrder = setLattice.joinPartialOrder(setEq)

      And(compress(lhs.values, rhs.values))
    }

    def and(lhs: And[A], rhs: And[A]): And[A] = {
      implicit val semilattice = setLattice.meetSemilattice
      implicit val partialOrder = setLattice.meetPartialOrder(setEq)

      And(compress(lhs.values, rhs.values))
    }

    def without(lhs: And[A], rhs: And[A]): And[A] = ???
  }

  class TreeLattice[A](
    val evidence: Eq[A]
  ) extends BoundedLattice[Tree[A]] {
    private val setLattice = algebra.instances.set.setLattice[Tree[A]]
    private val setEq = cats.kernel.instances.set.catsKernelStdPartialOrderForSet[Tree[A]]

    import evidence._

    def one: Tree[A] = Or(setLattice.zero) // Everything matches
    def zero: Tree[A] = And(setLattice.zero) // Nothing matches

    def join(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
      implicit val lattice = new OrLattice(evidence)

      val or = lattice.join(Or.create(lhs), Or.create(rhs))

      println(s"Join $lhs $rhs -> $or")

      // TODO - more efficient size predicate
      if (or.values.size == 1) {
        or.values.head
      } else {
        or
      }
    }

    def meet(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
      implicit val semilattice = setLattice.meetSemilattice
      implicit val partialOrder = setLattice.meetPartialOrder(setEq)

      val set: Set[Tree[A]] = (lhs, rhs) match {
        case (Neg(a), Pos(b)) if eqv(a, b) => setLattice.zero
        case (Pos(a), Neg(b)) if eqv(a, b) => setLattice.zero
        case (Or(a), b) if (a.isEmpty) => Set(b)
        case (a, Or(b)) if (b.isEmpty) => Set(a)
        case (And(a), And(b)) => compress(a, b)
        case (And(a), b) => compress(a, Set(b))
        case (a, And(b)) => compress(Set(a), b)
        case (a, b) => compress(Set(a), Set(b))
      }

      println(s"Meet $lhs $rhs -> $set")

      // TODO - more efficient size predicate
      if (set.size == 1) {
        set.head
      } else {
        And(set)
      }
    }
  }
}
