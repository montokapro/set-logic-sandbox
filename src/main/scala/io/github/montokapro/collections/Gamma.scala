package io.github.montokapro.collections

import algebra.lattice.BoundedLattice
import algebra.lattice.Lattice
import algebra.lattice.GenBool
import cats.kernel.BoundedSemilattice
import cats.kernel.Eq
import cats.kernel.PartialOrder
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

  class TreeLattice[A](
    val evidence: Eq[A]
  ) extends BoundedLattice[Tree[A]] {
    private val setLattice = algebra.instances.set.setLattice[Tree[A]]
    private val setEq = cats.kernel.instances.set.catsKernelStdPartialOrderForSet[Tree[A]]

    import evidence._

    def one: Tree[A] = Or(setLattice.zero) // Everything matches
    def zero: Tree[A] = And(setLattice.zero) // Nothing matches

    def join(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
      def min(a: Set[Tree[A]], b: Set[Tree[A]]): Set[Tree[A]] =
        setLattice.joinPartialOrder(setEq)
          .pmin(a, b)
          .getOrElse(setLattice.or(a, b))

      val set: Set[Tree[A]] = (lhs, rhs) match {
        case (Neg(a), Pos(b)) if eqv(a, b) => setLattice.zero
        case (Pos(a), Neg(b)) if eqv(a, b) => setLattice.zero
        case (And(a), b) if (a.isEmpty) => Set(b)
        case (a, And(b)) if (b.isEmpty) => Set(a)
        case (Or(a), Or(b)) => min(a, b)
        case (Or(a), b) => min(a, Set(b))
        case (a, Or(b)) => min(Set(a), b)
        case (a, b) => min(Set(a), Set(b))
      }

      println(s"Join $lhs $rhs -> $set")

      // TODO - more efficient size predicate
      if (set.size == 1) {
        set.head
      } else {
        Or(set)
      }
    }

    def meet(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
      def max(a: Set[Tree[A]], b: Set[Tree[A]]): Set[Tree[A]] =
        setLattice.meetPartialOrder(setEq)
          .pmin(a, b)
          .getOrElse(setLattice.and(a, b))

      val set: Set[Tree[A]] = (lhs, rhs) match {
        case (Neg(a), Pos(b)) if eqv(a, b) => setLattice.zero
        case (Pos(a), Neg(b)) if eqv(a, b) => setLattice.zero
        case (Or(a), b) if (a.isEmpty) => Set(b)
        case (a, Or(b)) if (b.isEmpty) => Set(a)
        case (And(a), And(b)) => max(a, b)
        case (And(a), b) => max(a, Set(b))
        case (a, And(b)) => max(Set(a), b)
        case (a, b) => max(Set(a), Set(b))
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
