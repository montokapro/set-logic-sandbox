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
    import evidence._

    // private def leafOr(lhs: Leaf[A], rhs: Leaf[A]): Tree[A] = (lhs, rhs) match {
    //   case (Pos(a), Neg(b)) if eqv(a, b) => Or(setLattice.zero)
    //   case (a, b) => Or(Set(a, b))
    // }

    // private def leafAnd(lhs: Leaf[A], rhs: Leaf[A]): Tree[A] = (lhs, rhs) match {
    //   case (Pos(a), Neg(b)) if eqv(a, b) => And(setLattice.zero)
    //   case (a, b) => And(Set(a, b))
    // }

    private def treeOr(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
      (lhs, rhs) match {
        case (Pos(a), Neg(b)) if eqv(a, b) => Or(setLattice.zero)
        case (Or(a), Or(b)) => Or(setLattice.or(a, b))
        case (Or(a), b) => Or(setLattice.or(a, Set(b)))
        case (a, Or(b)) => Or(setLattice.or(Set(a), b))
        case (a, b) => Or(setLattice.or(Set(a), Set(b)))
      }
    }

    def zero: Tree[A] = Or(setLattice.zero) // Everything matches
    def one: Tree[A] = And(setLattice.zero) // Nothing matches

    def join(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
      val set: Set[Tree[A]] = (lhs, rhs) match {
        case (Neg(a), Pos(b)) if eqv(a, b) => setLattice.zero
        case (Pos(a), Neg(b)) if eqv(a, b) => setLattice.zero
        case (Or(a), Or(b)) => setLattice.or(a, b)
        case (Or(a), b) => setLattice.or(a, Set(b))
        case (a, Or(b)) => setLattice.or(Set(a), b)
        case (a, b) => setLattice.or(Set(a), Set(b))
      }

      // TODO - more efficient size predicate
      if (set.size == 1) {
        set.head
      } else {
        Or(set)
      }
    }

    def meet(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
      val set: Set[Tree[A]] = (lhs, rhs) match {
        case (Neg(a), Pos(b)) if eqv(a, b) => setLattice.zero
        case (Pos(a), Neg(b)) if eqv(a, b) => setLattice.zero
        case (And(a), And(b)) => setLattice.and(a, b)
        case (And(a), b) => setLattice.and(a, Set(b))
        case (a, And(b)) => setLattice.and(Set(a), b)
        case (a, b) => setLattice.and(Set(a), Set(b))
      }

      // TODO - more efficient size predicate
      if (set.size == 1) {
        set.head
      } else {
        And(set)
      }
    }
  }
}
