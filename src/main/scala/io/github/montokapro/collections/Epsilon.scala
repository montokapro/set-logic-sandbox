package io.github.montokapro.collections

import cats.CommutativeMonad
import scala.annotation.tailrec
import cats.{Applicative, Traverse}
import cats.Functor
import cats.kernel.Eq
import cats.kernel.PartialOrder
import algebra.lattice.Lattice

object Epsilon {
  class EqPartialOrder[A](eq: Eq[A]) extends PartialOrder[A] {
    def partialCompare(x: A, y: A): Double =
      if (eq.eqv(x, y)) 0.0 else Double.NaN
  }

  sealed abstract trait Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  sealed abstract trait Branch[A] extends Tree[A] {
    def values: Set[Tree[A]]
  }

  final case class Or[A](values: Set[Tree[A]]) extends Branch[A] {
    def flatten: Or[A] = Or(Or.flatten(values))
  }

  final case class And[A](values: Set[Tree[A]]) extends Branch[A] {
    def flatten: And[A] = And(And.flatten(values))
  }

  object Tree {
    def leaf[A](value: A): Tree[A] = Leaf(value)
    def or[A](values: Set[Tree[A]]): Tree[A] = Or(values)
    def and[A](values: Set[Tree[A]]): Tree[A] = And(values)

    implicit val functor: Functor[Tree] =
      new Functor[Tree] {
        def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
          tree match {
            case Or(values) =>
              Or(values.map(map(_)(f)))
            case And(values) =>
              And(values.map(map(_)(f)))
            case Leaf(value) =>
              Leaf(f(value))
          }
      }

    // Assumes x and y are reduced
    implicit def partialOrder[A]: PartialOrder[Tree[A]] =
      new PartialOrder[Tree[A]] {
        def partialCompare(x: Tree[A], y: Tree[A]): Double =
          (x, y) match {
            case (lhs: Or[A], rhs: Or[A]) =>
              Or.partialOrder.partialCompare(lhs, rhs)
            case (lhs: And[A], rhs: And[A]) =>
              And.partialOrder.partialCompare(lhs, rhs)
            case (lhs: Or[A], rhs: Leaf[A]) =>
              Or.partialOrder.partialCompare(lhs, Or.create(rhs))
            case (lhs: And[A], rhs: Leaf[A]) =>
              And.partialOrder.partialCompare(lhs, And.create(rhs))
            case (lhs: Leaf[A], rhs: Or[A]) =>
              Or.partialOrder.partialCompare(Or.create(lhs), rhs)
            case (lhs: Leaf[A], rhs: And[A]) =>
              And.partialOrder.partialCompare(And.create(lhs), rhs)
            case (lhs: Leaf[A], rhs: Leaf[A]) =>
              Leaf.partialOrder.partialCompare(lhs, rhs)
            case _ =>
              Double.NaN
          }
      }

    def flatten[A](tree: Tree[A]): Tree[A] = {
      tree match {
        case (Or(values)) =>
          val set = Or.flatten(values).map(flatten)
          if (set.size == 1) {
            set.head
          } else {
            or(set)
          }
        case (And(values)) =>
          val set = And.flatten(values).map(flatten)
          if (set.size == 1) {
            set.head
          } else {
            and(set)
          }
        case (leaf: Leaf[A]) =>
          leaf
      }
    }


    def reduce[A](tree: Tree[A]): Tree[A] = {
      val lattice = new TreeLattice[A]
      import lattice._

      tree match {
        case (Or(values)) =>
          val set: Set[Tree[A]] = Or.create( // .create is no-op
            joinSemilattice.combineAllOption(Or.flatten(values.map(reduce)))
              .getOrElse(or(Set.empty[Tree[A]]))
          ).values.map(flatten)
          if (set.size == 1) {
            set.head
          } else {
            or(set)
          }
        case (And(values)) =>
          val set: Set[Tree[A]] = And.create( // .create is no-op
            meetSemilattice.combineAllOption(And.flatten(values.map(reduce)))
              .getOrElse(and(Set.empty[Tree[A]]))
          ).values.map(flatten)
          if (set.size == 1) {
            set.head
          } else {
            and(set)
          }
        case (leaf: Leaf[A]) =>
          leaf
      }
    }
  }

  object Leaf {
    implicit def partialOrder[A]: PartialOrder[Leaf[A]] =
      PartialOrder.by((leaf: Leaf[A]) => leaf.value)(new EqPartialOrder(Eq.fromUniversalEquals))
  }

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

    def flatten[A](tree: Or[A]): Or[A] =
      Or(flatten(tree.values))

    implicit def partialOrder[A]: PartialOrder[Or[A]] =
      new PartialOrder[Or[A]] {
        def partialCompare(a: Or[A], b: Or[A]): Double = {
          def reduce(fa: Set[Tree[A]], a: Tree[A]): Set[Tree[A]] =
            fa.filterNot(Tree.partialOrder.gteqv(_, a))

          (b.values.foldLeft(a.values)(reduce).isEmpty, a.values.foldLeft(b.values)(reduce).isEmpty) match {
            case (true, true) => 0.0
            case (false, false) => Double.NaN
            case (true, false) => -1.0
            case (false, true) => 1.0
          }
        }
      }
  }

  object And {
    def create[A]: Tree[A] => And[A] = {
      case and@And(_) => and
      case tree => And(Set(tree))
    }

    // This is correct for properties but incorrect for ids
    // this does set union and not intersection
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

    def flatten[A](tree: And[A]): And[A] =
      And(flatten(tree.values))

    implicit def partialOrder[A]: PartialOrder[And[A]] =
      new PartialOrder[And[A]] {
        def partialCompare(a: And[A], b: And[A]): Double = {
          def reduce(fa: Set[Tree[A]], a: Tree[A]): Set[Tree[A]] =
            fa.filterNot(Tree.partialOrder.gteqv(_, a))

          (b.values.foldLeft(a.values)(reduce).isEmpty, a.values.foldLeft(b.values)(reduce).isEmpty) match {
            case (true, true) => 0.0
            case (false, false) => Double.NaN
            case (true, false) => -1.0
            case (false, true) => 1.0
          }
        }
      }
  }

  // First pass before a foldable lattice
  class DeepSetLattice[A](
    partialOrder: PartialOrder[A]
  ) extends Lattice[Set[A]] {
    private def reduce(po: PartialOrder[A])(fa: Set[A], a: A): Set[A] =
      fa.filter(po.lteqv(_, a))

    def join(a: Set[A], b: Set[A]): Set[A] = {
      def reduce(po: PartialOrder[A])(fa: Set[A], a: A): Set[A] =
        fa.filterNot(po.gt(_, a))

      def infininum = reduce(partialOrder) _

      a.foldLeft(b)(infininum) | b.foldLeft(a)(infininum)
    }

    def meet(a: Set[A], b: Set[A]): Set[A] = {
      def reduce(po: PartialOrder[A])(fa: Set[A], a: A): Set[A] =
        fa.filter(po.lteqv(_, a))

      def supremum = reduce(partialOrder) _

      a.foldLeft(b)(supremum) | b.foldLeft(a)(supremum)
    }
  }

  class TreeLattice[A] extends Lattice[Tree[A]] {
    private val joinLattice = new DeepSetLattice(Tree.partialOrder[A])
    def join(a: Tree[A], b: Tree[A]): Tree[A] = {
      Or(
        joinLattice.join(
          Or.create(a).flatten.values.map(And.create(_).flatten),
          Or.create(b).flatten.values.map(And.create(_).flatten)
        ).map(And.create)
      )
    }

    // This is correct for properties but incorrect for ids
    // this does set union and not intersection
    private val meetLattice = new DeepSetLattice(PartialOrder.reverse(Tree.partialOrder[A]))
    def meet(a: Tree[A], b: Tree[A]): Tree[A] = {
      And(
        meetLattice.join(
          And.create(a).flatten.values.map(Or.create(_).flatten),
          And.create(b).flatten.values.map(Or.create(_).flatten)
        ).map(Or.create)
      )
    }
  }
}
