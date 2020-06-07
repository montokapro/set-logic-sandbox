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
        // import cats.instances.set._

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
            case (lhs: Leaf[A], rhs: Leaf[A]) =>
              Leaf.partialOrder.partialCompare(lhs, rhs)
            case _ =>
              Double.NaN
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
      PartialOrder.by((or: Or[A]) => or.values)(algebra.instances.set.catsKernelStdPartialOrderForSet[Tree[A]])
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

    def flatten[A](tree: And[A]): And[A] =
      And(flatten(tree.values))

    implicit def partialOrder[A]: PartialOrder[And[A]] =
      PartialOrder.reverse(PartialOrder.by((and: And[A]) => and.values)(algebra.instances.set.catsKernelStdPartialOrderForSet[Tree[A]]))
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
    private val deepSetLattice = new DeepSetLattice(Tree.partialOrder[A])

    def join(a: Tree[A], b: Tree[A]): Tree[A] = {
      Or(
        deepSetLattice.join(
          Or.create(a).flatten.values.map(And.create(_).flatten),
          Or.create(b).flatten.values.map(And.create(_).flatten)
        ).map(And.create)
      )
    }

    def meet(a: Tree[A], b: Tree[A]): Tree[A] = {
      And(
        deepSetLattice.join(
          And.create(a).flatten.values.map(Or.create(_).flatten),
          And.create(b).flatten.values.map(Or.create(_).flatten)
        ).map(Or.create)
      )
    }
  }

// From algebra:
// class SetBoolRng[A] extends BoolRng[Set[A]] {
//   def zero: Set[A] = Set.empty
//   def plus(x: Set[A], y: Set[A]): Set[A] = (x -- y) | (y -- x) // this is xor - Hey! this looks a lot like the reduce code we wrote
//   def times(x: Set[A], y: Set[A]): Set[A] = x & y
// }

  // class NestedLattice[F[_], A](
  //   implicit
  //   // evidence: UnorderedFoldable[F[_]],
  //   outer: BoundedSemilattice[F[A]],
  //   inner: Lattice[A]
  // ) extends BoundedLattice[F[A]] {
  //   import cats.syntax.partialOrder._
  //   import cats.syntax.unorderedFoldable._

  //   private def reduce(acc: F[A], a: A): F[A] = acc.filterNot(_ < a)

  //   private def compress[A](a: A, b: A)(
  //     implicit
  //     semilattice: Semilattice[A],
  //     partialOrder: PartialOrder[A]
  //   ): A = {
  //     partialOrder.pmin(a, b).getOrElse(semilattice.combine(a, b))
  //   }


  //   (
  //     (1 2 3)
  //     (2 4)
  //   )
  //   (
  //     (2 3 4)
  //     (1 3)
  //   )

  //   def meet(a: Set[Set[Int]], b: Set[Set[Int]]): Set[Set[Int]] = {
  //     def reduce(acc: Set[Set[Int]], set: Set[Int]) = acc.filterNot(_ < set)

  //     outer.combineAll(compress)
  //   }

  //   def join(a: Set[Set[Int]], b: Set[Set[Int]]): Set[Set[Int]] = {
  //     def reduce(acc: Set[Set[Int]], set: Set[Int]) = acc.filterNot(_ > set)

  //     a.foldLeft(b)(reduce) | b.foldLeft(a)(reduce) // This is xor!!!
  //   }


  //   def join(lhs: F[A], rhs: F[A]): F[A]
  //     val semilattice = outer.joinSemilattice
  //     val partialOrder = inner.joinSemilattice.asJoinPartialOrder(setEq)

  //     val values = outer.combine(lhs.foldLeft(rhs)(reduce), rhs.foldLeft(lhs)(reduce))
  //     inner.combineAll(values)

  //     implicit val partialOrder = setLattice.meetSemilattice.asJoinPartialOrder(setEq)

  //     partialOrder.pmin(a, b).getOrElse(semilattice.combine(lhs, rhs))
  //   }

  //   def meet(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
  //     val partialOrder = inner.meetSemilattice.asJoinPartialOrder(setEq)

  //     And(compress(Or.create(lhs).values, Or.create(rhs).values))
  //   }
  // }

  // class TreeLattice[A](
  //   implicit
  //   lattice: Lattice[A]
  // ) extends BoundedLattice[Tree[A]] {
  //   // Set(1, 2), Set(Set(1) -> Set(1)
  //   // Set(1, 2), Set(3) -> Set(1, 2), Set(3)

  //   // (or
  //   //   (and
  //   //     (id 1)
  //   //     (or
  //   //       (id 2)
  //   //       (id 3)
  //   //     )
  //   //   )
  //   // )

  //   // def combine(lhs: A, rhs: A): A = {
  //   //   partialOrder.pmin(lhs, rhs).getOrElse(semilattice.combine(lhs, rhs))
  //   // }

  //   def zero: Tree[A] = Or(setLattice.zero) // Nothing matches
  //   def one: Tree[A] = And(setLattice.zero) // Everything matches

  //   def join(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
  //     def create(tree: Or[A]): Or[A] = {
  //       Or(
  //         Or.flatten(Or.create(tree)).values.map(
  //           value => And.flatten(And.create(value))
  //         )
  //       )
  //     }

  //     val lOr = create(lhs)
  //     val rOr = create(rhs)

  //     implicit val semilattice = lattice.joinSemilattice
  //     implicit val partialOrder = setLattice.meetSemilattice.asJoinPartialOrder(setEq)

  //     partialOrder.pmin(a, b).getOrElse(semilattice.combine(lhs, rhs))
  //   }

  //   def meet(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
  //     def create(tree: And[A]): And[A] = {
  //       And(
  //         And.flatten(And.create(tree)).values.map(
  //           value => Or.flatten(Or.create(value))
  //         )
  //       )
  //     }

  //     implicit val semilattice = setLattice.joinSemilattice
  //     implicit val partialOrder = setLattice.joinSemilattice.asJoinPartialOrder(setEq)

  //     And(compress(Or.create(lhs).values, Or.create(rhs).values))
  //   }
  // }

  //   class PartialOrderLattice[A](
//     implicit
//     lattice: Lattice[A]
//   ) extends BoundedSemilattice[A] {
//     // Set(1, 2), Set(Set(1) -> Set(1)
//     // Set(1, 2), Set(3) -> Set(1, 2), Set(3)

//     def combine(lhs: A, rhs: A): A = {
//       partialOrder.pmin(a, b).getOrElse(semilattice.combine(lhs, rhs))
//     }

//     def one: Tree[A] = And(setLattice.zero) // Everything matches
//     def zero: Tree[A] = Or(setLattice.zero) // Nothing matches

//     def join(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
//       implicit val semilattice = lattice.joinSemilattice
//       implicit val partialOrder = setLattice.meetSemilattice.asJoinPartialOrder(setEq)

//       partialOrder.pmin(a, b).getOrElse(semilattice.combine(lhs, rhs))
//     }

//     def meet(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
//       implicit val semilattice = setLattice.joinSemilattice
//       implicit val partialOrder = setLattice.joinSemilattice.asJoinPartialOrder(setEq)

//       And(compress(Or.create(lhs).values, Or.create(rhs).values))
//     }
//   }

//   class PartialOrderLattice[F[_], A](
//     implicit
//     fa: BoundedSemilattice[F[A]],
//     a: Lattice[A]
//   ) extends BoundedSemilattice[F[A]] {
//     def zero: F[A] = semilattice.zero // Set.empty

//     // Set(Set(1, 2)), Set(Set(1)) -> Set(Set(1))
//     // Set(Set(1, 2)), Set(Set(3)) -> Set(Set(1, 2), Set(3))

//     def combine(lhs: F[A], rhs: F[A]): F[A] = {
//       partialOrder.pmin(a, b).getOrElse(semilattice.combine(lhs, rhs))
//     }

//     def one: Tree[A] = And(setLattice.zero) // Everything matches
//     def zero: Tree[A] = Or(setLattice.zero) // Nothing matches

//     def join(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
//       implicit val semilattice = setLattice.joinSemilattice
//       implicit val partialOrder = setLattice.meetSemilattice.asJoinPartialOrder(setEq)

//       Or(compress(And.create(lhs).values, And.create(rhs).values))
//     }

//     def meet(lhs: Tree[A], rhs: Tree[A]): Tree[A] = {
//       implicit val semilattice = setLattice.joinSemilattice
//       implicit val partialOrder = setLattice.joinSemilattice.asJoinPartialOrder(setEq)

//       And(compress(Or.create(lhs).values, Or.create(rhs).values))
//     }
//   }
}
