package io.github.montokapro.collections

import algebra.lattice.BoundedLattice
import algebra.lattice.Lattice
import algebra.lattice.GenBool
import cats.kernel.BoundedSemilattice
import cats.kernel.Eq
import cats.kernel.PartialOrder
import scala.annotation.tailrec

object Beta {
  sealed trait Tree {
    def toSet: Set[Int]
  }

  case class Leaf(value: Int) extends Tree {
    def toSet = Set(value)
  }

  case class Branch(values: Set[Leaf]) extends Tree {
    private val lattice = algebra.instances.set.setLattice[Int]

    // TODO: @annotation.tailrec or stack safe fold
    def toSet = lattice.joinSemilattice.combineAll(values.map(_.toSet))
  }

  class TreeLattice extends GenBool[Tree] {
    private val lattice = algebra.instances.set.setLattice[Int]

    // TODO: simplify Branch of size 1 to leaf
    def zero: Tree = Branch(lattice.zero.map(Leaf))
    def or(lhs: Tree, rhs: Tree): Tree = Branch(lattice.join(lhs.toSet, rhs.toSet).map(Leaf))
    def and(lhs: Tree, rhs: Tree): Tree = Branch(lattice.meet(lhs.toSet, rhs.toSet).map(Leaf))
    def without(lhs: Tree, rhs: Tree): Tree = Branch(lattice.without(lhs.toSet, rhs.toSet).map(Leaf))
  }

  // object Tree {
  //   private val lattice = algebra.instances.set.setLattice[Int]

  //   // TODO: @annotation.tailrec or stack safe fold
  //   def toJoinSet(tree: Tree): Set[Int] = tree match {
  //     case Leaf(value) => Set(value)
  //     case Branch(values) => lattice.joinSemilattice.combineAll(values.map(toJoinSet))
  //   }

  //   // // TODO: @annotation.tailrec or stack safe fold
  //   // def toMeetSet(tree: Tree): Set[Int] = tree match {
  //   //   case Leaf(value) => Set(value)
  //   //   case Branch(values) => lattice.meetSemilattice.combineAll(values.map(toMeetSet))
  //   // }
  // }

  // object LeafLattice extends BoundedLattice[Tree] with Eq[Tree] {
  //   private val lattice = algebra.instances.set.setLattice[Int]
  //   private val partialOrder = algebra.instances.set.catsKernelStdPartialOrderForSet[Int]

  //   def meet(a: Tree, b: Tree): Tree = a
  //   def join(a: Tree, b: Tree): Tree = Branch(lattice.join(Tree.toJoinSet(a), Tree.toJoinSet(b)).map(Leaf))
  //   def zero: Tree = Branch(Set.empty)
  //   def one: Tree = Branch(Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).map(Leaf(_)))

  //   def eqv(a: Tree, b: Tree) = a == b
  // }

  class PartialOrderSetLattice[A](
    val lattice: BoundedLattice[A] with Eq[A]
  ) extends BoundedLattice[Set[A]] {
    import cats.syntax.partialOrder._

    private implicit val l = lattice
    private implicit val po = lattice.meetPartialOrder

    def meet(a: Set[A], b: Set[A]): Set[A] = {
      def reduce(acc: Set[A], value: A) = acc.filterNot(_ < value)

      a.foldLeft(b)(reduce) | b.foldLeft(a)(reduce)
    }

    def join(a: Set[A], b: Set[A]): Set[A] = {
      def reduce(acc: Set[A], value: A) = acc.filterNot(_ > value)

    a.foldLeft(b)(reduce) | b.foldLeft(a)(reduce)
    }

    def zero: Set[A] = Set(lattice.one)
    def one: Set[A] = Set(lattice.zero)
  }
}
