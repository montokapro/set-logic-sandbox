package potree

import cats.Functor
import cats.kernel.PartialOrder
import algebra.lattice.Lattice
import algebra.lattice.Bool
import algebra.lattice.GenBool

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

object Tree {
  // TODO: handle nested
  def flatten[A](tree: Tree[A]): Tree[A] = {
    tree match {
      case (Or(values)) =>
        val list = values.map(flatten)
        if (list.size == 1) {
          list.head
        } else {
          Or(list)
        }
      case (And(values)) =>
        val list = values.map(flatten)
        if (list.size == 1) {
          list.head
        } else {
          And(list)
        }
      case (leaf: Leaf[A]) =>
        leaf
    }
  }
}
