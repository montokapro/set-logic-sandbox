package potree

import cats.kernel.Eq
import cats.kernel.PartialOrder
import cats.kernel.Semigroup
import cats.kernel.BoundedSemilattice
import algebra.lattice.Lattice
import algebra.lattice.Bool
import io.github.montokapro.collections.Eta.TreeInverse

class PoTreeBool[A](
  po: PartialOrder[A],
  in: Inverse[A]
) extends Bool[Tree[A]] with JoinGroup[Tree[A]] with MeetGroup[Tree[A]] {
  import cats.kernel.Comparison._

  val treePo: PartialOrder[Tree[A]] = new TreePartialOrderInstances(po).treePo
  val orSemigroup: Semigroup[List[Tree[A]]] = new PoListSemigroup(treePo)
  val andSemigroup: Semigroup[List[Tree[A]]] = new PoListSemigroup(treePo)

  def zero = Or(List.empty)

  def or(a: Tree[A], b: Tree[A]): Tree[A] = (a, b) match {
    case (Leaf(l), Leaf(r)) =>
      if (po.eqv(l, r)) {
        Leaf(l)
      } else if (po.eqv(l, in.inverse(r))) {
        one
      } else if (po.lt(l, r)) {
        Leaf(l)
      } else if (po.gt(l, r)) {
        Leaf(r)
      } else {
        Or(List(Leaf(l), Leaf(r)))
      }
    case (l, r) => Tree.flatten(Or(orSemigroup.combine(
      Or.create(l).values,
      Or.create(r).values
    )))
  }

  def and(a: Tree[A], b: Tree[A]): Tree[A] = (a, b) match {
    case (Leaf(l), Leaf(r)) =>
      if (po.eqv(l, r)) {
        Leaf(l)
      } else if (po.eqv(l, in.inverse(r))) {
        zero
      } else if (po.lt(l, r)) {
        Leaf(l)
      } else if (po.gt(l, r)) {
        Leaf(r)
      } else {
        And(List(Leaf(l), Leaf(r)))
      }
    case (l, r) => Tree.flatten(And(andSemigroup.combine(
      And.create(l).values,
      And.create(r).values
    )))
  }

  def one = And(List.empty)

  def complement(a: Tree[A]) = a match {
    case Or(values) => And(values.map(complement))
    case And(values) => Or(values.map(complement))
    case Leaf(value) => Leaf(in.inverse(value))
  }

  // TODO: recursive deep reduction
  def reduce(tree: Tree[A]) = tree match {
    case (Or(values)) => joinGroup.combineAll(values)
    case (And(values)) => meetGroup.combineAll(values)
    case (leaf: Leaf[A]) => leaf // TODO: deep reduction
  }
}
