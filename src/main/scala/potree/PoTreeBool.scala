package potree

import cats.kernel.Eq
import cats.kernel.PartialOrder
import cats.kernel.Group
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
  val orGroup: Group[List[Tree[A]]] = new PoListGroup(treePo, meetGroup)
  val andGroup: Group[List[Tree[A]]] = new PoListGroup(treePo, joinGroup)

  def zero = Or(List.empty)

  def or(a: Tree[A], b: Tree[A]): Tree[A] = (a, b) match {
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
        Or(List(Leaf(l), Leaf(r)))
      }
    case (l: ToOr, r: ToOr) => println(s"or - toOr $l $r"); Tree.flatten(Or(orGroup.combine(
      Or.create(l).values, Or.create(r).values
    )))
    case (l, r) => println(s"or - elseOr $l $r"); Tree.flatten(Or(orGroup.combine(
      Or.create(l).values, Or.create(r).values
    )))
  }

  def and(a: Tree[A], b: Tree[A]): Tree[A] = (a, b) match {
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
        And(List(Leaf(l), Leaf(r)))
      }
    case (l: ToAnd, r: ToAnd) => println(s"and - ToAnd $l $r"); Tree.flatten(And(andGroup.combine(
      And.create(l).values, And.create(r).values
    )))
    case (l, r) => println(s"and - elseAnd $l $r"); Tree.flatten(And(andGroup.combine(
      And.create(l).values, And.create(r).values
    )))
  }

  def one = And(List.empty)

  def complement(a: Tree[A]) = a match {
    case Or(values) => And(values.map(complement))
    case And(values) => Or(values.map(complement))
    case Leaf(value) => Leaf(in.inverse(value))
  }

  // def reduce(tree: Tree[A]) = tree match {
  //   case (Or(values)) =>
  //     val set: Set[Tree[A]] = Or.create( // .create is no-op
  //       joinSemilattice.combineAllOption(Or.flatten(values.map(reduce)))
  //         .getOrElse(or(Set.empty[Tree[A]]))
  //     ).values.map(flatten)
  //     if (set.size == 1) {
  //       set.head
  //     } else {
  //       Or(set)
  //     }
  //   case (And(values)) =>
  //     val set: Set[Tree[A]] = And.create( // .create is no-op
  //       meetSemilattice.combineAllOption(And.flatten(values.map(reduce)))
  //         .getOrElse(and(Set.empty[Tree[A]]))
  //     ).values.map(flatten)
  //     if (set.size == 1) {
  //       set.head
  //     } else {
  //       And(set)
  //     }
  //   case (leaf: Leaf[A]) =>
  //     leaf
  // }
}
