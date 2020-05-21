package io.github.montokapro.collections

import algebra.lattice.BoundedLattice
import algebra.lattice.Lattice
import cats.kernel.Eq

object Beta extends BoundedLattice[Set[Int]] with Eq[Set[Int]] {
  private val lattice = algebra.instances.set.setLattice[Int]
  private val partialOrder = algebra.instances.set.catsKernelStdPartialOrderForSet[Int]

  def meet(a: Set[Int], b: Set[Int]): Set[Int] = lattice.meet(a, b)
  def join(a: Set[Int], b: Set[Int]): Set[Int] = lattice.join(a, b)
  def zero: Set[Int] = lattice.zero
  def one: Set[Int] = Set(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

  def eqv(a: Set[Int], b: Set[Int]) = partialOrder.eqv(a, b)
}

// scala> Beta.meet(Set(1, 2), Set(2, 3))
// res1: Set[Int] = Set(2)

// scala> Beta.join(Set(1, 2), Set(2, 3))
// res2: Set[Int] = Set(1, 2, 3)

object Alpha extends BoundedLattice[Set[Set[Int]]] {
  import cats.syntax.partialOrder._

  private implicit val lattice = Beta
  private implicit val partialOrder = lattice.meetPartialOrder

  // scala> val c = Set(Set(1, 2), Set(2, 3, 4))
  // c: scala.collection.immutable.Set[scala.collection.immutable.Set[Int]] = Set(Set(1, 2), Set(2, 3, 4))

  // scala> val d = Set(Set(1, 2, 3), Set(3, 4))
  // d: scala.collection.immutable.Set[scala.collection.immutable.Set[Int]] = Set(Set(1, 2, 3), Set(3, 4))

  def meet(a: Set[Set[Int]], b: Set[Set[Int]]): Set[Set[Int]] = {
    def reduce(acc: Set[Set[Int]], set: Set[Int]) = acc.filterNot(_ < set)

    a.foldLeft(b)(reduce) | b.foldLeft(a)(reduce)
  }

  def join(a: Set[Set[Int]], b: Set[Set[Int]]): Set[Set[Int]] = {
    def reduce(acc: Set[Set[Int]], set: Set[Int]) = acc.filterNot(_ > set)

    a.foldLeft(b)(reduce) | b.foldLeft(a)(reduce)
  }

  def zero: Set[Set[Int]] = Set(Beta.zero)
  def one: Set[Set[Int]] = Set(Beta.one)
}

// scala> Alpha.join(Set(Set(1, 2), Set(3)), Set(Set(1), Set(2, 3)))
// res0: Set[Set[Int]] = Set(Set(1), Set(3))

// scala> Alpha.meet(Set(Set(1, 2), Set(3)), Set(Set(1), Set(2, 3)))
// res1: Set[Set[Int]] = Set(Set(2, 3), Set(1, 2))
