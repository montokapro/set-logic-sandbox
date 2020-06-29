package io.github.montokapro.collections

import cats.Functor
import cats.kernel.PartialOrder
import algebra.lattice.Lattice

object Zeta {
  import scala.{specialized => sp}
  trait Inverse[@sp(Int, Long, Float, Double) A] extends Any {
    def inverse(a: A): A
  }

  object Inverse {
    def by[F[_], @sp A](
      implicit
      functor: Functor[F],
      ev: Inverse[A]
    ): Inverse[F[A]] =
      new Inverse[F[A]] {
        def inverse(a: F[A]): F[A] = {
          functor.map(a)(ev.inverse)
        }
      }
  }

  sealed abstract trait Dual[A] {
    def inverse: Dual[A]
  }

  case class Neg[A](value: A) extends Dual[A] {
    def inverse = Pos(value)
  }

  case class Pos[A](value: A) extends Dual[A] {
    def inverse = Neg(value)
  }

  class DualInverse[A] extends Inverse[Dual[A]] {
    def inverse(dual: Dual[A]): Dual[A] = dual.inverse
  }

  trait DualInstances[A] {
    implicit val inverseForDual: Inverse[Dual[A]] = new DualInverse

    implicit val functor: Functor[Dual] =
      new Functor[Dual] {
        def map[A, B](dual: Dual[A])(f: A => B): Dual[B] =
          dual match {
            case Neg(value) =>
              Neg(f(value))
            case Pos(value) =>
              Pos(f(value))
          }
      }
  }

  class PosetLattice[A](
    po: PartialOrder[A],
  ) extends Lattice[List[A]] {
    import cats.instances.list._

    def join(a: List[A], b: List[A]): List[A] = {
      a.foldLeft(b)((c, d) => c.filterNot(po.gt(_, d))).union(
        b.foldLeft(a)((c, d) => c.filterNot(po.gteqv(_, d)))
      )
    }

    def meet(a: List[A], b: List[A]): List[A] = {
      a.foldLeft(b)((c, d) => c.filter(po.lt(_, d))).union(
        b.foldLeft(a)((c, d) => c.filter(po.lteqv(_, d)))
      )
    }
  }

  // This is probably not a real group... run the laws suite
  import cats.kernel.Group
  import algebra.lattice.GenBool
  class PosetGroup[A](
    inverse: Inverse[A],
    bool: GenBool[A]
  ) extends Group[List[A]] with Inverse[List[A]] {
    import cats.instances.list._

    def inverse(a: List[A]): List[A] =
      Functor[List].map(a)(inverse.inverse)

    def empty: List[A] = List.empty

    // TODO: is this associative? Otherwise consider a loop or quasigroup.
    def combine(a: List[A], b: List[A]): List[A] = {
      a.foldLeft(b)((fc, c) => fc.map(bool.without(_, inverse.inverse(c))))
    }
  }

  // Using a Set has become prohibitive.
  // For now we use a list, which is admittedly inefficient.
  //
  // In the future, we could use a custom poset class:
  // https://hackage.haskell.org/package/pomaps-0.0.2.1/docs/Data-POSet.html
  //
  // However, performance degrades for large anti-chains,
  // which is a likely use case for a "linter" where reductions are rare
  //
  // Ideally, the new Poset class would accept two PartialOrders:
  // The first would define lattice behavior.
  // The second would allow an optimization by defining deterministic memory layout.
  //   (this could be an Order as well, if we wanted to enforce efficiency)
  import cats.kernel.Eq
  import algebra.lattice.GenBool
  class InversePosetLattice[A](
    inverse: Inverse[A],
    bool: GenBool[A],
    eq: Eq[A]
  ) extends Lattice[List[A]] {
    import cats.instances.list._

    private val po = bool.joinSemilattice.asJoinPartialOrder(eq)
    private val posetLattice = new PosetLattice(po)
    private val posetGroup = new PosetGroup(inverse, bool)

    def join(a: List[A], b: List[A]): List[A] = {
      posetLattice.join(
        posetGroup.combine(a, b),
        posetGroup.combine(b, a)
      )
    }

    def meet(a: List[A], b: List[A]): List[A] = {
      posetLattice.meet(
        posetGroup.combine(a, b),
        posetGroup.combine(b, a)
      )
    }
  }

  // Idea:
  //
  // What if we sorted our lists by set size?
  // Perhaps we could avoid folding over the full list.
  //
  // We also could optimize the partial order computation
  // The GenBool instance is correct but not the most efficient
}
