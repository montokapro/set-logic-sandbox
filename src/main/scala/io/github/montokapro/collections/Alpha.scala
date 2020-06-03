package io.github.montokapro.collections

import algebra.lattice.BoundedLattice
import algebra.lattice.Lattice
import cats.kernel.Eq

object Alpha {
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

    // val e = Set(Set(1, 2), Set(2, 3, 4), Set(1, 2, 3), Set(3, 4))

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

  // (and (and (or 1 2) (or 1)) (and (or 1) (or 2 3)))
  // scala> Alpha.join(Set(Set(1, 2), Set(3)), Set(Set(1), Set(2, 3)))
  // res0: Set[Set[Int]] = Set(Set(1), Set(3))

  // (or (or (and 1 2) (and 1)) (or (and 1) (and 2 3)))
  // scala> Alpha.meet(Set(Set(1, 2), Set(3)), Set(Set(1), Set(2, 3)))
  // res1: Set[Set[Int]] = Set(Set(2, 3), Set(1, 2))


  import algebra.lattice.DistributiveLattice

  abstract class Eta[A] extends DistributiveLattice[Set[A]] with Eq[Set[A]] {
    private val lattice = algebra.instances.set.setLattice[A]
    private val partialOrder = algebra.instances.set.catsKernelStdPartialOrderForSet[A]

  def meet(a: Set[A], b: Set[A]): Set[A] = lattice.meet(a, b)
    def join(a: Set[A], b: Set[A]): Set[A] = lattice.join(a, b)

    def eqv(a: Set[A], b: Set[A]) = partialOrder.eqv(a, b)
  }

  abstract class Zeta[A] extends BoundedLattice[Set[A]] {
    import cats.kernel.PartialOrder
    import cats.syntax.partialOrder._

    implicit val lattice: BoundedLattice[A]
    implicit val eq: Eq[A]
    implicit val partialOrder: PartialOrder[A]

    def meet(a: Set[A], b: Set[A]): Set[A] = {
      def reduce(acc: Set[A], value: A) = acc.filterNot(_ < value)

      a.foldLeft(b)(reduce) | b.foldLeft(a)(reduce)
    }

    def join(a: Set[A], b: Set[A]): Set[A] = {
      def reduce(acc: Set[A], value: A) = acc.filterNot(_ > value)

      a.foldLeft(b)(reduce) | b.foldLeft(a)(reduce)
    }

    def zero: Set[A] = Set(lattice.zero)
    def one: Set[A] = Set(lattice.one)
  }

  import cats.kernel.PartialOrder

  class Mu[A](
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

  // scala> val mu = new Mu(Beta)
  // mu: io.github.montokapro.collections.Mu[Set[Int]] = io.github.montokapro.collections.Mu@47babb32

  // scala> val c = Set(Set(1, 2), Set(2, 3, 4))
  // c: scala.collection.immutable.Set[scala.collection.immutable.Set[Int]] = Set(Set(1, 2), Set(2, 3, 4))

  // scala> val d = Set(Set(1, 2, 3), Set(3, 4))
  // d: scala.collection.immutable.Set[scala.collection.immutable.Set[Int]] = Set(Set(1, 2, 3), Set(3, 4))

  // scala> mu.joinSemilattice.combineAll(Set(c, d))
  // res0: Set[Set[Int]] = Set(Set(3, 4), Set(1, 2))

  // scala> mu.meetSemilattice.combineAll(Set(c, d))
  // res1: Set[Set[Int]] = Set(Set(1, 2, 3), Set(2, 3, 4))

  object Functions {
    //   // import algebra.instances.list.catsKernelStdMonoidForList._
    import cats.Alternative
    import cats.UnorderedTraverse
    import cats.Monad

    /**
      * Fold over the inner structure to combine all of the values with
      * our combine method inherited from MonoidK. The result is for us
      * to accumulate all of the "interesting" values of the inner G, so
      * if G is Option, we collect all the Some values, if G is Either,
      * we collect all the Right values, etc.
      *
      * Example:
      * {{{
      * scala> import cats.implicits._
      * scala> val x: List[Vector[Int]] = List(Vector(1, 2), Vector(3, 4))
      * scala> Alternative[List].unite(x)
      * res0: List[Int] = List(1, 2, 3, 4)
      * }}}
      */
    // def unite[G[_], A](fga: F[G[A]])(implicit FM: Monad[F], G: Foldable[G]): F[A] =
    //   FM.flatMap(fga) { ga =>
    //     G.foldLeft(ga, empty[A])((acc, a) => combineK(acc, pure(a)))
    //   }

    def a[F[_], A](fa: F[A])(implicit ev: Alternative[F] with UnorderedTraverse[F]): F[F[A]] = ev.map(fa)(ev.pure _)

    def simplify[F[_], A](fa: F[A])(implicit ev: Monad[F]): F[A] = ev.flatMap(fa)(ev.pure)
  }

  // object Omega {
  //   implicit def setMonad(implicit app: Applicative[Set]) =
  //     new Monad[Set] {
  //       // Define flatMap using Option's flatten method
  //       override def flatMap[A, B](fa: Set[A])(f: A => Set[B]): Set[B] =
  //         app.map(fa)(f).flatten
  //       // Reuse this definition from Applicative.
  //       override def pure[A](a: A): Set[A] = app.pure(a)

  //       @annotation.tailrec
  //       def tailRecM[A, B](init: A)(fn: A => Set[Either[A, B]]): Set[B] =
  //         fn(init) match {
  //           case None => None
  //           case Some(Right(b)) => Some(b)
  //           case Some(Left(a)) => tailRecM(a)(fn)
  //         }
  //     }
  // }

  // abstract trait Dual[A] {
  //   def dual: Dual[A]
  // }

  // sealed trait Dual2[A] extends Dual[A]

  // case class Pos[A](val value: A) extends Dual2[A] {
  //   override def complement = Neg[A](value)
  // }

  // case class Neg[A](val value: A) extends Dual2[A] {
  //   override def complement: Pos[A](value)
  // }

  // case class And[A](val value: A) extends Dual[A] {
  //   override def dual: Neg[A]
  // }

  // case class Neg[A](val value: A) extends Dual[A] {
  //   override def dual: Pos[A]
  // }

  // import algebra.lattice.Bool
  // import algebra.lattice.JoinSemilattice

  // abstract class Delta[Dual[A]](implicit lattice: JoinSemiLattice[A]) extends Bool[Dual[A]] {
  //   def meet(a: Dual[A], b: Dual[A]): Dual[A] = (a, b) match {
  //     case (Pos(x), Pos(y)) => Pos(x meet y)
  //     case (Neg(x), Neg(y)) => Neg(x join y)
  //     case _ => zero
  //   }
  //   def join(a: Dual[A], b: Dual[A]): Set[Int] = lattice.join(a, b)
  //   def zero: Set[Int] = Pos(lattice.zero)
  //   def one: Set[Int] = Neg(lattice.zero)

  //   // TODO: def eqv(a: Dual[A], b: Dual[A]) = a == b || a.dual = b


  //   def imp(a: A, b: A): A = or(complement(a), b)
  //   def without(a: A, b: A): A = and(a, complement(b))

  //   // xor is already defined in both Heyting and GenBool.
  //   // In Bool, the definitions coincide, so we just use one of them.
  //   override def xor(a: A, b: A): A =
  //     or(without(a, b), without(b, a))

  //   override def dual: Bool[A] = new DualBool(this)

  //   /**
  //    * Every Boolean algebra is a BoolRing, with multiplication defined as
  //    * `and` and addition defined as `xor`. Bool does not extend BoolRing
  //    * because, e.g. we might want a Bool[Int] and CommutativeRing[Int] to
  //    * refer to different structures, by default.
  //    *
  //    * Note that the ring returned by this method is not an extension of
  //    * the `Rig` returned from `BoundedDistributiveLattice.asCommutativeRig`.
  //    */
  //   override def asBoolRing: BoolRing[A] = new BoolRingFromBool(self)
  // }
}
