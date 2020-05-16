package io.github.montokapro.collections

import cats.kernel.PartialOrder

trait FuzzySet[F[_]] {
  def empty[A]: F[A]

  def add[A](fa: F[A], a: A)(implicit order: PartialOrder[A]): F[A]
}

// object FuzzySet {
//   implicit val fuzzySet = new FuzzySet[Set] {
//     override def empty[A]: Set[A] = Set.empty[A]

//     def add[A](fa: Set[A], a: A)(implicit order: PartialOrder[A]): Set[A] = fa + a
//   }

//   def apply[F[_]](implicit set: FuzzySet[F]): FuzzySet[F] = set
// }
