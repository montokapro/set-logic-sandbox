package io.github.montokapro.collections.laws

import io.github.montokapro.collections.FuzzySet
import org.scalacheck.{Arbitrary, Prop}
import Prop._
import org.typelevel.discipline.Laws
import cats.laws.IsEq
import cats.kernel.Eq
import cats.laws.discipline.catsLawsIsEqToProp

trait FuzzySetTests[F[_]] extends Laws {
  def laws: FuzzySetLaws[F]

  def fuzzySet[A: Arbitrary](implicit
    // ArbA: Arbitrary[A],
    // ArbFA: Arbitrary[F[A]],
    // CogenA: Cogen[A],
    ArbAFA: Arbitrary[A => F[A]],
    EqFA: Eq[F[A]]
  ): RuleSet =
    new DefaultRuleSet(
      name = "fuzzy set",
      parent = None,
      "idempotent add" -> forAll(laws.idempotentAdd[A] _),
    )
}

object FuzzySetTests {
  def apply[F[_]: FuzzySet]: FuzzySetTests[F] =
    new FuzzySetTests[F] {
      def laws: FuzzySetLaws[F] = FuzzySetLaws[F]
    }
}
