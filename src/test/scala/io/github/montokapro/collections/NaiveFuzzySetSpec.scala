package io.github.montokapro.collections

// import org.scalatest.FunSuite

// import cats.laws.discipline._

// import org.typelevel.discipline.scalatest.Discipline

import io.github.montokapro.collections.laws.FuzzySetTests

// import arbitraries._
// import cats.implicits._
// import org.scalacheck.ScalacheckShapeless._

// import org.scalatest.prop.Checkers
// import org.scalacheck.{ Arbitrary, Prop, Gen }
// import Prop._

// import Arbitrary.arbitrary

// import cats.laws.discipline._
// import Arbitraries._
// import org.typelevel.discipline.Laws

import NaiveFuzzySet._

// import arbitrary._
import cats.tests.CatsSuite

// import cats.laws.discipline.arbitrary

// import cats.instances.all._
// import cats.laws.discipline.arbitrary._


import cats._

class NaiveFuzzySetSpec extends CatsSuite {
  implicit def eq[A: Eq]: Eq[FuzzySet[List]] = Eq.fromUniversalEquals

  // import org.scalacheck.{Arbitrary, Gen}
  // import io.github.montokapro.collections.NaiveFuzzySet._
  // implicit def arb[A: Arbitrary]: Arbitrary[FuzzySet[List[A]]] =
  //   Arbitrary(
  //     Gen.oneOf(
  //       fuzzyList.empty[A],
  //       (
  //         for {
  //           a <- Arbitrary.arbitrary[A]
  //         } yield fuzzyList.add(fuzzyList.empty[A], a)
  //       )
  //     )
  //   )

  // checkAll("List", FuzzySetTests[List].fuzzySet) // Debug

  // checkAll("Set", FuzzySetTests[Set].fuzzySet)
}
