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
import cats.laws.discipline.arbitrary._

class NaiveFuzzySetSpec extends CatsSuite {
  checkAll("List", FuzzySetTests[List].fuzzySet) // Debug

  // checkAll("Set", FuzzySetTests[Set].fuzzySet)
}
