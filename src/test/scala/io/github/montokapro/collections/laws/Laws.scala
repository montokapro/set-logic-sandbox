package io.github.montokapro.collections.laws

import io.github.montokapro.collections.FuzzySet

import cats.kernel.PartialOrder

import cats.laws.{IsEq, IsEqArrow}

import org.typelevel.discipline.Laws

import cats.implicits._

import cats.kernel._

import org.typelevel.discipline.Laws

import org.scalacheck.{Arbitrary, Cogen, Prop}
import org.scalacheck.Prop._

import cats.kernel.instances.all._

trait FuzzySetLaws[F[_]] extends Laws {
  implicit override def F: FuzzySet[F]

  def idempotentAdd[A: PartialOrder](a: A): IsEq[F[A]] =
    F.add(F.add(F.empty[A], a), a) <-> F.add(F.empty[A], a)
}

object FuzzySetLaws {
  def apply[F[_]](implicit evidence: FuzzySet[F]): FuzzySetLaws[F] =
    new FuzzySetLaws[F] {
      def F: FuzzySet[F] = evidence
    }
}
