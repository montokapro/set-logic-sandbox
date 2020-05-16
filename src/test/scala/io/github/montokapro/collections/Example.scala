package io.github.montokapro.collections

import cats._
// import cats._

sealed trait Tree[+A]
// defined trait Tree

case object Leaf extends Tree[Nothing]
// defined object Leaf

case class Node[A](p: A, left: Tree[A], right: Tree[A]) extends Tree[A]
// defined class Node

object Tree {
  implicit val functorTree: Functor[Tree] = new Functor[Tree] {
    def map[A, B](tree: Tree[A])(f: A => B) = tree match {
      case Leaf => Leaf
      case Node(p, left, right) => Node(f(p), map(left)(f), map(right)(f))
    }
  }
}
// defined object Tree
// warning: previously defined trait Tree is not a companion to object Tree.
// Companions must be defined together; you may wish to use :paste mode for this.

import cats.implicits._
// import cats.implicits._

import cats.laws.discipline.FunctorTests
// import cats.laws.discipline.FunctorTests

import cats.tests.CatsSuite

class Example extends CatsSuite {
  implicit def eqTree[A: Eq]: Eq[Tree[A]] = Eq.fromUniversalEquals

  import org.scalacheck.{Arbitrary, Gen}
  implicit def arbTree[A: Arbitrary]: Arbitrary[Tree[A]] =
    Arbitrary(
      Gen.oneOf(
        Gen.const(Leaf),
        (
          for {
            e <- Arbitrary.arbitrary[A]
          } yield Node(e, Leaf, Leaf)
        )
      )
    )

  checkAll("Tree.FunctorLaws", FunctorTests[Tree].functor[Int, Int, String])
}
