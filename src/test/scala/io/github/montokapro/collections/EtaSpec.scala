package io.github.montokapro.collections

import org.scalatest.FunSpec
import cats.kernel.instances.int._
import cats.kernel.Eq
import cats.kernel.PartialOrder

class EtaSpec extends FunSpec {
  import Eta._

  class EqPartialOrder[A](eq: Eq[A]) extends PartialOrder[A] {
    def partialCompare(x: A, y: A): Double =
      if (eq.eqv(x, y)) 0.0 else Double.NaN
  }

  import cats.instances.int._

  val po = new EqPartialOrder(Eq[Int])
  val treePoInstances = new TreePartialOrderInstances(po)

  describe("partialOrder") {
    def leaf(a: Int): Tree[Int] = Leaf(a)
    def or(as: Tree[Int]*): Tree[Int] = Or(as.toList)
    def and(as: Tree[Int]*): Tree[Int] = And(as.toList)

    implicit val treePo = treePoInstances.treePo

    import cats.kernel.Comparison._
    import treePo._
    import cats.syntax.partialOrder._

    describe("leaf") {
      it("should compare equal") {
        assert(partialComparison(leaf(1), leaf(1)) == Some(EqualTo))
      }

      it("should compare unequal") {
        assert(partialComparison(leaf(1), leaf(2)) == None)
      }
    }

    describe("or") {
      it("should compare shallow") {
        val less = or(
          leaf(1),
          leaf(2)
        )
        val more = or(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(more > less)
      }

      it("should compare leaf") {
        val less = leaf(1)
        val more = or(
          leaf(1),
          leaf(2)
        )
        assert(more > less)
      }

      it("should compare nested") {
        val less = or(
          leaf(1),
          and(
            leaf(2),
            leaf(3)
          )
        )
        val more = or(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(more > less)
      }

      it("should compare complex") {
        val less = or(
          leaf(1),
          and(
            leaf(2),
            leaf(3),
            leaf(4)
          )
        )
        val more = or(
          leaf(1),
          leaf(2),
          and(
            leaf(3),
            leaf(4)
          )
        )
        assert(more > less)
      }
    }

    describe("and") {
      it("should compare shallow") {
        val less = and(
          leaf(1),
          leaf(2)
        )
        val more = and(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(more > less)
      }

      it("should compare leaf") {
        val less = leaf(1)
        val more = and(
          leaf(1),
          leaf(2)
        )
        assert(more > less)
      }

      it("should compare nested") {
        val less = and(
          leaf(1),
          or(
            leaf(2),
            leaf(3)
          )
        )
        val more = and(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(more > less)
      }

      it("should compare complex") {
        val less = and(
          leaf(1),
          or(
            leaf(2),
            leaf(3),
            leaf(4)
          )
        )
        val more = and(
          leaf(1),
          leaf(2),
          or(
            leaf(3),
            leaf(4)
          )
        )
        assert(more > less)
      }
    }
  }
}
