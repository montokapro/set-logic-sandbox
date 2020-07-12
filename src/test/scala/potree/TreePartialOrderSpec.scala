package potree

import org.scalatest.FunSpec
import cats.kernel.instances.int._
import cats.kernel.Eq
import cats.kernel.PartialOrder

class TreePartialOrderSpec extends FunSpec {
  describe("equal order") {
    class EqPartialOrder[A](eq: Eq[A]) extends PartialOrder[A] {
      def partialCompare(x: A, y: A): Double =
        if (eq.eqv(x, y)) 0.0 else Double.NaN
    }

    import cats.instances.int._

    val po = new EqPartialOrder(Eq[Int])
    val treePoInstances = new TreePartialOrderInstances(po)

    def leaf(a: Int): Tree[Int] = Leaf(a)
    def or(as: Tree[Int]*): Tree[Int] = Or(as.toList)
    def and(as: Tree[Int]*): Tree[Int] = And(as.toList)

    implicit val treePo = treePoInstances.treePo

    import cats.kernel.Comparison._
    import cats.syntax.partialOrder._
    import treePo._

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

  describe("partial order") {
    import cats.instances.set._

    val po = PartialOrder[Set[Int]]
    val treePoInstances = new TreePartialOrderInstances(po)

    def leaf(a: Set[Int]): Tree[Set[Int]] = Leaf(a)
    def or(as: Tree[Set[Int]]*): Tree[Set[Int]] = Or(as.toList)
    def and(as: Tree[Set[Int]]*): Tree[Set[Int]] = And(as.toList)

    implicit val treePo = treePoInstances.treePo

    import cats.kernel.Comparison._
    import cats.syntax.partialOrder._
    import treePo._

    describe("leaf") {
      it("should compare equal") {
        assert(partialComparison(leaf(Set(1)), leaf(Set(1))) == Some(EqualTo))
      }

      it("should compare less than") {
        assert(partialComparison(leaf(Set(1)), leaf(Set(1, 2))) == Some(LessThan))
      }

      it("should compare greater than") {
        assert(partialComparison(leaf(Set(1, 2)), leaf(Set(2))) == Some(GreaterThan))
      }

      it("should compare unequal") {
        assert(partialComparison(leaf(Set(1)), leaf(Set(2))) == None)
      }
    }

    describe("and") {
      describe("leaf") {
        it("should compare equal") {
          val less = and(
            leaf(Set(1, 2)),
            leaf(Set(2, 3))
          )
          val more = and(
            leaf(Set(2, 1)),
            leaf(Set(3, 2))
          )
          assert(more eqv less)
        }
      }

      describe("or") {
        it("should compare equal") {
          val less = and(
            or(
              leaf(Set(1, 2)),
              leaf(Set(2, 3))
            ),
            or(
              leaf(Set(3, 4)),
              leaf(Set(4, 5))
            )
          )
          val more = and(
            or(
              leaf(Set(3, 2)),
              leaf(Set(2, 1))
            ),
            or(
              leaf(Set(5, 4)),
              leaf(Set(4, 3))
            )
          )
          assert(more eqv less)
        }
      }
    }

    describe("or") {
      describe("leaf") {
        it("should compare equal") {
          val less = or(
            leaf(Set(1, 2)),
            leaf(Set(2, 3))
          )
          val more = or(
            leaf(Set(2, 1)),
            leaf(Set(3, 2))
          )
          assert(more eqv less)
        }
      }

      describe("or") {
        it("should compare equal") {
          val less = and(
            or(
              leaf(Set(1, 2)),
              leaf(Set(2, 3))
            ),
            or(
              leaf(Set(3, 4)),
              leaf(Set(4, 5))
            )
          )
          val more = and(
            or(
              leaf(Set(3, 2)),
              leaf(Set(2, 1))
            ),
            or(
              leaf(Set(5, 4)),
              leaf(Set(4, 3))
            )
          )
          assert(more eqv less)
        }
      }
    }
  }
}
