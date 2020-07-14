package potree

import org.scalatest.FunSpec
import cats.kernel.Eq
import cats.kernel.PartialOrder

class PoTreeBoolReduceSpec extends FunSpec {
  class EqPartialOrder[A](eq: Eq[A]) extends PartialOrder[A] {
    def partialCompare(x: A, y: A): Double =
      if (eq.eqv(x, y)) 0.0 else Double.NaN
  }

  import cats.instances.int._

  val po = new EqPartialOrder(Eq[Int])

  // TODO: generate Inverse from Group instance
  val in = new Inverse[Int] { def inverse(i: Int): Int = -i }

  import cats.syntax.eq._

  def leaf(a: Int): Tree[Int] = Leaf(a)
  def or(as: Tree[Int]*): Tree[Int] = Or(as.toList)
  def and(as: Tree[Int]*): Tree[Int] = And(as.toList)

  val bool = new PoTreeBool(po, in)

  import bool._

  describe("leaf") {
    it("irreducible") {
      val actual = reduce(leaf(1))
      val expected = leaf(1)
      assert(actual == expected)
      //assert(actual eqv expected)
    }
  }

  describe("or") {
    describe("leaf") {
      it("equal") {
        val actual = reduce(or(
          leaf(1),
          leaf(1),
          leaf(1)
        ))
        val expected = leaf(1)
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inverse") {
        val actual = reduce(or(
          leaf(1),
          leaf(-1),
          leaf(1)
        ))
        val expected = one
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inequal") {
        val actual = reduce(or(
          leaf(1),
          leaf(2),
          leaf(3)
        ))
        val expected = or(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }
    }

    describe("and") {
      it("equal") {
        val actual = reduce(or(
          and(
            leaf(1),
            or(
              leaf(2),
              leaf(-3)
            )
          ),
          and(
            or(
              leaf(-3),
              leaf(2)
            ),
            leaf(1)
          ),
          leaf(4)
        ))
        val expected = or(
          and(
            leaf(1),
            or(
              leaf(2),
              leaf(-3)
            )
          ),
          leaf(4)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("comparable") {
        val actual = reduce(or(
          leaf(1),
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(1),
            leaf(2),
            leaf(3)
          )
        ))
        val expected = leaf(1)
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inequal") {
        val actual = reduce(or(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(2),
            leaf(3)
          ),
          or(
            leaf(4),
            leaf(5)
          ),
        ))
        val expected = or(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(2),
            leaf(3)
          ),
          leaf(4),
          leaf(5)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inverse") {
        val actual = reduce(or(
          and(
            leaf(1),
            leaf(2)
          ),
          leaf(-1),
          leaf(-3)
        ))
        val expected = one
        assert(actual == expected)
        //assert(actual eqv expected)
      }
    }
  }
}
