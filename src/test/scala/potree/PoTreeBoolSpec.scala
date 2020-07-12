package potree

import org.scalatest.FunSpec
import cats.kernel.Eq
import cats.kernel.PartialOrder

class PoTreeBoolSpec extends FunSpec {
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

  describe("join") {
    describe("leaf") {
      it("equal") {
        val actual = join(
          Leaf(1),
          Leaf(1)
        )
        val expected = Leaf(1)
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inverse") {
        val actual = join(
          Leaf(1),
          Leaf(-1)
        )
        val expected = one
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inequal") {
        val actual = join(
          Leaf(1),
          Leaf(2)
        )
        val expected = or(
          Leaf(1),
          Leaf(2)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }
    }

    describe("and") {
      it("equal") {
        val actual = join(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(1),
            leaf(2)
          )
        )
        val expected = and(
          leaf(1),
          leaf(2)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("comparable") {
        val actual = join(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(1),
            leaf(2),
            leaf(3)
          )
        )
        val expected = and(
          leaf(1),
          leaf(2)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inequal") {
        val actual = join(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(2),
            leaf(3)
          )
        )
        val expected = or(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(2),
            leaf(3)
          )
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inverse") {
        val actual = join(
          and(
            leaf(1),
            leaf(2)
          ),
          or(
            leaf(-1),
            leaf(-3)
          )
        )
        val expected = one
        assert(actual == expected)
        //assert(actual eqv expected)
      }
    }

    describe("or") {
      it("equal") {
        val actual = join(
          or(
            leaf(1),
            leaf(2)
          ),
          or(
            leaf(1),
            leaf(2)
          )
        )
        val expected = or(
          leaf(1),
          leaf(2)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("comparable") {
        val actual = join(
          or(
            leaf(1),
            leaf(2)
          ),
          or(
            leaf(1),
            leaf(2),
            leaf(3)
          )
        )
        val expected = or(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inequal") {
        val actual = join(
          or(
            leaf(1),
            leaf(2)
          ),
          or(
            leaf(2),
            leaf(3)
          )
        )
        val expected = or(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inverse") {
        val actual = join(
          or(
            leaf(1),
            leaf(2)
          ),
          or(
            leaf(-1),
            leaf(-3)
          )
        )
        val expected = one
        assert(actual == expected)
        //assert(actual eqv expected)
      }
    }
  }

  describe("meet") {
    describe("leaf") {
      it("equal") {
        val actual = meet(
          Leaf(1),
          Leaf(1)
        )
        val expected = Leaf(1)
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inverse") {
        val actual = meet(
          Leaf(1),
          Leaf(-1)
        )
        val expected = zero
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inequal") {
        val actual = meet(
          Leaf(1),
          Leaf(2)
        )
        val expected = and(
          Leaf(1),
          Leaf(2)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }
    }

    describe("or") {
      it("equal") {
        val actual = meet(
          or(
            leaf(1),
            leaf(2)
          ),
          or(
            leaf(1),
            leaf(2)
          )
        )
        val expected = or(
          leaf(1),
          leaf(2)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("comparable") {
        val actual = meet(
          or(
            leaf(1),
            leaf(2)
          ),
          or(
            leaf(1),
            leaf(2),
            leaf(3)
          )
        )
        val expected = or(
          leaf(1),
          leaf(2)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inequal") {
        val actual = meet(
          or(
            leaf(1),
            leaf(2)
          ),
          or(
            leaf(2),
            leaf(3)
          )
        )
        val expected = and(
          or(
            leaf(1),
            leaf(2)
          ),
          or(
            leaf(2),
            leaf(3)
          )
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inverse") {
        val actual = meet(
          or(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(-1),
            leaf(-3)
          )
        )
        val expected = zero
        assert(actual == expected)
        //assert(actual eqv expected)
      }
    }

    describe("and") {
      it("equal") {
        val actual = meet(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(1),
            leaf(2)
          )
        )
        val expected = and(
          leaf(1),
          leaf(2)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("comparable") {
        val actual = meet(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(1),
            leaf(2),
            leaf(3)
          )
        )
        val expected = and(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inequal") {
        val actual = meet(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(2),
            leaf(3)
          )
        )
        val expected = and(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(actual == expected)
        //assert(actual eqv expected)
      }

      it("inverse") {
        val actual = meet(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(-1),
            leaf(-3)
          )
        )
        val expected = zero
        assert(actual == expected)
        //assert(actual eqv expected)
      }
    }
  }

  describe("complement") {
    it("leaf") {
      val actual = complement(Leaf(1))

      val expected = Leaf(-1)
      assert(actual == expected)
      //assert(actual eqv expected)
    }

    it("and") {
      val actual = complement(and(
        leaf(1),
        or(
          leaf(2),
          and(
            leaf(3),
            leaf(-4)
          )
        )
      ))

      val expected = or(
        leaf(-1),
        and(
          leaf(-2),
          or(
            leaf(-3),
            leaf(4)
          )
        )
      )

      assert(actual == expected)
      //assert(actual eqv expected)
    }

    it("or") {
      val actual = complement(or(
        leaf(1),
        and(
          leaf(2),
          or(
            leaf(3),
            leaf(-4)
          )
        )
      ))

      val expected = and(
        leaf(-1),
        or(
          leaf(-2),
          and(
            leaf(-3),
            leaf(4)
          )
        )
      )

      assert(actual == expected)
      //assert(actual eqv expected)
    }
  }
}
