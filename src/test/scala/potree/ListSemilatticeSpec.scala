package potree

import org.scalatest.FunSpec
import cats.kernel.Eq

class ListSemilatticeSpec extends FunSpec {
  val eq = Eq.fromUniversalEquals[Int]
  val semilattice = new ListSemilattice(eq)

  import semilattice._

  implicit val listEq: Eq[List[Int]] = new Eq[List[Int]] {
    def eqv(a: List[Int], b: List[Int]) = a.toSet == b.toSet
  }

  import cats.syntax.eq._

  describe("combine") {
    it("single equal") {
      val actual = combine(
        List(1),
        List(1)
      )
      val expected = List.empty[Int]
      assert(actual eqv expected)
    }

    it("multiple equal") {
      val actual = combine(
        List(1, 2),
        List(1, 2)
      )
      val expected = List.empty[Int]
      assert(actual eqv expected)
    }

    it("greater than") {
      val actual = combine(
        List(1, 2, 3),
        List(1, 2)
      )
      val expected = List(3)
      assert(actual eqv expected)
    }

    it("less than") {
      val actual = combine(
        List(1, 2),
        List(1, 2, 3)
      )
      val expected = List(3)
      assert(actual eqv expected)
    }

    it("common factors") {
      val actual = combine(
        List(1, 2, 3, 4),
        List(1, 2, 5, 6)
      )
      val expected = List(3, 4, 5, 6)
      assert(actual eqv expected)
    }
  }
}
