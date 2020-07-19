package potree

import org.scalatest.FunSpec
import cats.kernel.Eq
import cats.kernel.Order

class ListOrderSpec extends FunSpec {
  import cats.instances.int._
  import cats.syntax.order._

  implicit val listOrder = new ListOrder(Order[Int])

  describe("partial order") {
    it("equal") {
      assert(List.empty[Int] eqv List.empty[Int])
      assert(List(1) eqv List(1))
      assert(List(1, 2) eqv List(1, 2))
    }

    it("less than") {
      assert(List.empty[Int] < List(1))
      assert(List(1) < List(1, 2))
      assert(List(2) < List(1, 2))
      assert(List(1, 3) < List(1, 2, 3))
      assert(List(2) < List(1, 2, 3))
    }

    it("greater than") {
      assert(List(1) > List.empty[Int])
      assert(List(1, 2) > List(1))
      assert(List(1, 2) > List(2))
      assert(List(1, 2, 3) > List(1, 3))
      assert(List(1, 2, 3) > List(2))
    }
  }

  /*
   * These tests describe false positives, where the total order compares sets
   * that shouldn't be comparable by set logic.
   *
   * Our total ordering needs to be consistent, but any ordering that acts as
   * an upper bound on the partial order is sufficient for our needs.
   */
  describe("total order") {
    it("less than") {
      assert(List(1) < List(2))
      assert(List(1, 2) < List(1, 3))
      assert(List(1, 3) < List(2, 3))
      assert(List(2, 3) < List(1, 4))
      assert(List(2) < List(1, 3))
      assert(List(1, 2) < List(3))
    }

    it("greater than") {
      assert(List(2) > List(1))
      assert(List(1, 3) > List(1, 2))
      assert(List(2, 3) > List(1, 3))
      assert(List(1, 4) > List(2, 3))
      assert(List(1, 3) > List(2))
      assert(List(3) > List(1, 2))
    }
  }
}
