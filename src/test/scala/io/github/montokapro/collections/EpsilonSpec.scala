package io.github.montokapro.collections

import org.scalatest.FunSpec
import cats.kernel.instances.int._

class EpsilonSpec extends FunSpec {
  import Epsilon._

  describe("DeepSetLattice") {
    val partialOrder = algebra.instances.set.catsKernelStdPartialOrderForSet[Int]

    describe("with partial order") {
      val setLattice = new DeepSetLattice(partialOrder)

      import setLattice._

      it("should join single") {
        val actual = join(
          Set(
            Set(1)
          ),
          Set(
            Set(1)
          )
        )
        val expected =
          Set(
            Set(1)
          )
        assert(actual == expected)
      }

      it("should join equal") {
        val actual = join(
          Set(
            Set(1, 2)
          ),
          Set(
            Set(1, 2)
          )
        )
        val expected =
          Set(
            Set(1, 2)
          )
        assert(actual == expected)
      }

      it("should join comparable") {
        val actual = join(
          Set(
            Set(1, 2)
          ),
          Set(
            Set(1)
          )
        )
        val expected =
          Set(
            Set(1)
          )
        assert(actual == expected)
      }

      it("should join complex") {
        val actual = join(
          Set(
            Set(1, 2),
            Set(3)
          ),
          Set(
            Set(1),
            Set(2, 3)
          )
        )
        val expected =
          Set(
            Set(1),
            Set(3)
          )
        assert(actual == expected)
      }

      it("should meet single") {
        val actual = meet(
          Set(
            Set(1)
          ),
          Set(
            Set(1)
          )
        )
        val expected =
          Set(
            Set(1)
          )
        assert(actual == expected)
      }

      it("should meet equal") {
        val actual = meet(
          Set(
            Set(1, 2)
          ),
          Set(
            Set(1, 2)
          )
        )
        val expected =
          Set(
            Set(1, 2)
          )
        assert(actual == expected)
      }

      it("should meet comparable") {
        val actual = meet(
          Set(
            Set(1, 2)
          ),
          Set(
            Set(1)
          )
        )
        val expected =
          Set(
            Set(1)
          )
        assert(actual == expected)
      }

      it("should meet complex") {
        val actual = meet(
          Set(
            Set(1, 2),
            Set(3)
          ),
          Set(
            Set(1),
            Set(2, 3)
          )
        )
        val expected = Set.empty
        assert(actual == expected)
      }
    }

    describe("with reversed partial order") {
      import cats.kernel.PartialOrder

      val setLattice = new DeepSetLattice(PartialOrder.reverse(partialOrder))

      import setLattice._

      it("should join single") {
        val actual = join(
          Set(
            Set(1)
          ),
          Set(
            Set(1)
          )
        )
        val expected =
          Set(
            Set(1)
          )
        assert(actual == expected)
      }

      it("should join equal") {
        val actual = join(
          Set(
            Set(1, 2)
          ),
          Set(
            Set(1, 2)
          )
        )
        val expected =
          Set(
            Set(1, 2)
          )
        assert(actual == expected)
      }

      it("should join comparable") {
        val actual = join(
          Set(
            Set(1, 2)
          ),
          Set(
            Set(1)
          )
        )
        val expected =
          Set(
            Set(1, 2)
          )
        assert(actual == expected)
      }

      it("should join complex") {
        val actual = join(
          Set(
            Set(1, 2),
            Set(3)
          ),
          Set(
            Set(1),
            Set(2, 3)
          )
        )
        val expected =
          Set(
            Set(1, 2),
            Set(2, 3)
          )
        assert(actual == expected)
      }

      it("should meet single") {
        val actual = meet(
          Set(
            Set(1)
          ),
          Set(
            Set(1)
          )
        )
        val expected =
          Set(
            Set(1)
          )
        assert(actual == expected)
      }

      it("should meet equal") {
        val actual = meet(
          Set(
            Set(1, 2)
          ),
          Set(
            Set(1, 2)
          )
        )
        val expected =
          Set(
            Set(1, 2)
          )
        assert(actual == expected)
      }

      it("should meet comparable") {
        val actual = meet(
          Set(
            Set(1, 2)
          ),
          Set(
            Set(1)
          )
        )
        val expected =
          Set(
            Set(1, 2)
          )
        assert(actual == expected)
      }

      it("should meet complex") {
        val actual = meet(
          Set(
            Set(1, 2),
            Set(3)
          ),
          Set(
            Set(1),
            Set(2, 3)
          )
        )
        val expected = Set.empty
        assert(actual == expected)
      }
    }
  }
}
