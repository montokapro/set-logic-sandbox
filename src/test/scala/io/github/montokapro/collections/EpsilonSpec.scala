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

  describe("Tree") {
    describe("functor") {
      def leaf(a: Int): Tree[Int] = Tree.leaf(a)
      def or(as: Tree[Int]*): Tree[Int] = Tree.or(as.toSet)
      def and(as: Tree[Int]*): Tree[Int] = Tree.and(as.toSet)

      import Tree.functor
      import cats.implicits._
      import cats.syntax._

      it("should map leaf") {
        val actual = leaf(1).map(_ + 1)
        val expected = leaf(2)
        assert(actual == expected)
      }

      it("should map or") {
        val actual = or(
          leaf(1),
          leaf(2)
        ).map(_ + 1)
        val expected = or(
          leaf(2),
          leaf(3)
        )
        assert(actual == expected)
      }

      it("should map and") {
        val actual = and(
          leaf(1),
          leaf(2)
        ).map(_ + 1)
        val expected = and(
          leaf(2),
          leaf(3)
        )
        assert(actual == expected)
      }

      it("should map complex") {
        val actual = or(
          leaf(1),
          and(
            leaf(2),
            leaf(3)
          )
        ).map(_ + 1)
        val expected = or(
          leaf(2),
          and(
            leaf(3),
            leaf(4)
          )
        )
        assert(actual == expected)
      }
    }

    describe("or") {
      def leaf(a: Int): Tree[Int] = Tree.leaf(a)
      def or(as: Tree[Int]*): Or[Int] = Or(as.toSet)

      it("should flatten") {
        val actual = or(
          leaf(1),
          or(
            leaf(2),
            leaf(3)
          )
        ).flatten
        val expected = or(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(actual == expected)
      }
    }

    describe("and") {
      def leaf(a: Int): Tree[Int] = Tree.leaf(a)
      def and(as: Tree[Int]*): And[Int] = And(as.toSet)

      it("should flatten") {
        val actual = and(
          leaf(1),
          and(
            leaf(2),
            leaf(3)
          )
        ).flatten
        val expected = and(
          leaf(1),
          leaf(2),
          leaf(3)
        )
        assert(actual == expected)
      }
    }
  }

  describe("TreeLattice") {
    val treeLattice = new TreeLattice[Int]

    import treeLattice._

    def leaf(a: Int): Tree[Int] = Tree.leaf(a)
    def or(as: Tree[Int]*): Tree[Int] = Tree.or(as.toSet)
    def and(as: Tree[Int]*): Tree[Int] = Tree.and(as.toSet)

    it("should join single") {
      val actual = join(
        leaf(1),
        leaf(1)
      )
      val expected = or(and(leaf(1))) // TODO: reduce
      assert(actual == expected)
    }

    it("should join complex") {
      val actual = join(
        or(
          and(
            leaf(1),
            leaf(2),
            leaf(3)
          ),
          and(
            leaf(3),
            leaf(4)
          )
        ),
        or(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(2),
            leaf(3),
            leaf(4)
          )
        )
      )
      val expected =
        or(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(3),
            leaf(4)
          )
        )
      assert(actual == expected)
    }

    it("should join nested") {
      val actual = join(
        or(
          and(
            leaf(1),
            leaf(2),
            leaf(3),
            and(
              or(
                leaf(5),
                leaf(6)
              ),
              leaf(7)
            )
          ),
          or(
            and(
              leaf(3),
              leaf(4)
            ),
            and(
              leaf(5),
              leaf(6)
            )
          )
        ),
        or(
          or(
            and(
              leaf(1),
              leaf(2)
            ),
            and(
              leaf(6),
              leaf(7)
            )
          ),
          or(
            and(
              leaf(2),
              leaf(3),
              leaf(4)
            ),
            and(
              leaf(5),
              or(
                leaf(6),
                leaf(7)
              )
            )
          )
        )
      )
      val expected =
        or(
          and(
            leaf(1),
            leaf(2)
          ),
          and(
            leaf(3),
            leaf(4)
          ),
          and(
            leaf(5),
            leaf(6)
          ),
          and(
            leaf(6),
            leaf(7)
          )
        )
      assert(actual == expected)
    }

    it("should meet single") {
      val actual = meet(
        leaf(1),
        leaf(1)
      )
      val expected = and(or(leaf(1))) // TODO: reduce
      assert(actual == expected)
    }

    it("should meet complex") {
      val actual = meet(
        and(
          or(
            leaf(1),
            leaf(2),
            leaf(3)
          ),
          or(
            leaf(3),
            leaf(4)
          )
        ),
        and(
          or(
            leaf(1),
            leaf(2)
          ),
          or(
            leaf(2),
            leaf(3),
            leaf(4)
          )
        )
      )
      // This is an alternate sensical interpretation
      // This makes sense if these are properties, not ids
      // val expected =
      //   and(
      //     or(
      //       leaf(1),
      //       leaf(2),
      //       leaf(3)
      //     ),
      //     or(
      //       leaf(3),
      //       leaf(4),
      //       leaf(5)
      //     )
      //   )
      val expected = and()
      assert(actual == expected)
    }
  }
}
