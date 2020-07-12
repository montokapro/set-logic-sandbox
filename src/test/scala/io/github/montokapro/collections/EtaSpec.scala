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

  describe("partial order") {
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

  describe("poset gen boolean algebra") {
    import cats.instances.set._
    import cats.syntax.partialOrder._

    val genBool = new PosetGenBool[Set[Int]](PartialOrder[Set[Int]])

    import genBool._

    describe("without") {
      // it("equal") {
      //   val actual = without(
      //     List(Set(1)),
      //     List(Set(1))
      //   )
      //   val expected = zero
      //   println(actual)
      //   assert(actual.toSet eqv expected.toSet)
      // }

      // it("greater than") {
      //   val actual = without(
      //     List(Set(1, 2, 3)),
      //     List(Set(1, 2))
      //   )
      //   val expected = List(Set(3))
      //   println(actual)
      //   assert(actual.toSet eqv expected.toSet)
      // }

      // it("less than") {
      //   val actual = without(
      //     List(Set(1, 2)),
      //     List(Set(1, 2, 3))
      //   )
      //   val expected = List(Set(2))
      //   println(actual)
      //   assert(actual.toSet eqv expected.toSet)
      // }
    }
  }

  describe("simple boolean algebra") {
    def leaf(a: Int): Tree[Int] = Leaf(a)
    def or(as: Tree[Int]*): Tree[Int] = Or(as.toList)
    def and(as: Tree[Int]*): Tree[Int] = And(as.toList)

    val in = new Inverse[Int] { def inverse(i: Int): Int = -i }
    val po = new EqPartialOrder(Eq[Int])
    val treeIn = new TreeInverse(in)

    implicit val treePo = treePoInstances.treePo

    // val bool = new PoOrGenBool[Int](treeIn, treePo)

    import cats.syntax.partialOrder._
    import treePo._
    // import bool._

    describe("without") {
      // it("equal leaf") {
      //   val actual = without(
      //     leaf(1),
      //     leaf(1)
      //   )
      //   val expected = and()
      //   println(actual)
      //   assert(actual eqv expected)
      // }

      // it("inequal leaf") {
      //   val actual = without(
      //     leaf(1),
      //     leaf(2)
      //   )
      //   val expected = and(
      //     leaf(1),
      //     leaf(-2)
      //   )
      //   println(actual)
      //   assert(actual eqv expected)
      // }
    }

    // describe("imp") {
    //   it("leaf") {
    //     val actual = imp(
    //       leaf(1),
    //       leaf(2)
    //     )
    //     val expected = or(
    //       leaf(-1),
    //       leaf(2)
    //     )
    //     assert(actual eqv expected)
    //   }
    // }
  }

  describe("boolean algebra") {
    def leaf(a: Int): Tree[Int] = Leaf(a)
    def or(as: Tree[Int]*): Tree[Int] = Or(as.toList)
    def and(as: Tree[Int]*): Tree[Int] = And(as.toList)

    val in = new Inverse[Int] { def inverse(i: Int): Int = -i }
    val po = new EqPartialOrder(Eq[Int])
    val bool = new InversePoTreeBool[Int](in, po)

    implicit val treePo = treePoInstances.treePo

    import cats.syntax.partialOrder._
    import treePo._
    import bool._

    def flatten[A](tree: Tree[A]): Tree[A] = {
      tree match {
        case (Or(values)) =>
          val list = values.map(flatten)
          if (list.size == 1) {
              list.head
          } else {
            Or(list)
          }
        case (And(values)) =>
          val list = values.map(flatten)
          if (list.size == 1) {
            list.head
          } else {
            And(list)
          }
        case (leaf: Leaf[A]) =>
          leaf
      }
    }

    describe("join") {
      // it("leaf") {
      //   val actual = join(
      //     leaf(1),
      //     leaf(1)
      //   )
      //   val expected = leaf(1)
      //   assert(flatten(actual) eqv expected)
      //   // assert(actual eqv expected)
      // }

      // it("inverse leaf") {
      //   val actual = join(
      //     leaf(1),
      //     leaf(-1)
      //   )
      //   println(s"inverse-leaf $actual -> ${flatten(actual)}")
      //   val expected = or()
      //   assert(flatten(actual) eqv expected)
      // }

      // it("inverse or") {
      //   val actual = join(
      //     or(
      //       leaf(1),
      //       leaf(2)
      //     ),
      //     or(
      //       leaf(-1),
      //       leaf(-2)
      //     )
      //   )
      //   val expected = and()
      //   assert(flatten(actual) eqv expected)
      // }
    }

    // it("should join complex") {
    //   val actual = join(
    //     or(
    //       and(
    //         leaf(1),
    //         leaf(2),
    //         leaf(3)
    //       ),
    //       and(
    //         leaf(3),
    //         leaf(4)
    //       )
    //     ),
    //     or(
    //       and(
    //         leaf(1),
    //         leaf(2)
    //       ),
    //       and(
    //         leaf(2),
    //         leaf(3),
    //         leaf(4)
    //       )
    //     )
    //   )
    //   val expected =
    //     or(
    //       and(
    //         leaf(1),
    //         leaf(2)
    //       ),
    //       and(
    //         leaf(3),
    //         leaf(4)
    //       )
    //     )
    //   assert(actual == expected)
    // }

    // it("should join nested") {
    //   val actual = join(
    //     or(
    //       and(
    //         leaf(1),
    //         and(
    //           or(
    //             leaf(2),
    //             leaf(3)
    //           ),
    //           leaf(4)
    //         )
    //       ),
    //       or(
    //         and(
    //           leaf(2),
    //           or(
    //             leaf(3),
    //             leaf(4),
    //             leaf(5)
    //           )
    //         )
    //       )
    //     ),
    //     or(
    //       or(
    //         and(
    //           or(
    //             leaf(1),
    //             leaf(2),
    //             leaf(3)
    //           ),
    //           leaf(4)
    //         )
    //       ),
    //       and(
    //         and(
    //           leaf(2),
    //           or(
    //             leaf(3),
    //             leaf(4)
    //           )
    //         ),
    //         leaf(5)
    //       )
    //     )
    //   )
    //   val expected =
    //     or(
    //       and(
    //         or(
    //           leaf(1),
    //           leaf(2),
    //           leaf(3)
    //         ),
    //         leaf(4)
    //       ),
    //       and(
    //         leaf(2),
    //         or(
    //           leaf(3),
    //           leaf(4),
    //           leaf(5)
    //         )
    //       )
    //     )
    //   assert(actual == expected)
    // }

    describe("meet") {
      // it("leaf") {
      //   val actual = meet(
      //     leaf(1),
      //     leaf(1)
      //   )
      //   val expected = leaf(1)
      //   assert(flatten(actual) eqv expected)
      //   // assert(actual eqv expected)
      // }

      // it("inverse leaf") {
      //   val actual = meet(
      //     leaf(1),
      //     leaf(-1)
      //   )
      //   val expected = and()
      //   assert(flatten(actual) eqv expected)
      // }

      // it("inverse and") {
      //   val actual = meet(
      //     and(
      //       leaf(1),
      //       leaf(2)
      //     ),
      //     and(
      //       leaf(-1),
      //       leaf(-2)
      //     )
      //   )
      //   val expected = or()
      //   assert(flatten(actual) eqv expected)
      // }
    }

//     it("should meet complex") {
//       val actual = meet(
//         and(
//           or(
//             leaf(1),
//             leaf(2),
//             leaf(3)
//           ),
//           or(
//             leaf(3),
//             leaf(4)
//           )
//         ),
//         and(
//           or(
//             leaf(1),
//             leaf(2)
//           ),
//           or(
//             leaf(2),
//             leaf(3),
//             leaf(4)
//           )
//         )
//       )
//       // This is an alternate sensical interpretation
//       // This makes sense if these are properties, not ids
//       val expected =
//         and(
//           or(
//             leaf(1),
//             leaf(2),
//             leaf(3)
//           ),
//           or(
//             leaf(2),
//             leaf(3),
//             leaf(4)
//           )
//         )
//       // val expected = and()
//       assert(actual == expected)
//     }
  }
}
