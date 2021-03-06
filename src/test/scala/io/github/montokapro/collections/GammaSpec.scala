package io.github.montokapro.collections

import org.scalatest.FunSpec
import cats.kernel.instances.int._

class GammaSpec extends FunSpec {
  import Gamma._

  val treeLattice = new TreeLattice(cats.kernel.instances.int.catsKernelStdOrderForInt)
  val branchLattice = new BranchLattice(cats.kernel.instances.int.catsKernelStdOrderForInt)

  describe("or") {
    import Tree._
    it("should flatten") {
      val actual = Or.flatten(Set(
        pos(1),
        or(Set(
          pos(2),
          pos(3),
          or(Set(
            pos(4),
            pos(5)
          )),
          and(Set(
            pos(6),
            pos(7)
          ))
        ))
      ))
      val expected = Set(1, 2, 3, 4, 5).map(pos(_)) + and(Set(6, 7).map(pos(_)))
      assert(actual == expected)
    }

    it("should compress") {
      import branchLattice._
      val actual = meet(
        pos(1),
        or(Set(
          pos(1),
          pos(2)
        ))
      )
      val expected =
        and(Set(
          pos(1)
        ))
      assert(actual == expected)
    }

    it("should compress all") {
      val actual = branchLattice.joinSemilattice.combineAll(Set(
        pos(1),
        and(Set(
          pos(1),
          pos(2)
        )),
        pos(3)
      ))
      val expected = or(Set(
        and(Set(
          pos(1),
        )),
        and(Set(
          pos(3)
        ))
      ))
      assert(actual == expected)
    }
  }

  describe("and") {
    import Tree._
    it("should flatten") {
      val actual = And.flatten(Set(
        pos(1),
        and(Set(
          pos(2),
          pos(3),
          and(Set(
            pos(4),
            pos(5)
          )),
          or(Set(
            pos(6),
            pos(7)
          ))
        ))
      ))
      val expected = Set(1, 2, 3, 4, 5).map(pos(_)) + or(Set(6, 7).map(pos(_)))
      assert(actual == expected)
    }

    import branchLattice._
    it("should compress") {
      val actual = join(
        pos(1),
        and(Set(
          pos(1),
          pos(2)
        ))
      )
      val expected = // and(Set(
        or(Set(
          pos(1),
          pos(2)
        ))
      // ))
      assert(actual == expected)
    }

    it("should compress all") {
      val actual = branchLattice.meetSemilattice.combineAll(Set(
        pos(1),
        and(Set(
          pos(1),
          pos(2)
        )),
        pos(3)
      ))
      val expected = and(Set( // Should be the empty set for ids, this for props
        or(Set(
          pos(1),
          pos(2)
        )),
        or(Set(
          pos(3)
        ))
      ))
      assert(actual == expected)
    }
  }

  describe("join") {
    import treeLattice._

    it("should combine bounds") {
      assert(join(zero, one) == one)
      assert(join(one, zero) == one)
    }

    it("should combine zero") {
      assert(join(zero, Gamma.Pos(1)) == Gamma.Pos(1))
      assert(join(Gamma.Pos(1), zero) == Gamma.Pos(1))
    }

    it("should combine one") {
      assert(join(one, Gamma.Pos(1)) == one)
      assert(join(Gamma.Pos(1), one) == one)
    }

    it("should combine opposites") {
      assert(join(Gamma.Pos(1), Gamma.Neg(1)) == one)
      assert(join(Gamma.Neg(1), Gamma.Pos(1)) == one)
      assert(join(Gamma.Pos(1), Gamma.Or(Set(Gamma.Neg(1), Gamma.Neg(2)))) == one)
      assert(join(Gamma.Or(Set(Gamma.Neg(1), Gamma.Neg(2))), Gamma.Pos(1)) == one)
      assert(join(Gamma.Or(Set(Gamma.Pos(1), Gamma.Pos(2))), Gamma.Neg(1)) == one)
      assert(join(Gamma.Neg(1), Gamma.Or(Set(Gamma.Pos(1), Gamma.Pos(2)))) == one)
    }

    // TODO - what should these evaluate too?
    it("should combine ands") {
      def f(i: Int): Gamma.Tree[Int] = Gamma.And((1 to i).toSet[Int].map(Gamma.Pos(_)))
      def g(i: Int): Gamma.Tree[Int] = Gamma.And((1 to i).toSet[Int].map(Gamma.Neg(_)))

      assert(join(f(2), f(3)) == f(2))
      assert(join(f(3), f(2)) == f(2))
      assert(join(f(0), f(2)) == f(0))
      assert(join(f(2), f(0)) == f(0))

      // TODO
      assert(join(g(2), g(3)) == g(3))
      assert(join(g(3), g(2)) == g(3))
      assert(join(g(0), g(2)) == g(2))
      assert(join(g(2), g(0)) == g(2))
    }
  }

  describe("meet") {
    import treeLattice._

    it("should combine bounds") {
      assert(meet(zero, one) == zero)
      assert(meet(one, zero) == zero)
    }

    it("should combine zero") {
      assert(meet(zero, Gamma.Pos(1)) == zero)
      assert(meet(Gamma.Pos(1), zero) == zero)
    }

    it("should combine one") {
      assert(meet(one, Gamma.Pos(1)) == Gamma.Pos(1))
      assert(meet(Gamma.Pos(1), one) == Gamma.Pos(1))
    }

    it("should combine opposites") {
      assert(meet(Gamma.Pos(1), Gamma.Neg(1)) == zero)
      assert(meet(Gamma.Neg(1), Gamma.Pos(1)) == zero)
      assert(meet(Gamma.Pos(1), Gamma.And(Set(Gamma.Neg(1), Gamma.Neg(2)))) == zero)
      assert(meet(Gamma.And(Set(Gamma.Neg(1), Gamma.Neg(2))), Gamma.Pos(1)) == zero)
      assert(meet(Gamma.And(Set(Gamma.Pos(1), Gamma.Pos(2))), Gamma.Neg(1)) == zero)
      assert(meet(Gamma.Neg(1), Gamma.And(Set(Gamma.Pos(1), Gamma.Pos(2)))) == zero)
    }

    // TODO - what should these evaluate too?
    it("should combine ors") {
      assert(meet(Gamma.Pos(1), Gamma.Or(Set(Gamma.Neg(1), Gamma.Neg(2)))) == zero)
      assert(meet(Gamma.Or(Set(Gamma.Neg(1), Gamma.Neg(2))), Gamma.Pos(1)) == zero)
      assert(meet(Gamma.Or(Set(Gamma.Pos(1), Gamma.Pos(2))), Gamma.Neg(1)) == zero)
      assert(meet(Gamma.Neg(1), Gamma.Or(Set(Gamma.Pos(1), Gamma.Pos(2)))) == zero)
      assert(join(Gamma.Or(Set(Gamma.Pos(1), Gamma.Pos(2))), Gamma.Pos(1)) == zero)
      assert(join(Gamma.Pos(1), Gamma.Or(Set(Gamma.Pos(1), Gamma.Pos(2)))) == zero)
      assert(join(Gamma.Or(Set(Gamma.Neg(1), Gamma.Neg(2))), Gamma.Neg(1)) == zero)
      assert(join(Gamma.Neg(1), Gamma.Or(Set(Gamma.Neg(1), Gamma.Neg(2)))) == zero)      
    }
  }

  // describe("singleOption") {
  //   it("should process empty") {
  //     val actual = Set.empty
  //     val expected = None
  //     assert(singleOption(actual) == expected)
  //   }

  //   it("should process single") {
  //     val actual = Set(1)
  //     val expected = Some(1)
  //     assert(singleOption(actual) == expected)
  //   }

  //   it("should process multiple") {
  //     val actual = Set(1, 2)
  //     val expected = None
  //     assert(singleOption(actual) == expected)
  //   }

  //   it("should process even more") {
  //     val actual = Set(1, 2, 3)
  //     val expected = None
  //     assert(singleOption(actual) == expected)
  //   }
  // }

  // describe("And") {
  //   it("should simplify single") {
  //     val actual = And(Set(
  //       Id(1),
  //     ))
  //     val expected = Id(1)
  //     assert(Expr.reduce(actual) == expected)
  //   }

  //   it("should simplify multiple") {
  //     val actual = And(Set(
  //       Id(1),
  //       Id(2)
  //     ))
  //     val expected = Bottom
  //     assert(Expr.reduce(actual) == expected)
  //   }

  //   it("should reduce nested") {
  //     val actual = And(Set(
  //       And(Set(
  //         Id(1),
  //         Id(2)
  //       )),
  //       And(Set(
  //         Id(2),
  //         Id(3)
  //       ))
  //     ))
  //     val expected = Bottom
  //     assert(Expr.reduce(actual) == expected)
  //   }

  //   it("should reduce empty") {
  //     val actual = And(Set(
  //       Id(1),
  //       Bottom
  //     ))
  //     val expected = Bottom
  //     assert(Expr.reduce(actual) == expected)
  //   }

  //   describe("Not") {
  //     it("should simplify single") {
  //       val actual = And(Set(
  //         Not(Id(1)),
  //       ))
  //       val expected = Not(Id(1))
  //       assert(Expr.reduce(actual) == expected)
  //     }

  //     it("should not simplify multiple") {
  //       val actual = And(Set(
  //         Not(Id(1)),
  //         Not(Id(2))
  //       ))
  //       assert(Expr.reduce(actual) == actual)
  //     }

  //     it("should reduce nested") {
  //       val actual = And(Set(
  //         And(Set(
  //           Not(Id(1)),
  //           Not(Id(2))
  //         )),
  //         And(Set(
  //           Not(Id(2)),
  //           Not(Id(3))
  //         ))
  //       ))
  //       val expected = And(Set(
  //         Not(Id(1)),
  //         Not(Id(2)),
  //         Not(Id(3))
  //       ))
  //       assert(Expr.reduce(actual) == expected)
  //     }

  //     it("should reduce empty") {
  //       val actual = And(Set(
  //         Not(Id(1)),
  //         Bottom
  //       ))
  //       val expected = Bottom
  //       assert(Expr.reduce(actual) == expected)
  //     }
  //   }
  // }

  // describe("Or") {
  //   it("should simplify single") {
  //     val actual = Or(Set(
  //       Id(1)
  //     ))
  //     val expected = Id(1)
  //     assert(Expr.reduce(actual) == expected)
  //   }

  //   it("should not simplify multiple") {
  //     val actual = Or(Set(
  //       Id(1),
  //       Id(2)
  //     ))
  //     assert(Expr.reduce(actual) == actual)
  //   }

  //   it("should reduce nested") {
  //     val actual = Or(Set(
  //       Or(Set(
  //         Id(1),
  //         Id(2)
  //       )),
  //       Or(Set(
  //         Id(2),
  //         Id(3)
  //       ))
  //     ))
  //     val expected = Or(Set(
  //       Id(1),
  //       Id(2),
  //       Id(3)
  //     ))
  //     assert(Expr.reduce(actual) == expected)
  //   }

  //   it("should reduce empty") {
  //     val actual = Or(Set(
  //       Id(1),
  //       Top
  //     ))
  //     val expected = Top
  //     assert(Expr.reduce(actual) == expected)
  //   }

  //   describe("Not") {
  //     it("should simplify single") {
  //       val actual = Or(Set(
  //         Not(Id(1)),
  //       ))
  //       val expected = Not(Id(1))
  //       assert(Expr.reduce(actual) == expected)
  //     }

  //     it("should simplify multiple") {
  //       val actual = Or(Set(
  //         Not(Id(1)),
  //         Not(Id(2))
  //       ))
  //       val expected = Top
  //       assert(Expr.reduce(actual) == expected)
  //     }

  //     it("should reduce nested") {
  //       val actual = Or(Set(
  //         Or(Set(
  //           Not(Id(1)),
  //           Not(Id(2))
  //         )),
  //         Or(Set(
  //           Not(Id(2)),
  //           Not(Id(3))
  //         ))
  //       ))
  //       val expected = Not(Id(2))
  //       assert(Expr.reduce(actual) == expected)
  //     }

  //     it("should reduce empty") {
  //       val actual = Or(Set(
  //         Not(Id(1)),
  //         Top
  //       ))
  //       val expected = Top
  //       assert(Expr.reduce(actual) == expected)
  //     }
  //   }
  // }
}
