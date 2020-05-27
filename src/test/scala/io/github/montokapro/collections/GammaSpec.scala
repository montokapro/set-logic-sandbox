package io.github.montokapro.collections

import org.scalatest.FunSpec
import cats.kernel.instances.int._

class GammaSpec extends FunSpec {
  val treeLattice = new Gamma.TreeLattice(cats.kernel.instances.int.catsKernelStdOrderForInt)

  import treeLattice._

  describe("join") {
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
  }

  describe("meet") {
    it("should combine bounds 2") {
      assert(meet(zero, one) == zero)
      assert(meet(one, zero) == zero)
    }

    it("should combine zero 2") {
      assert(meet(zero, Gamma.Pos(1)) == zero)
      assert(meet(Gamma.Pos(1), zero) == zero)
    }

    it("should combine one 2") {
      assert(meet(one, Gamma.Pos(1)) == Gamma.Pos(1))
      assert(meet(Gamma.Pos(1), one) == Gamma.Pos(1))
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
