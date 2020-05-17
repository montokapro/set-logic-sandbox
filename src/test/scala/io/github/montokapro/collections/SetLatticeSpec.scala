package io.github.montokapro.collections

import org.scalatest.FunSpec

class SetLatticeSetSpec extends FunSpec {
  describe("empty") {
    it("toSet") {
      assert(SetLattice.empty[Int].toSet == Set.empty)
    }
  }

  describe("one") {
    it("toSet") {
      assert(SetLattice(Set(1)).toSet == Set(Set(1)))
    }

    it("add equal") {
      assert(SetLattice(Set(1)).add(Set(1)).toSet == Set(Set(1)))
    }

    it("add greater than") {
      assert(SetLattice(Set(1)).add(Set(1, 2)).toSet == Set(Set(1, 2)))
    }

    it("add less than") {
      assert(SetLattice(Set(1, 2)).add(Set(1)).toSet == Set(Set(1, 2)))
    }
  }
}
