// package io.github.montokapro.collections

// import org.scalatest.FunSpec
// import cats.PartialOrder
// import algebra.lattice.GenBool
// import algebra.lattice.Lattice

// class ThetaSpec extends FunSpec {
//   import Theta._

//   import cats.syntax.partialOrder._

//   import algebra.instances.set._

//   val genBool: GenBool[List[Set[Int]]] = new PosetGenBool(PartialOrder[Set[Int]], GenBool[Set[Int]])
//   import genBool._

//   describe("set") {
//     it("greater than") {
//       assert(GenBool[Set[Int]].without(Set(1, 2, 3), Set(1, 2)) eqv Set(3))
//     }

//     it("equal") {
//       assert(GenBool[Set[Int]].without(Set(1, 2), Set(1, 2)) eqv Set.empty)
//     }

//     it("less than") {
//       assert(GenBool[Set[Int]].without(Set(1, 2), Set(1, 2, 3)) eqv Set.empty)
//     }
//   }

//   describe("without") {
//     it("1") {
//       val actual = without(
//         List(
//           Set(1)
//         ),
//         List(
//           Set(1)
//         )
//       )
//       val expected = List(Set.empty[Int])
//       assert(actual.toSet eqv expected.toSet)
//     }

//     it("2") {
//       val actual = without(
//         List(
//           Set(1, 2, 3)
//         ),
//         List(
//           Set(1, 2)
//         )
//       )
//       val expected = List(
//         Set(3)
//       )
//       assert(actual.toSet eqv expected.toSet)
//     }

//     it("3") {
//       val actual = without(
//         List(
//           Set(1, 2, 3, 4),
//           Set(1, 2, 5, 6)
//         ),
//         List(
//           Set(1, 2)
//         )
//       )
//       val expected = List(
//         Set(3, 4),
//         Set(5, 6)
//       )
//       assert(actual.toSet eqv expected.toSet)
//     }

//     it("4") {
//       val actual = without(
//         List(
//           Set(1, 3),
//           Set(2, 4)
//         ),
//         List(
//           Set(1, 2)
//         )
//       )
//       val expected = List.empty[Set[Int]]
//       assert(actual.toSet eqv expected.toSet)
//     }

//     it("5") {
//       val actual = without(
//         List(
//           Set(1, 3),
//           Set(2, 3)
//         ),
//         List(
//           Set(1, 2)
//         )
//       )
//       val expected = List(
//         Set(3),
//         Set(3)
//       )
//       assert(actual.toSet eqv expected.toSet)
//     }
//   }
// }
