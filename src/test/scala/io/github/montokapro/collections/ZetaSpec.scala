package io.github.montokapro.collections

import org.scalatest.FunSpec
import cats.kernel.instances.int._
import cats.kernel.Eq
import cats.Functor
import algebra.lattice.GenBool
import algebra.lattice.Lattice

class ZetaSpec extends FunSpec {
  import Zeta._

  describe("PosetLattice") {
    import cats.instances.list._

    // Inefficient set-like lattice for testing
    class ListLattice[A] extends GenBool[List[A]] {
      def zero = List.empty

      def or(a: List[A], b: List[A]): List[A] = {
        (a.toSet | b.toSet).toList
      }

      def and(a: List[A], b: List[A]): List[A] = {
        (a.toSet & b.toSet).toList
      }

      def without(a: List[A], b: List[A]): List[A] = {
        (a.toSet -- b.toSet).toList
      }
    }

    val inverse: Inverse[List[Dual[Int]]] = Inverse.by(
      Functor[List],
      new DualInverse
    )
    val lattice: GenBool[List[Dual[Int]]] = new ListLattice[Dual[Int]]
    val eq: Eq[List[Dual[Int]]] = new Eq[List[Dual[Int]]] {
      def eqv(a: List[Dual[Int]], b: List[Dual[Int]]) = a.toSet == b.toSet
    }

    describe("with lattice") {
      val posetLattice: Lattice[List[List[Dual[Int]]]] = new InversePosetLattice(inverse, lattice, eq)

      import posetLattice._

      implicit val nestedEq: Eq[List[List[Dual[Int]]]] = new Eq[List[List[Dual[Int]]]] {
        def eqv(a: List[List[Dual[Int]]], b: List[List[Dual[Int]]]) =
          a.map(_.toSet).toSet == b.map(_.toSet).toSet
      }

      import cats.syntax.eq._

      describe("join") {
        it("should combine single") {
          val actual = join(
            List(
              List(
                Pos(1)
              )
            ),
            List(
              List(
                Pos(1)
              )
            )
          )
          val expected =
            List(
              List(
                Pos(1)
              )
            )
          assert(actual eqv expected)
        }

        it("should combine single inverse") {
          val actual = join(
            List(
              List(
                Neg(1)
              )
            ),
            List(
              List(
                Pos(1)
              )
            )
          )
          val expected = List(
            List.empty
          )
          assert(actual eqv expected)
        }

        it("should combine partial inverse") {
          val actual = join(
            List(
              List(
                Neg(1),
                Neg(2),
                Pos(3),
                Neg(4)
              )
            ),
            List(
              List(
                Pos(1),
                Pos(2),
                Pos(3)
              )
            )
          )
          val expected =
            List(
              List(
                Pos(3)
              )
            )
          assert(actual eqv expected)
        }

        it("should combine equal") {
          val actual = join(
            List(
              List(
                Pos(1),
                Neg(2)
              )
            ),
            List(
              List(
                Neg(2),
                Pos(1)
              )
            )
          )
          val expected =
            List(
              List(
                Pos(1),
                Neg(2)
              )
            )
          assert(actual eqv expected)
        }

        it("should combine comparable") {
          val actual = join(
            List(
              List(
                Pos(1),
                Neg(2)
              )
            ),
            List(
              List(
                Pos(1)
              )
            )
          )
          val expected =
            List(
              List(
                Pos(1)
              )
            )
          assert(actual eqv expected)
        }

        it("should combine complex") {
          val actual = join(
            List(
              List(
                Pos(1),
                Neg(2)
              ),
              List(
                Pos(3)
              )
            ),
            List(
              List(
                Pos(1)
              ),
              List(
                Neg(2),
                Pos(3)
              )
            )
          )
          val expected =
            List(
              List(
                Pos(1)
              ),
              List(
                Pos(3)
              )
            )
          assert(actual eqv expected)
        }
      }

      describe("meet") {
        it("should combine single") {
          val actual = meet(
            List(
              List(
                Pos(1)
              )
            ),
            List(
              List(
                Pos(1)
              )
            )
          )
          val expected =
            List(
              List(
                Pos(1)
              )
            )
          assert(actual eqv expected)
        }

        it("should combine single inverse") {
          val actual = meet(
            List(
              List(
                Neg(1)
              )
            ),
            List(
              List(
                Pos(1)
              )
            )
          )
          val expected = List(
            List.empty
          )
          assert(actual eqv expected)
        }

        it("should combine equal") {
          val actual = meet(
            List(
              List(
                Pos(1),
                Neg(2)
              )
            ),
            List(
              List(
                Neg(2),
                Pos(1)
              )
            )
          )
          val expected =
            List(
              List(
                Pos(1),
                Neg(2)
              )
            )
          assert(actual eqv expected)
        }

        it("should combine comparable") {
          val actual = meet(
            List(
              List(
                Pos(1),
                Neg(2)
              )
            ),
            List(
              List(
                Pos(1)
              )
            )
          )
          val expected =
            List(
              List(
                Pos(1)
              )
            )
          assert(actual eqv expected)
        }

        it("should combine complex") {
          val actual = meet(
            List(
              List(
                Pos(1),
                Neg(2)
              ),
              List(
                Pos(3)
              )
            ),
            List(
              List(
                Pos(1)
              ),
              List(
                Neg(2),
                Pos(3)
              )
            )
          )
          val expected = List.empty
          assert(actual eqv expected)
        }
      }
    }
  }
}
