package potree

import cats.kernel.Eq
import cats.kernel.Group
import algebra.lattice.BoundedJoinSemilattice
import algebra.lattice.BoundedJoinSemilatticeFunctions

import scala.{specialized => sp}

trait JoinGroup[@sp(Int, Long, Float, Double) A] extends Any with BoundedJoinSemilattice[A] { self =>
  def complement(a: A): A

  def joinGroup: Group[A] =
    new Group[A] {
      def empty: A = self.zero
      def inverse(a: A): A = complement(a)
      def combine(x: A, y: A): A = join(x, y)
    }
}

trait JoinGroupFunctions[B[A] <: JoinGroup[A]] extends BoundedJoinSemilatticeFunctions[B] {
  def complement[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: B[A]): A = ev.complement(a)
}

object JoinGroup extends BoundedJoinSemilatticeFunctions[JoinGroup]
  with JoinGroupFunctions[JoinGroup] {

  /**
   * Access an implicit `JoinGroup[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit ev: JoinGroup[A]): JoinGroup[A] = ev
}
