package potree

import cats.kernel.Eq
import cats.kernel.Group
import algebra.lattice.BoundedMeetSemilattice
import algebra.lattice.BoundedMeetSemilatticeFunctions

import scala.{specialized => sp}

trait MeetGroup[@sp(Int, Long, Float, Double) A] extends Any with BoundedMeetSemilattice[A] { self =>
  def complement(a: A): A

  def meetGroup: Group[A] =
    new Group[A] {
      def empty: A = self.one
      def inverse(a: A): A = complement(a)
      def combine(x: A, y: A): A = meet(x, y)
    }
}

trait MeetGroupFunctions[B[A] <: MeetGroup[A]] extends BoundedMeetSemilatticeFunctions[B] {
  def complement[@sp(Int, Long, Float, Double) A](a: A)(implicit ev: B[A]): A = ev.complement(a)
}

object MeetGroup extends BoundedMeetSemilatticeFunctions[MeetGroup]
  with MeetGroupFunctions[MeetGroup] {

  /**
   * Access an implicit `MeetGroup[A]`.
   */
  @inline final def apply[@sp(Int, Long, Float, Double) A](implicit ev: MeetGroup[A]): MeetGroup[A] = ev
}
