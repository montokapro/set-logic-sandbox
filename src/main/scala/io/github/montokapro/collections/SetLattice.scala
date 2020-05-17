package io.github.montokapro.collections

sealed abstract class SetLattice[A] {
  def add(value: Set[A]): SetLattice[A]

  def toSet: Set[Set[A]]
}

object SetLattice {
  import cats._
  import cats.kernel.Semilattice

  object SetInstances {
    def dualSemilatticeForSet[A]: Semilattice[Set[A]] =
      new SetSemilattice[A]

    class SetSemilattice[A] extends Semilattice[Set[A]] {
      def combine(x: Set[A], y: Set[A]): Set[A] = x & y
    }
  }

  def add[A](fa: SetLattice[A], next: Set[A]): SetLattice[A] = {
    import cats.implicits._
    import cats.syntax._

    fa match {
      case Item(prev, set) => {
        if (prev.isEmpty) {
          Item(next, Set.empty[SetLattice[A]])
        } else {
          val intersection = prev & next // Semilattice[Set[A]].combine(prev, next)(SetInstances.dualSemilatticeForSet)

          if (intersection.isEmpty) {
            Item(next, set + Item(next, Set.empty[SetLattice[A]]))
          } else if (intersection == next) {
            fa
          } else {
            val subtraction = next -- intersection

            // This isn't right
            Item(
              intersection,
              Set(
                Item(prev -- intersection, Set.empty),
                Item(next -- intersection, set)
              )
            )
          }
        }
      }
    }
  }

  def toSet[A](fa: SetLattice[A]): Set[Set[A]] = fa match {
    case Item(value, set) => {
      if (set.isEmpty) {
        Set(value)
      } else {
        set.flatMap(item => item.toSet.map(_ | value))
      }
    }
  }

  private[collections] case class Item[A](value: Set[A], children: Set[SetLattice[A]]) extends SetLattice[A] {
    override def add(value: Set[A]) = SetLattice.add(this, value)

    override def toSet = SetLattice.toSet(this)
  }

  def empty[A]: SetLattice[A] = Item[A](Set.empty[A], Set.empty[SetLattice[A]])

  def apply[A](value: Set[A]): SetLattice[A] = Item[A](value, Set.empty[SetLattice[A]])
}
