package potree

import cats.Functor
import cats.Monad
import scala.annotation.tailrec

object TreeMonad {
  implicit val treeMonad = new Monad[Tree] {
    import cats.instances.list._

    def pure[A](value: A): Tree[A] =
      Leaf(value)

    def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] =
      tree match {
        case Or(values) =>
          Or(Functor[List].map(values)(flatMap(_)(f)))
        case And(values) =>
          And(Functor[List].map(values)(flatMap(_)(f)))
        case Leaf(value) =>
          f(value)
      }

    // // Stack unsafe
    // def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
    //   flatMap(f(a)) {
    //     case Left(value) =>
    //       tailRecM(value)(f)
    //     case Right(value) =>
    //       Leaf(value)
    //   }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(
        open: List[Tree[Either[A, B]]],
        closed: List[Either[(Boolean, Int), Tree[B]]]
      ): List[Tree[B]] =
        open match {
          case Or(values) :: next =>
            loop(values.toList ++ next, Left((false, values.size)) :: closed)
          case And(values) :: next =>
            loop(values.toList ++ next, Left((true, values.size)) :: closed)
          case Leaf(Left(value)) :: next =>
            loop(f(value) :: next, closed)
          case Leaf(Right(value)) :: next =>
            loop(next, Right(pure(value)) :: closed)
          case Nil =>
            closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
              maybeTree match {
                case Left((false, size)) => {
                  val (head, tail) = acc.splitAt(size)
                  Or(head) :: tail
                }
                case Left((false, size)) => {
                  val (head, tail) = acc.splitAt(size)
                  Or(head) :: tail
                }
                case Right(value) => value :: acc
              }
            }
        }
      loop(List(f(a)), Nil).head
    }
  }
}
