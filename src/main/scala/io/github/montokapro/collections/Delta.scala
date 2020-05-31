package io.github.montokapro.collections

object Delta {
  import cats.{Applicative, Traverse}

  sealed abstract trait Tree[A]

  final case class Leaf[A](value: A) extends Tree[A]

  final case class Branch[A](values: Set[Tree[A]]) extends Tree[A]

  import cats.Functor
  implicit val treeFunctor: Functor[Tree] =
    new Functor[Tree] {
      import cats.instances.set._

      def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
        tree match {
          case Branch(values) =>
            Branch(values.map(map(_)(f)))
          case Leaf(value) =>
            Leaf(f(value))
        }
    }

  object Tree {
    def leaf[A](value: A): Tree[A] = Leaf(value)
    def branch[A](values: Set[Tree[A]]): Tree[A] = Branch(values)
  }

  import cats.CommutativeMonad
  import scala.annotation.tailrec
  implicit val treeMonad = new CommutativeMonad[Tree] {
    def pure[A](value: A): Tree[A] =
      Leaf(value)

    def flatMap[A, B](tree: Tree[A])
      (f: A => Tree[B]): Tree[B] =
      tree match {
        case Branch(values) =>
          Branch(values.map(flatMap(_)(f)))
        case Leaf(value) =>
          f(value)
      }

    // // TODO: not stack safe, use the below version
    // def tailRecM[A, B](a: A)
    //   (f: A => Tree[Either[A, B]]): Tree[B] =
    //   flatMap(f(a)) {
    //     case Left(value) =>
    //       tailRecM(value)(f)
    //     case Right(value) =>
    //       Leaf(value)
    //   }

    // TODO: test this! MonadTests[Tree].monad[Int, Int, String]
    // Use CatsSuite
    def tailRecM[A, B](a: A)
      (f: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(
        open: List[Tree[Either[A, B]]],
        closed: List[Either[Int, Tree[B]]]): List[Tree[B]] =
        open match {
          case Branch(values) :: next =>
            loop(values.toList ++ next, Left(values.size) :: closed)
          case Leaf(Left(value)) :: next =>
            loop(f(value) :: next, closed)
          case Leaf(Right(value)) :: next =>
            loop(next, Right(pure(value)) :: closed)
          case Nil =>
            closed.foldLeft(Nil: List[Tree[B]]) { (acc, maybeTree) =>
              maybeTree match {
                case Left(size) => {
                  val (head, tail) = acc.splitAt(size)
                  Branch(head.toSet) :: tail
                }
                case Right(value) => value :: acc
              }
            }
        }
      loop(List(f(a)), Nil).head
    }
  }

  import cats.UnorderedFoldable
  import cats.kernel.CommutativeMonoid
  implicit val treeFoldable = new UnorderedFoldable[Tree] {
    import cats.instances.set._

    def unorderedFoldMap[A, B](fa: Tree[A])
      (f: A => B)(implicit B: CommutativeMonoid[B]): B = fa match {
      case Branch(values) =>
        B.combineAll(values.toList.map(unorderedFoldMap(_)(f)))
      case Leaf(value) => f(value)
    }

    // Unordered traverse functions:

    // def unorderedTraverse[G[_], A, B](fa: Tree[A])
    //   (f: A => G[B])(implicit G: CommutativeApplicative[G]): G[Tree[B]] =
    //   fa match {
    //     case Leaf(value) => G.map(f(value))(Leaf(_))
    //     case Branch(values) => values.foldLeft(G.pure(Tree.branch(Set.empty[Tree[B]]))) { (acc, a) =>
    //       Apply[G].map2(acc, unorderedTraverse(a)(f))(CommutativeMonoid[Tree[A]].combine)
    //     }
    //   }

    // Traverse functions:

    // def foldLeft[A, B](fa: Tree[A], b: B)(f: (B, A) => B): B =
    //   fa match {
    //     case Leaf(value) => f(b, value)
    //     case Branch(_) => ???
    //   }

    // def foldRight[A, B](fa: Tree[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
    //   fa match {
    //     case Leaf(value) => f(value, lb)
    //     case Branch(values) => ???
    //   }

    // def traverse[G[_], A, B](fa: Tree[A])
    //   (f: A => G[B])(implicit G: CommunicativeApplicative[G]): G[Tree[B]] =
    //   fa match {
    //     case Leaf(value) => G.map(f(value))(Leaf(_))
    //     case Branch(values) => ???
    //   }
  }

  // def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: Set[A])(f: A => G[B]): G[Set[B]] =
  //   sa.foldLeft(Applicative[G].pure(Set.empty[B])) { (acc, a) =>
  //     Apply[G].map2(acc, f(a))(_ + _)
  //   }

  // def foldLeft[A, B](fa: List[A], b: B)(f: (B, A) => B): B =
  //       fa.foldLeft(b)(f)

  //     def foldRight[A, B](fa: List[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = {
  //       def loop(as: List[A]): Eval[B] =
  //         as match {
  //           case Nil    => lb
  //           case h :: t => f(h, Eval.defer(loop(t)))
  //         }
  //       Eval.defer(loop(fa))
  //     }

    //     def traverse[F[_]: Applicative, B](f: A => F[B]): F[Tree[B]] = this match {
    //   case Tree.Empty()         => Applicative[F].pure(Tree.Empty())
    //   case Tree.Branch(v, l, r) => Applicative[F].map3(f(v), l.traverse(f), r.traverse(f))(Tree.Branch(_, _, _))
    // }
}

// scala> val b = Branch[Int](Set(Branch(Set(Leaf(1), Leaf(2)))))
// b: io.github.montokapro.collections.Delta.Branch[Int] = Branch(Set(Branch(Set(Leaf(1), Leaf(2)))))

// scala> import cats.Functor
// import cats.Functor

// scala> Functor[Tree].map(b)(_ + 1)
// res3: io.github.montokapro.collections.Delta.Tree[Int] = Branch(Set(Leaf(2), Leaf(3)))

// scala> import cats.Monad
// import cats.Monad

// scala> Monad[Tree].flatMap(b)(a => Leaf(a))
// res1: io.github.montokapro.collections.Delta.Tree[Int] = Branch(Set(Branch(Set(Leaf(1), Leaf(2)))))

// scala> Monad[Tree].flatMap(b)(a => Branch[Int](Set(Leaf(a), Leaf(a + 1))))
// res8: io.github.montokapro.collections.Delta.Tree[Int] = Branch(Set(Branch(Set(Branch(Set(Leaf(1), Leaf(2))), Branch(Set(Leaf(2), Leaf(3)))))))

// scala> UnorderedFoldable[Tree].unorderedFold(Branch(Set(Tree.branch(Set(Tree.leaf(2), Tree.leaf(3))), Tree.leaf(7))))
// res10: Int = 12
