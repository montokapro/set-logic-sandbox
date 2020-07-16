package potree

import org.scalatest.FunSpec
import cats.Functor
import cats.Monad

class TreeMonadSpec extends FunSpec {
  import cats.syntax._
  import TreeMonad._

  def leaf(a: Int): Tree[Int] = Leaf(a)
  def or(as: Tree[Int]*): Tree[Int] = Or(as.toList)
  def and(as: Tree[Int]*): Tree[Int] = And(as.toList)

  it("map") {
    val actual = Functor[Tree].map(leaf(1))(_ + 1)
    val expected = leaf(2)
    assert(actual == expected)
  }

  it("flatMap") {
    val actual = Monad[Tree].flatMap(
      and(
        leaf(1),
        leaf(2)
      )
    )(value => or(
        leaf(value),
        leaf(value + 1)
      )
    )
    val expected = and(
      or(
        leaf(1),
        leaf(2)
      ),
      or(
        leaf(2),
        leaf(3)
      )
    )
    assert(actual == expected)
  }
}
