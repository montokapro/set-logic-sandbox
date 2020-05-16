package io.github.montokapro.collections

object NaiveFuzzySet {
  // Debugging only
  implicit val fuzzyList: FuzzySet[List] = new FuzzySet[List] {
    def empty[A]: List[A] = List.empty[A]

    def add[A](fa: List[A], a: A): List[A] = a :: fa
  }
}
