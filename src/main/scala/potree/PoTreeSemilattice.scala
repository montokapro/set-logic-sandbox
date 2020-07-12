// package potree

// import cats.kernel.Group
// import cats.kernel.PartialOrder
// import cats.kernel.BoundedSemilattice

// class PoTreeSemilattice[A](
//   po: PartialOrder[A],
//   in: Inverse[A],
//   group: Group[A]
// ) extends BoundedSemilattice[Tree[A]] {
//   import cats.syntax.eq._

//   def empty: List[A] = List.empty

//   def combine(a: List[A], b: List[A]): List[A] = {
//     a.foldLeft(b)((acc, neg) => {
//       acc.filterNot(pos => eq.eqv(pos, neg))
//     }) ++
//     b.foldLeft(a)((acc, neg) => {
//       acc.filterNot(pos => eq.eqv(pos, neg))
//     })
//   }
// }
