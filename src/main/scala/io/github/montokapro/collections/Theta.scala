// package io.github.montokapro.collections

// import cats.kernel.PartialOrder
// import algebra.lattice.GenBool

// object Theta {
//   class PosetGenBool[A](
//     po: PartialOrder[A],
//     genBool: GenBool[A]
//   ) extends GenBool[List[A]] {
//     import cats.instances.list._

//     def zero: List[A] = List.empty

//     def or(a: List[A], b: List[A]): List[A] = {
//       val r = a.foldLeft(b)((c, d) => c.filterNot(po.gt(_, d))).union(
//         b.foldLeft(a)((c, d) => c.filterNot(po.gteqv(_, d)))
//       )
// //      println(s"pgb-join $a $b -> $r")
//       r
//     }

//     def and(a: List[A], b: List[A]): List[A] = {
//       val r = a.foldLeft(b)((c, d) => c.filter(po.lt(_, d))).union(
//         b.foldLeft(a)((c, d) => c.filter(po.lteqv(_, d)))
//       )
// //      println(s"pgb-meet $a $b -> $r")
//       r
//     }

//     def without(a: List[A], b: List[A]): List[A] = {
//       val r = b.foldLeft(a)((acc, d) => {
//         val r2 = acc.map(genBool.without(_, d))
// //        println(s"pgb-without-2 $acc $d -> $r2")
//         r2
//       })
// //      println(s"pgb-without $a $b -> $r")
//       r
//     }
//   }

//   def without(ls: List[A], rs: List[A])(implicit po: PartialOrder): List[A] = {
//     private def withoutOne(ls: List[A], r: A) = {
      
//       ls.flatMap()
//     }

//     neg.foldLeft(pos)((acc, d) => {
//         val r2 = acc.map(genBool.without(_, d))
// //        println(s"pgb-without-2 $acc $d -> $r2")
//         r2
//       })

//   }

//   class PoGenBool[A](
//     po: PartialOrder[A]
//   ) extends GenBool[List[A]] {
//     import cats.instances.list._

//     def zero: List[A] = List.empty

//     def or(a: List[A], b: List[A]): List[A] = {
//       val r = a.foldLeft(b)((c, d) => c.filterNot(po.gt(_, d))).union(
//         b.foldLeft(a)((c, d) => c.filterNot(po.gteqv(_, d)))
//       )
// //      println(s"pgb-join $a $b -> $r")
//       r
//     }

//     def and(a: List[A], b: List[A]): List[A] = {
//       val r = a.foldLeft(b)((c, d) => c.filter(po.lt(_, d))).union(
//         b.foldLeft(a)((c, d) => c.filter(po.lteqv(_, d)))
//       )
// //      println(s"pgb-meet $a $b -> $r")
//       r
//     }

//     def without(a: List[A], b: List[A]): List[A] = {
//       val r = b.foldLeft(a)((acc, d) => {
//         val r2 = acc.map(genBool.without(_, d))
// //        println(s"pgb-without-2 $acc $d -> $r2")
//         r2
//       })
// //      println(s"pgb-without $a $b -> $r")
//       r
//     }
//   }
// }
