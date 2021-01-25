// import scala.util.control.NonFatal

// type && [F[_], G[_]] = [x] =>> F[x] & G[x]

// trait CombinatorErrors[E] {
//   def tupleLeft(e: E): E
//   def tupleRight(e: E): E
//   def tupleBoth(e1: E, e2: E): E
// }

// trait Reader[-C[_], +A] { A =>:
//   def read[E](s: String)(implicit E: C[E]): Either[E, A]
// }

// object Reader {
//   given ReaderOps {
//     extension (A: Reader[C, A]) def zip[C[_], A, K[_], B](B: Reader[K, B]): Reader[C && K && CombinatorErrors, (A, B)] = 
//       new Reader[C && K && CombinatorErrors, (A, B)] {
//         def read[E](s: String)(implicit ev: (C && K && CombinatorErrors)[E]): Either[E, (A, B)] = 
//           (A.read(s), B.read(s)) match {
//             case (Left(e1), Left(e2)) => Left(ev.tupleBoth(e1, e2))
//             case (Left(e1), Right(_)) => Left(ev.tupleLeft(e1))
//             case (Right(_), Left(e2)) => Left(ev.tupleRight(e2))
//             case (Right(a), Right(b)) => Right((a, b))
//           }
//       }
//   }

//   given as Reader[IntErrors, Int] {
//   def read[E](s: String)(implicit E: IntErrors[E]): Either[E, Int] =
//     try Right(s.toInt) catch {
//       case NonFatal(e) => Left(E.invalidInt(s))
//     }
//  }

//   given longReader :  Reader[LongErrors, Long] {
//     def read[E](s: String)(implicit E: LongErrors[E]): Either[E, Long] =
//         try Right(s.toLong) catch {
//         case NonFatal(e) => Left(E.invalidLong(s))
//         }
//     }
// }

// trait IntErrors[E] {
//   def invalidInt(s: String): E
// }



// trait LongErrors[E] {
//   def invalidLong(s: String): E
// }


// trait DoubleErrors[E] {
//   def invalidDouble(s: String): E
// }

// given doubleReader :  Reader[DoubleErrors, Double] {
//   def read[E](s: String)(implicit E: DoubleErrors[E]): Either[E, Double] =
//     try Right(s.toDouble) catch {
//       case NonFatal(e) => Left(E.invalidDouble(s))
//     }
// }
