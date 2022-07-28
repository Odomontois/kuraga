package dailytip.intermediate

import scala.collection.immutable.ArraySeq

extension [A](xs: List[A])
  def toIntArraySeq[B >: A <: Int]: ArraySeq[Int]    =
    ArraySeq.from[Int](xs)
  def toLongArraySeq[B >: A <: Long]: ArraySeq[Long] =
    ArraySeq.from[Long](xs)

val xs: List[1 | 2] = List(1, 2, 1)

//xs.toIntArraySeq

//xs.toLongArraySeq // produces error
