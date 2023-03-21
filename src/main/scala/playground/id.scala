package playground
opaque type Id[+A] = A

object Id:
  def apply[A](x: A): Id[A] = x

opaque type Id2[A] = A

object Id2:
  def apply[A](x: A): Id2[A] = x

enum Eit[+X, +Y]:
  case L[+X](x: X) extends Eit[X, Nothing]
  case R[+Y](y: Y) extends Eit[Nothing, Y]

case class I[+A](a: A)

object IdTest extends App:
  if (1 < 2) Id(Left("1")) else Id(Right(2))
  if (1 < 2) Id2(Left("1")) else Id2(Right(2))
  if (1 < 2) I(Left("1")) else I(Right(2))
  if (1 < 2) Id(Eit.L("1")) else Id(Eit.R(2))
  if (1 < 2) Id2(Eit.L("1")) else Id2(Eit.R(2))
  if (1 < 2) I(Eit.L("1")) else I(Eit.R(2))
  
