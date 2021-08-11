import scala.reflect.ClassTag

class Menoxe[F[_]](using val ct: ClassTag[F[Any]])

object Menoxe:
    def derived[F[_]](using ClassTag[F[Any]]): Menoxe[F] = new{}

// object Karnava:
//     opaque type Karnava[A, B] derives Menoxe = String

// summon[Menoxe[Karnava.Karnava[Int, *]]].ct