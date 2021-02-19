package  playground.chat.kaivessel
import scala.compiletime.ops.int.*


type K0 = Any
type K1 = [A] =>> Any
type K2 = [f[_]] =>> Any
type K3 = [c[f[_]]] =>> Any
type * = AnyKind
type ^[k <: *] = [_ <: k] =>> Any
type +~[k <: *, n <: Int] = KindOps.+~[k, n] 


object KindOps:
    type +~[k <: *, n <: Int] = n match 
        case 0 => k
        case n => [_ <: (k +~ (n - 1))] =>> Any
    