package playground.data

enum Nat:
    case Z
    case S[A <: Nat]()


object BT:
    type BT[N <: Nat, +A] = N match
        case Nat.Z.type => A
        case Nat.S[n]   => IArray[BT[n, A]]




