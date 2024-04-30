package kuraga.treematch

import playground.chat.vec.Vec

enum Lambda:
    case Var(name: String)
    case Abs(param: String, body: Lambda)
    case App(fun: Lambda, arg: Lambda)

case class LambdaMap[+A](
    varMap: Map[String, A],
    boundMap: Vector[A],
    absMap: Map[String, LambdaMap[A]],
    appMap: LambdaMap[LambdaMap[A]] | Null,
):
    def search(l: Lambda): Option[A] = l match
        case Lambda.Var(name)        => varMap.get(name)
        case Lambda.Abs(param, body) => absMap.get(param).flatMap(_.search(body))
        case Lambda.App(fun, arg)    =>
            if appMap == null then None
            else appMap.search(fun).flatMap(_.search(arg))

    def insert[B](l: Lambda, b: B, bound: Map[String, Int] = Map.empty): LambdaMap[A | B] = l match
        case Lambda.Var(bound(pos))  =>
            val ix = boundMap.size - pos
            copy(boundMap = boundMap.updated(ix, b))
        case Lambda.Var(name)        => copy(varMap = varMap.updated(name, b))
        case Lambda.Abs(param, body) =>
            val inner = absMap.getOrElse(param, LambdaMap.empty).insert(body, b, bound.updated(param, bound.size))
            copy(absMap = absMap.updated(param, inner))
        case Lambda.App(fun, arg)    =>
            copy(appMap = appMap match
                case null => LambdaMap.singleton(fun, LambdaMap.singleton(arg, b))
                case _    => appMap.insert(fun, LambdaMap.singleton(arg, b))
            )

    def unionWith[B, C](that: LambdaMap[B])(f: (A, B) => C): LambdaMap[A | B | C] =
        LambdaMap(
          LambdaMap.unionWith(varMap, that.varMap)(f),
          LambdaMap.unionVecWith(boundMap, that.boundMap)(f),
          LambdaMap.unionWith(absMap, that.absMap)((a, b) => a.unionWith(b)(f)),
          if appMap == null then that.appMap
          else if that.appMap == null then appMap
          else appMap.unionWith(that.appMap)(_.unionWith(_)(f))
        )

    def matches(term: Lambda): LazyList[(Lambda, Map[String, Lambda])] = ???
end LambdaMap

object LambdaMap:
    val empty: LambdaMap[Nothing] = LambdaMap(Map.empty, Vector.empty, Map.empty, null)

    def unionWith[K, A, B, C](m1: Map[K, A], m2: Map[K, B])(f: (A, B) => C): Map[K, A | B | C] =
        val commonKeys = m1.keySet & m2.keySet
        val old        = (m1 -- commonKeys) ++ (m2 -- commonKeys)
        val merged     = commonKeys.iterator.map(k => (k, f(m1(k), m2(k)))).toMap
        old ++ merged

    def unionVecWith[A, B, C](v1: Vector[A], v2: Vector[B])(f: (A, B) => C): Vector[A | B | C] =
        val commonSize = v1.size.min(v2.size)
        val remain     = v1.drop(commonSize) ++ v2.drop(commonSize)
        val common     = v1.take(commonSize).lazyZip(v2).map(f)
        common ++ remain

    def singleton[A](l: Lambda, a: A): LambdaMap[A] = LambdaMap.empty.insert(l, a)
end LambdaMap
