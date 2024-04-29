package kuraga.treematch

enum Lambda:
    case Var(name: String)
    case Abs(param: String, body: Lambda)
    case App(fun: Lambda, arg: Lambda)

case class LambdaMap[+A](
    varMap: Map[String, A],
    absMap: Map[String, LambdaMap[A]],
    appMap: LambdaMap[LambdaMap[A]] | Null,
):
    def search(l: Lambda): Option[A] = l match
        case Lambda.Var(name)        => varMap.get(name)
        case Lambda.Abs(param, body) => absMap.get(param).flatMap(_.search(body))
        case Lambda.App(fun, arg)    =>
            if appMap == null then None
            else appMap.search(fun).flatMap(_.search(arg))

    def insert[B](l: Lambda, b: B): LambdaMap[A | B] = l match
        case Lambda.Var(name)        => copy(varMap = varMap.updated(name, b))
        case Lambda.Abs(param, body) =>
            copy(absMap = absMap.updated(param, absMap.getOrElse(param, LambdaMap.empty).insert(body, b)))
        case Lambda.App(fun, arg)    =>
            copy(appMap = appMap match
                case null => LambdaMap.singleton(fun, LambdaMap.singleton(arg, b))
                case _    => appMap.insert(fun, LambdaMap.singleton(arg, b))
            )
end LambdaMap

object LambdaMap:
    val empty: LambdaMap[Nothing] = LambdaMap(Map.empty, Map.empty, null)

    def singleton[A](l: Lambda, a: A): LambdaMap[A] = l match
        case Lambda.Var(name)        => LambdaMap(Map(name -> a), Map.empty, null)
        case Lambda.Abs(param, body) => LambdaMap(Map.empty, Map(param -> singleton(body, a)), null)
        case Lambda.App(fun, arg)    =>
            LambdaMap(Map.empty, Map.empty, LambdaMap.singleton(fun, LambdaMap.singleton(arg, a)))
