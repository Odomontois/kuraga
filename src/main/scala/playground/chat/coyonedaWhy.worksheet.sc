import cats.free.Coyoneda
type UserId = String
type OrderId = String

enum AuthRequest[A] {
    case ForUser(userId: UserId)                       extends AuthRequest[UserId]
    case ForOrder(order: OrderId)                      extends AuthRequest[OrderId]
    case ForGroup[A](requests: Vector[AuthRequest[A]]) extends AuthRequest[A]

    def items: Vector[A] = this match {
        case ForUser(id)    => Vector(id)
        case ForOrder(id)   => Vector(id)
        case ForGroup(reqs) => reqs.flatMap(_.items)
    }
}

type OrderInfo = String
type OrderOwner = String
def getOrderInfo(orderId: OrderId): OrderInfo       = ???
def getOrderOwner(orderInfo: OrderInfo): OrderOwner = ???

def getInfos(req: Coyoneda[AuthRequest, OrderId]): Coyoneda[AuthRequest, OrderInfo] = req.map(getOrderInfo)

def getOrderOwners(req: Coyoneda[AuthRequest, OrderInfo]): Coyoneda[AuthRequest, OrderOwner] = req.map(getOrderOwner)

1 + 3
