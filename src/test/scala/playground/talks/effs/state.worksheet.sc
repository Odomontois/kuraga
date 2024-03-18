trait State[S, +A] {
    def run(s: S): (S, A)

    def map[B](f: A => B): State[S, B] = { s =>
        val (s1, a) = run(s)
        (s1, f(a))
    }

    def flatMap[B](f: A => State[S, B]) = { s =>
        val (s1, a) = run(s)
        f(a).run(s1)
    }
}

object State {
    def apply[S, A](a: A): State[S, A] = s => (s, a)

    def get[S]: State[S, S] = s => (s, s)

    def set[S](s: S): State[S, Unit] = _ => (s, ())

    def update[S](f: S => S): State[S, Unit] = s => (f(s), ())
}

def addClient(client: Client): State[WowEnterpriseData, Unit] = for {
    data   <- State.get
    clients = data.clients.updated(client.id, client)
    _      <- State.set(data.copy(clients = clients))
} yield ()

def addProduct(product: Product): State[WowEnterpriseData, Unit] = for {
    data    <- State.get
    products = data.products.updated(product.id, product)
    _       <- State.set(data.copy(products = products))
} yield ()

def addUsage(clientId: ClientId, productId: ProductId): State[WowEnterpriseData, Unit] = for {
    data <- State.get
    usage = data.usage + ((clientId, productId))
    _    <- State.set(data.copy(usage = usage))
} yield ()

def suchEnterpiseLogic(client: Client, product: Product): State[WowEnterpriseData, Unit] = for {
    _ <- addClient(client)
    _ <- addProduct(product)
    _ <- addUsage(client.id, product.id)
} yield ()

type ProductId
type ClientId

case class Cliend(id: ClientId)
case class Product(id: ProductId)

class WowEnterpriseData(
    var clients: Map[ClientId, Cliend],
    var products: Map[ProductId, Product],
    var usage: Set[(ClientId, ProductId)]
)
