package playground.chat.oop

trait Named:
    def name: String

abstract class Animal extends Named:
    def description: String                     = s"a $kind with name $name"
    protected def kind: String
    protected def canEat(food: String): Boolean = false
    def eatAll(food: LazyList[String]): Int     = food match
        case head #:: rest if canEat(head) => 1 + eatAll(rest)
        case _                             => 0

trait CanEatTasty extends Animal:
    override protected def canEat(food: String): Boolean = food.contains("tasty") || super.canEat(food)

trait Runnable:
    def speed: Double

class Dog(val name: String) extends Animal with Runnable:
    def kind                                    = "Dog"
    def speed: Double                           = 10.0
    override protected def canEat(food: String) = food.contains("dog") || super.canEat(food)

trait DescribeBetter extends Animal:
    override def description: String = s""

type Clazz[A] = (=> A) => A

def fix[A](f: (=> A) => A): A =
    lazy val x: A = f(x)
    x

def mix[A](clses: Clazz[A]*): Clazz[A] = self => clses.foldLeft(self)((acc, cls) => cls(acc))

case class NamedIf(name: () => String)

case class AnimalPub(
    named: () => NamedIf,
    description: () => String,
    kind: () => String,
    eatAll: LazyList[String] => Int
)

case class AnimalCls(
    pub: () => AnimalPub,
    canEat: String => Boolean
)

case class RunnableIf(speed: () => Double)

case class DogCls(
    animal: () => AnimalCls,
    runnable: () => RunnableIf
):
    def pub: DogPub = DogPub(() => animal().pub(), runnable)
end DogCls

case class DogPub(
    animal: () => AnimalPub,
    runnable: () => RunnableIf
)

val animal: Clazz[AnimalCls] = self =>
    AnimalCls(
      pub = () =>
          def sup = self.pub()
          AnimalPub(
            named = () => sup.named(),
            description = () => s"a ${sup.kind()} with name ${sup.named().name()}",
            kind = () => sup.kind(),
            eatAll = food =>
                food match
                    case head #:: rest if self.canEat(head) => 1 + self.pub().eatAll(rest)
                    case _                                  => 0
          ),
      canEat = food => false
    )

val canEatTasty: Clazz[AnimalCls] = self =>
    AnimalCls(
      pub = () => self.pub(),
      canEat = food => food.contains("tasty") || self.canEat(food)
    )

def dog(dogName: String, parent: Clazz[AnimalCls] = animal): Clazz[DogCls] = self =>
    lazy val animal = parent(self.animal())
    DogCls(
      animal = () =>
          AnimalCls(
            pub = () =>
                AnimalPub(
                  named = () => NamedIf(() => dogName),
                  description = () => animal.pub().description(),
                  kind = () => "Dog",
                  eatAll = s => animal.pub().eatAll(s)
                ),
            canEat = food => food.contains("dog") || animal.canEat(food)
          ),
      runnable = () => RunnableIf(() => 10.0)
    )

case class GooseMixin(
    animal: () => AnimalCls
)

lazy val dogFood: LazyList[String] =
    val start = LazyList("hot dog", "tasty pizza", "green broccoli", "french fries")
    start #::: dogFood

@main def classic() =
    val dg = new Dog("Sparky") with CanEatTasty
    println(dg.description)

    val amountEaten = dg.eatAll(dogFood)
    println(s"Amount of food eaten: $amountEaten")

@main def oofp() =
    val dg = fix(dog("Sparky", mix(animal, canEatTasty)))
    println(dg.animal().pub().description())

    val amountEaten = dg.animal().pub().eatAll(dogFood)
    println(s"Amount of food eaten: $amountEaten")
