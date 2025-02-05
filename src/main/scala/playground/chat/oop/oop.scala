package playground.chat.oop

trait Named:
    def name: String

abstract class Animal extends Named:
    def description: String                     = s"$kind of name $name"
    protected def kind: String
    protected def canEat(food: String): Boolean = false
    def eatAll(food: LazyList[String]): Int     = food match
        case head #:: rest if canEat(head) => 1 + eatAll(rest)
        case _                             => 0

trait Runnable:
    def speed: Double

class Dog(val name: String) extends Animal with Runnable:
    def kind                                             = "Dog"
    def speed: Double                                    = 10.0
    override protected def canEat(food: String): Boolean = food.contains("dog") || food.contains("tasty")

def fix[A](f: (=> A) => A): A =
    lazy val x: A = f(x)
    x

type Clazz[A] = (=> A) => A
case class Extends[A, B](sub: A => B, extension: Clazz[B] => Clazz[B])

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

val animalClass: Clazz[AnimalCls] = self =>
    AnimalCls(
      pub = () =>
          AnimalPub(
            named = () => self.pub().named(),
            description = () => s"a ${self.pub().kind()} with name ${self.pub().named().name()}",
            kind = () => self.pub().kind(),
            eatAll = food =>
                food match
                    case head #:: rest if self.canEat(head) => 1 + self.pub().eatAll(rest)
                    case _                                  => 0
          ),
      canEat = food => false
    )

def dogClass(dogName: String): Clazz[DogCls] = self =>
    lazy val animal = animalClass(self.animal())
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
            canEat = food => food.contains("dog") || food.contains("tasty")
          ),
      runnable = () => RunnableIf(() => 10.0)
    )

@main def lol() =
    val dg                                  = fix(dogClass("Sparky"))
    println(dg.animal().pub().description())
    val start                               = LazyList("hot dog", "tasty pizza", "broccoly")
    lazy val infiniteFood: LazyList[String] = start #::: infiniteFood
    val amountEaten                         = dg.animal().pub().eatAll(infiniteFood)
    println(s"Amount of food eaten: $amountEaten")
