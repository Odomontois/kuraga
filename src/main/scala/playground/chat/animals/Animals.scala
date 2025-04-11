package playground.chat.animals

import language.experimental.modularity
import scala.deriving.Mirror
import compiletime.testing.typeCheckErrors

enum Animal: 
  case Cat, Dog

val animir = summon[Mirror.SumOf[Animal]]
type Animals = Tuple.Union[animir.MirroredElemTypes]

enum Food[a <: Animal & Singleton](val animal: a):
    case Cat extends Food(Animal.Cat)
    case Dog extends Food(Animal.Dog)

type MeinDatum[a <: Animal & Singleton] = (a, Food[a])
class MyData(tracked val animal: Animal, val food: Food[animal.type]):
    def datum: MeinDatum[animal.type] = (animal, food)

object MyData:
    def unapply(data: MyData): MeinDatum[data.animal.type] = (data.animal, data.food)

val catData = MyData(Animal.Cat, Food.Cat)
val dogData = MyData(Animal.Dog, Food.Dog)
// val mixedData = MyData(Animal.Cat, Food.Dog)

// проверяет на исчерпанность
def matchDatum[a <: Animals](data: MeinDatum[a]): String = data match
  case (Animal.Cat, Food.Cat) => "cat"
  case (Animal.Dog, Food.Dog) => "dog"

// не проверяет на исчерпанность
def matchData(data: MyData): String = data match
  case MyData(Animal.Cat, Food.Cat) => "cat"
  case MyData(Animal.Dog, Food.Dog) => "dog"

@main def run() = println:
    s"""
        cat is ${matchDatum(catData.datum)}
        dog is ${matchDatum(dogData.datum)}
        ${typeCheckErrors("MyData(Animal.Cat, Food.Dog)").head} 
    """