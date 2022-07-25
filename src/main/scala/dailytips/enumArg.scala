package dailytips.enumArg

enum Category:
  case Fruit, Vegetable, Cooked

import dailytips.enumArg.Category.*

enum Food[C <: Category & Singleton](val category: C):
  case Apple      extends Food(Fruit)
  case Pear       extends Food(Fruit)
  case Tomato     extends Food(Vegetable)
  case Zucchini   extends Food(Vegetable)
  case Tofu       extends Food(Cooked)
  case SpringRoll extends Food(Cooked)
end Food

// note that pattern matching is exhaustive
def fruitName(food: Food[Fruit.type]) = food match
  case Food.Pear  => "Peer"
  case Food.Apple => "Appel"
//  case Food.Tomato => "Tomaat" // Error: Tomato is not a fruit

@main def foo() = println(Food.valueOf("Pear"))
