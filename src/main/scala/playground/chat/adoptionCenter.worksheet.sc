trait AnimalTag


case class Cat(catProperty: String) extends Animal
case class Dog(dogProperty: String) extends Animal

trait AdoptionCenter[-A <: Animal] {
    def adopt(animal: A): Unit
}

class DogAdoptionCenter extends AdoptionCenter[Dog] {
    override def adopt(animal: Dog): Unit =
        println(s"dog with property:${animal.dogProperty}")
}
class CatAdoptionCenter extends AdoptionCenter[Cat] {
    override def adopt(animal: Cat): Unit =
        println(s"cat with property:${animal.catProperty}")
}

class AdoptionCenterService[B >: AdoptionCenter[Animal]](adoptionServices: Seq[B]) {

    // private def findFor[AC<:AdoptionCenter[Animal]](animal: Animal): AC = animal match {
    //  case c: Cat =>
    //    adoptionServices.collectFirst { case c: CatAdoptionCenter => c }.get//.asInstanceOf[AdoptionCenter[Animal]]
    //  case d: Dog =>
    //    adoptionServices.collectFirst { case d: DogAdoptionCenter => d }.get//.asInstanceOf[AdoptionCenter[Animal]]
    // }

    private def findForCat(): CatAdoptionCenter = adoptionServices.collectFirst { case c: CatAdoptionCenter => c }.get

    private def findForDog(): DogAdoptionCenter = adoptionServices.collectFirst { case d: DogAdoptionCenter => d }.get

    def adoptAll(animals: Animal*) = animals.foreach {
        case c: Cat => findForCat().adopt(c)
        case d: Dog => findForDog().adopt(d)
    }
}

val sdac = new DogAdoptionCenter()
val scac = new CatAdoptionCenter()
val acs  = new AdoptionCenterService(Seq(sdac, scac))

acs.adoptAll(new Dog("ushi"), new Cat("hvost"))

class AdoptionCenterController(service: AdoptionCenterService[AdoptionCenter[Animal]]) {
    def endpointLogic() = service.adoptAll(new Dog("ushi"), new Cat("hvost"))
}
//type mismatch;
// found   : Playground.AdoptionCenterService[Playground.AdoptionCenter[Playground.Cat with Playground.Dog]]
// required: Playground.AdoptionCenterService[Playground.AdoptionCenter[Playground.Animal]]
//new AdoptionCenterController(acs)

class AdoptionCenterService2(adoptionServices: Seq[AdoptionCenter[Animal]]) {
    private def findForCat(): CatAdoptionCenter = adoptionServices.collectFirst { case c: CatAdoptionCenter => c }.get

    private def findForDog(): DogAdoptionCenter = adoptionServices.collectFirst { case d: DogAdoptionCenter => d }.get

    def adoptAll(animals: Animal*) = animals.foreach {
        case c: Cat => findForCat().adopt(c)
        case d: Dog => findForDog().adopt(d)
    }
}

val acs2 = new AdoptionCenterService2(
  Seq[AdoptionCenter[Animal]](sdac.asInstanceOf[AdoptionCenter[Animal]], scac.asInstanceOf[AdoptionCenter[Animal]])
)

acs2.adoptAll(new Dog("ushi"), new Cat("hvost"))

class AdoptionCenterController2(service: AdoptionCenterService2) {
    def endpointLogic() = service.adoptAll(new Dog("ushi"), new Cat("hvost"))
}

val controller = new AdoptionCenterController2(acs2)
controller.endpointLogic()
