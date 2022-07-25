package kuraga

case class Person[R](
    firstName: R,
    lastName: R,
    age: R,
    tags: R,
)

// trait Functor[F[_]]:
//     extension [A, B](fa: F[A])
//         def map(f: A => B): F[B]

given Functor[Person] with
    extension [A](fa: Person[A])
        def map[B](f: A => B): Person[B] =
            Person(
                f(fa.firstName),
                f(fa.lastName),
                f(fa.age),
                f(fa.tags),
            )    

// trait Pure[F[_]]:
//     def pure[A](a: A): F[A] 

// given Pure[Person] with
//     def pure[A](a: A): Person[A] = 
//         Person(
//                 a,
//                 a,
//                 a,
//                 a
//             )  

// trait Apply[F[_]]:
//   extension [A, B, C](fa: F[A])
//     def map2(fb: F[B])(f: (A, B) => C): F[C]
        
given Applicative[Person] with Traverse[Person] with
    def pure [A](a: A) : Person[A] = 
        Person(
                a,
                a,
                a,
                a
            )  


    extension [A, B, C](fa: Person[A])
        def map2(fb: Person[B])(f: (A, B) => C): Person[C] =
            Person(
                f(fa.firstName, fb.firstName),
                f(fa.lastName, fb.lastName),
                f(fa.age, fb.age),
                f(fa.tags, fb.tags)
            )  

    extension [A, B, F[_]](ta: Person[A])
        def traverse(f: A => F[B])(using F: Applicative[F]): F[Person[B]] = 
            f(ta.firstName).map4(
                f(ta.lastName),
                f(ta.age),
                f(ta.tags)
            )(Person(_, _, _, _))
    


