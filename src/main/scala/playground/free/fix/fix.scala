// package playground
// package free.fix


// type |@[F[+_], G[+_]] = [a] =>> F[a] | G[a]

// object Fix:
//   opaque type T[+F[+_]] = ApplyFix.T[F]

//   def apply[F[+_]](f: F[Fix[F]]): T[F] = ApplyFix(f)

//   extension [F[+_]](fix: T[F]):
//     def value: F[Fix[F]] = ApplyFix.unwrap(fix)

//   object ApplyFix:
//     opaque type T[+F[+_]] = F[Fix[F]]

//     def apply[F[+_]](f: F[Fix[F]]): T[F] = f

//     def unwrap[F[+_]](v: T[F]): F[Fix[F]] = v  

// type Fix[+F[+_]] = Fix.T[F]



