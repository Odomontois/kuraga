package playground

type Lol = [a, b <: a] =>> b

type Uncurry[t <: [a] =>> [b] =>> Any] = [a, b] =>> t[a][b]
