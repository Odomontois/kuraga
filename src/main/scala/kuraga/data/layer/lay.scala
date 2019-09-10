package kuraga.data.layer

type Layer[-S[-_, +_]] = [a] => S[Layer[S], a] => a