package kuraga

package object datum:
  type Expr[-P[-_, +_]] = Star[P, Nothing]
