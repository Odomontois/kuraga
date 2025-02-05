package playground.data.indexed

type MakeIndex[Line, IndexDef]                = Int
type IndexCollections[Line, Indices <: Tuple] = Tuple.Map[Indices, [I] =>> MakeIndex[Line, I]]

final case class Indexed[Line, Indices <: Tuple](
    lines: Vector[Line],
    indices: IndexCollections[Line, Indices],
)
