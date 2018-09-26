package ticket94

class C {
  type X = List[T] forSome { type T }
  val x: List[T] forSome { type T } = ???
}
