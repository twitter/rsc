package ticket404

class C {
  def m: List[Map[T, U] forSome { type U }] forSome { type T } = ???
}
