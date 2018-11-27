package ticket91

class C1[T <: Enumeration] {
  def m: T#Value = ???
}

class C2 {
  def m: Enumeration#Value = ???
}
