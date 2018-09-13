class C {
  for {
    xs <- ???
    s = (xs map fn toSet)
    if s.nonEmpty
  } yield {
    ???
  }
}
