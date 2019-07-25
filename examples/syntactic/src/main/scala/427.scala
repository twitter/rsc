object C {
  def foo(x: Int = 0) = 0
  foo(x = 1,
  )
  foo(
    x = 1, //Test of comments following trailing comma
  )
}
