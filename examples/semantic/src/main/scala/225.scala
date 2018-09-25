package ticket225

package object foo {
  type Foo = Int
}

package foo {
  class C {
    def foo: Foo = ???
  }
}
