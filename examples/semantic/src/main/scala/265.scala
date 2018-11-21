package ticket265

package object bar {
}

package bar.bar {
  class C
  import ticket265.bar.bar.C
}
