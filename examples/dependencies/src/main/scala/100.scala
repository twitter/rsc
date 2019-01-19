package ticket100

trait T1 {
  def x: Int = ???
}

trait T2 extends T1 {
  private def super$x(self: T1): Int = ???
  def y: Int = super.x
}

trait HasRemoteInfo extends Exception {
  private def super$getMessage(self: java.lang.Throwable)(): String = ???
  def exceptionMessage(): String = super.getMessage()
}
