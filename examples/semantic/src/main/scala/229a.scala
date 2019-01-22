package ticket229

import java.util.concurrent.Future

trait C extends Future[Int] {
  import java.util.concurrent.Future

  def m: Future[Int] = ???
}
