import scala.io.StdIn
import step1.Repl

object step1_read_print extends App with Repl{
  def loop(): Unit = {
    StdIn.readLine("user> ") match {
      case s: String =>
        rep(s)
        loop()
      case null => println("Bye...")
    }
  }
  loop()
}
