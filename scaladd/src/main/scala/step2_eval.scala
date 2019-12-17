import scala.io.StdIn

object step2_eval extends App with Repl{
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
