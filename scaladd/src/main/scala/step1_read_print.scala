import scala.io.StdIn
import mal.Repl

object step1_read_print extends App with Repl{
  def loop(): Unit = {
    StdIn.readLine("user> ") match {
      case s: String =>
        print(read(s))
        loop()
      case null => println("Bye...")
    }
  }
  loop()
}
