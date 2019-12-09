import scala.io.StdIn

trait Repl {

  def read(input: String): Option[String] = Some(input)

  def eval(input: String): Option[String] = Some(input)

  def print(input: String) = println(input)

  def rep(input: String) =
    for {
      input <- read(input)
      output <- eval(input)
    } yield (print(output))

}

object step0_repl extends App with Repl{
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
