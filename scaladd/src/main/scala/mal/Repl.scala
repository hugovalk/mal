package mal
import Reader.readString
import Printer.printString
import mal.types.MalType

import scala.util.{Failure, Success, Try}

trait Repl {

  def read(input: String): Try[MalType] = Try(readString(input))

  def eval(input: MalType): Try[MalType] = Try(input)

  def print(input: Try[MalType]): Unit = input match {
    case Success(malType) => println(printString(malType))
    case Failure(exception) => System.err.println(exception.getMessage)
  }

  def rep(input: String): Unit =
    print(read(input).flatMap(eval))

}