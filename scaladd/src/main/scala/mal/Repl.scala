package mal
import mal.types.MalType

import scala.util.{Failure, Success, Try}

/**
 * Contains the functions needed to build a Read Eval Print Loop.
 */
trait Repl extends Read with Evaluate with Print {

  /**
   * Read input and convert it to a [[MalType]].
   * @param input the input from the Repl.
   * @return [[Success]] containing the [[MalType]] corresponding to the input, [[Failure]] otherwise.
   */
  def read(input: String): Try[MalType] = Try(readString(input))

  /**
   * Evaluate a [[MalType]] to obtain the resulting output.
   * @param input the input data structure.
   * @return [[Success]] containing the [[MalType]] containing the result of the evaluations, [[Failure]] otherwise.
   */
  def eval(input: MalType): Try[MalType] = evalAst(input)

  /**
   * Prints the result of [[eval]] to stdin when successful or to stderr in case of an exception.
   * @param input the input data structure.
   */
  def print(input: Try[MalType]): Unit =
    input match {
      case Success(malType) =>
        println(printString(malType))
      case Failure(exception) => System.err.println(exception.getMessage)
    }

  /**
   * Combine [[read]], [[eval]] and [[print]].
   * @param inputString the input string.
   */
  def rep(inputString: String): Unit = {
    val output: Try[MalType] = for {
      i <- read(inputString)
      o <- eval(i)
    } yield o
    print(output)
  }

}