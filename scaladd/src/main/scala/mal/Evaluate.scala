package mal

import mal.types.MalType

import scala.util.{Success, Try}

/**
 * Contains functions to evaluate a [[MalType]] abstract syntax tree (AST).
 */
trait Evaluate {

  /**
   * Recursively evaluate an AST.
   * @param ast the input AST structure.
   * @return the result of evaluation.
   */
  def evalAst(ast: MalType): Try[MalType] = Success(ast)

}
