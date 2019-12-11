package step1

import scala.util.matching.Regex

object Reader {
  val MAL_PATTERN: Regex = "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)".r
  type Token = String

  def readString(input: String): MalType =
    readForm(tokenize(input))._1

  def tokenize(input: String): List[Token] =
    MAL_PATTERN.findAllMatchIn(input).toList.map(_.group(1))

  def readList(result: MalList, tokens: List[Token]): (MalList, List[Token]) = {
    tokens match {
      case t :: ts if t == ")" => (result, ts)
      case t :: _ if t != ")" =>
        val (form, remainingTokens) = readForm(tokens)
        readList(result.append(form), remainingTokens)
      case Nil => throw new IllegalStateException(".*(EOF|end of input|unbalanced).*")
    }
  }

  def readAtom(tokens: List[Token]): (MalType, List[Token]) = tokens match {
    case t :: ts => (MalSymbol(t), ts)
    case Nil => throw new IllegalStateException("No atom in empty token list.")
  }

  def readForm(tokens: List[Token]): (MalType, List[Token]) = {
    tokens match {
      case t :: ts if t == "(" => readList(MalList(), ts)
      case _ => readAtom(tokens)
    }
  }
}
