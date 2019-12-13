package step1

import scala.util.matching.Regex

object Reader {
  val MAL_PATTERN: Regex = "[\\s,]*(~@|[\\[\\]{}()'`~^@]|\"(?:\\\\.|[^\\\\\"])*\"?|;.*|[^\\s\\[\\]{}('\"`,;)]*)".r
  type Token = String

  def readString(input: String): MalType = {
    val (result, remainingTokens) = readForm(tokenize(input))
    if (remainingTokens.nonEmpty)
      throw new IllegalStateException(".*(EOF|end of input|unbalanced).*")
    result
  }

  def tokenize(input: String): List[Token] =
    MAL_PATTERN.findAllMatchIn(input).toList.map(_.group(1)).filter(t => t != "" && !t.startsWith(";"))

  def readSeq(result: MalSeq, tokens: List[Token], endToken: String): (MalSeq, List[Token]) = {
    tokens match {
      case t :: ts if t == endToken => (result, ts)
      case t :: _ if t != endToken =>
        val (form, remainingTokens) = readForm(tokens)
        readSeq(result.append(form), remainingTokens, endToken)
      case Nil => throw new IllegalStateException(".*(EOF|end of input|unbalanced).*")
    }
  }

  def invalidString(token: String): Boolean = {
    token == "\"" ||
      !token.endsWith("\"") ||
      !"^.*?([\\\\]*)\"$".r.findFirstMatchIn(token).forall(_.group(1).length % 2 == 0)
  }

  def readAtom(tokens: List[Token]): (MalType, List[Token]) = tokens match {
    case t :: _ if t.startsWith("\"") && invalidString(t) => throw new IllegalStateException(".*(EOF|end of input|unbalanced).*")
    case t :: ts => (MalSymbol(t), ts)
    case Nil => throw new IllegalStateException("No atom in empty token list.")
  }

  def quoting(tokens: List[Token], quoteSymol: MalSymbol): (MalType, List[Token]) = {
    val (result, remaining) = readForm(tokens)
    (MalList(Vector(quoteSymol, result)), remaining)
  }
  def meta(tokens: List[Token]): (MalType, List[Token]) = {
    val (meta, remaining) = readForm(tokens)
    val (result, finalRemaining) = readForm(remaining)
    (MalList(Vector(MalSymbol("with-meta"), meta, result)), finalRemaining)
  }

  def readForm(tokens: List[Token]): (MalType, List[Token]) = {
    tokens match {
      case t :: ts if t == "(" => readSeq(MalList(), ts, ")")
      case t :: ts if t == "[" => readSeq(MalVec(), ts, "]")
      case t :: ts if t == "'" => quoting(ts, MalSymbol("quote"))
      case t :: ts if t == "`" => quoting(ts, MalSymbol("quasiquote"))
      case t :: ts if t == "~" => quoting(ts, MalSymbol("unquote"))
      case t :: ts if t == "~@" => quoting(ts, MalSymbol("splice-unquote"))
      case t :: ts if t == "@" => quoting(ts, MalSymbol("deref"))
      case t :: ts if t == "^" => meta(ts)
      case _ => readAtom(tokens)
    }
  }
}
