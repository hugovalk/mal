package mal

import mal.types._

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

  def readForm(tokens: List[Token]): (MalType, List[Token]) = {
    tokens match {
      case t :: ts if t == "(" => buildSequence(MalList(), ts, ")")
      case t :: ts if t == "[" => buildSequence(MalVec(), ts, "]")
      case t :: ts if t == "{" => buildMap(MalMap(), ts)
      case t :: ts if t == "'" => buildQuoted(ts, MalSymbol("quote"))
      case t :: ts if t == "`" => buildQuoted(ts, MalSymbol("quasiquote"))
      case t :: ts if t == "~" => buildQuoted(ts, MalSymbol("unquote"))
      case t :: ts if t == "~@" => buildQuoted(ts, MalSymbol("splice-unquote"))
      case t :: ts if t == "@" => buildQuoted(ts, MalSymbol("deref"))
      case t :: ts if t == "^" => buildWithMeta(ts)
      case _ => buildAtom(tokens)
    }
  }

  def buildSequence(result: MalSeq, tokens: List[Token], endToken: Token): (MalSeq, List[Token]) = {
    tokens match {
      case t :: ts if t == endToken => (result, ts)
      case t :: _ if t != endToken =>
        val (form, remainingTokens) = readForm(tokens)
        buildSequence(result.append(form), remainingTokens, endToken)
      case Nil => throw new IllegalStateException(".*(EOF|end of input|unbalanced).*")
    }
  }

  def buildMap(result: MalMap, tokens: List[Token]): (MalMap, List[Token]) = {
    tokens match {
      case t :: ts if t == "}" => (result, ts)
      case t :: _ if t != "}" =>
        val (form1, remaining1) = readForm(tokens)
        remaining1 match {
          case t2 :: ts2 if t2 != "}" =>
            val (form2, remaining2) = readForm(remaining1)
            buildMap(result.+((form1, form2)), remaining2)
          case _ => throw new IllegalStateException(".*(EOF|end of input|unbalanced).*")
        }
      case Nil => throw new IllegalStateException(".*(EOF|end of input|unbalanced).*")
    }
  }

  def buildQuoted(tokens: List[Token], quoteSymol: MalSymbol): (MalType, List[Token]) = {
    val (result, remaining) = readForm(tokens)
    (MalList(Vector(quoteSymol, result)), remaining)
  }

  def buildWithMeta(tokens: List[Token]): (MalType, List[Token]) = {
    val (meta, remaining) = readForm(tokens)
    val (result, finalRemaining) = readForm(remaining)
    (MalList(MalSymbol("with-meta"), result, meta), finalRemaining)
  }

  def buildString(token: Token): MalString = {
    def invalidString(token: String): Boolean = {
      token == "\"" ||
        !token.endsWith("\"") ||
        !"^.*?([\\\\]*)\"$".r.findFirstMatchIn(token).forall(_.group(1).length % 2 == 0)
    }

    if (invalidString(token))
      throw new IllegalStateException(".*(EOF|end of input|unbalanced).*")
    else
      MalString(token)
  }

  def buildNumber(token: Token): MalNumber =
    MalNumber(BigDecimal(token))

  def buildAtom(tokens: List[Token]): (MalType, List[Token]) = tokens match {
    case t :: ts if t.startsWith("\"") => (buildString(t), ts)
    case t :: ts if t.matches("[0-9.]+") => (buildNumber(t), ts)
    case t :: ts => (MalSymbol(t), ts)
    case Nil => throw new IllegalStateException("No atom in empty token list.")
  }
}
