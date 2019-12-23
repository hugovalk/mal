package mal

import mal.types._

/**
 * Contains functions necessary to convert a [[MalType]] to [[String]].
 */
trait Print {
  def printString(malType: MalType): String = malType match {
    case MalList(vector) => s"(${vector.map(printString).mkString(" ")})"
    case MalVec(vector) => s"[${vector.map(printString).mkString(" ")}]"
    case MalMap(map) => s"{${map.map{case (k, v) => s"${printString(k)} ${printString(v)}"}.mkString(" ")}}"
    case MalSymbol(value) => value
    case MalString(value) => value
    case MalNumber(value) => s"$value"
    case MalNil => "nil"
  }
}
