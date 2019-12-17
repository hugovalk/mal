package mal

import mal.types._

object Printer {
  def printString(malType: MalType): String = malType match {
    case MalList(vector) => s"(${vector.map(printString).mkString(" ")})"
    case MalVec(vector) => s"[${vector.map(printString).mkString(" ")}]"
    case MalMap(map) => s"{${map.map{case (k, v) => s"${printString(k)} ${printString(v)}"}.mkString(" ")}}"
    case MalSymbol(value) => value
    case MalNil => "nil"
  }
}
