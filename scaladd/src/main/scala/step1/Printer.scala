package step1

object Printer {
  def printString(malType: MalType): String = malType match {
    case MalList(vector) => s"(${vector.map(printString).mkString(" ")})"
    case MalVec(vector) => s"[${vector.map(printString).mkString(" ")}]"
    case MalSymbol(value) => value
  }
}
