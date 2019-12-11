package step1

sealed trait MalType
case class MalList(items: Vector[MalType] = Vector()) extends MalType {
  def append(item: MalType): MalList = MalList(items.:+(item))
}
case class MalSymbol(value: String) extends MalType