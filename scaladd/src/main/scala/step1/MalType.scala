package step1

sealed trait MalType

sealed trait MalSeq extends MalType {
  def append(item: MalType): MalSeq
}
case class MalList(items: Vector[MalType] = Vector()) extends MalSeq {
  def append(item: MalType): MalSeq = MalList(items.:+(item))
}
case class MalVec(items: Vector[MalType] = Vector()) extends MalSeq {
  def append(item: MalType): MalSeq = MalVec(items.:+(item))
}

case class MalSymbol(value: String) extends MalType
case object MalNil extends MalType