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

case class MalMap(items: Map[MalType, MalType] = Map()) extends MalType {
  def +(kv: (MalType, MalType)): MalMap = MalMap(items.+(kv))
  def ++(m: MalMap): MalMap = MalMap(items.++(m.items))
  def get(key: MalType): MalType = items.get(key).getOrElse(MalNil)
}

case class MalSymbol(value: String) extends MalType
case object MalNil extends MalType