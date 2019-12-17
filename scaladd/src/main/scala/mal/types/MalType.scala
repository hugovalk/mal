package mal.types

sealed trait MalType

sealed trait MalSeq extends MalType {
  def append(item: MalType): MalSeq
}
object MalList {
  def apply(items: MalType*): MalList = {
    MalList(items.toVector)
  }
}
case class MalList(items: Vector[MalType] = Vector()) extends MalSeq {
  def append(item: MalType): MalSeq = MalList(items.:+(item))
}
object MalVec {
  def apply(items: MalType*): MalVec = {
    MalVec(items.toVector)
  }
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

