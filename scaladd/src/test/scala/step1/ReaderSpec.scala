package step1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import Reader.readString

class ReaderSpec extends AnyFlatSpec with Matchers {
  "Symbols" must "be parsed correctly" in {
    readString("test") mustEqual MalSymbol("test")
  }
  "List" must "be parsed correctly when empty" in {
    readString("()") mustEqual MalList(Vector())
  }
  it must "be parsed correctly containing symbols" in {
    readString("(test)") mustEqual MalList(Vector(MalSymbol("test")))
    readString("(test test2)") mustEqual MalList(Vector(MalSymbol("test"), MalSymbol("test2")))
  }
  it must "throw an error when not closed" in {
    assertThrows[IllegalStateException](readString("(test test"))
  }
}
