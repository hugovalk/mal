package mal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import Reader.readString
import mal.types._

class ReaderSpec extends AnyFlatSpec with Matchers {
  "Symbols" must "be parsed correctly" in {
    readString("test") mustEqual MalSymbol("test")
  }

  "List" must "be parsed correctly when empty" in {
    readString("()") mustEqual MalList()
    readString("( )") mustEqual MalList()
  }

  it must "be parsed correctly containing symbols" in {
    readString("(test)") mustEqual MalList(MalSymbol("test"))
    readString("(test test2)") mustEqual MalList(MalSymbol("test"), MalSymbol("test2"))
  }

  it must "throw an error when not closed" in {
    assertThrows[IllegalStateException](readString("(test test"))
  }

  "Strings" must "be parsed correctly" in {
    readString("\"\"") mustEqual MalSymbol("\"\"")
    readString("\"abc\"") mustEqual MalSymbol("\"abc\"")
  }

  it must "throw an error when Strings are invalid" in {
    assertThrows[IllegalStateException](readString("\""))
    assertThrows[IllegalStateException](readString("\"test"))
    assertThrows[IllegalStateException](readString("\"\"\""))
  }

  "Map" must "be parsed correctly when empty" in {
    readString("{}") mustEqual MalMap(Map())
    readString("{ }") mustEqual MalMap(Map())
  }
}
