package mal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import mal.types._


class ReadSpec extends AnyFlatSpec with Matchers with Read {
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
    readString("\"\"") mustEqual MalString("\"\"")
    readString("\"abc\"") mustEqual MalString("\"abc\"")
  }

  "Numbers" must "be parsed correctly" in {
    readString("1") mustEqual MalNumber(BigDecimal(1))
    readString("1463724637816516378") mustEqual MalNumber(BigDecimal("1463724637816516378"))
    readString("1.43287932758329578293875475") mustEqual MalNumber(BigDecimal("1.43287932758329578293875475"))
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
