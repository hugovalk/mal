package mal

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import Reader.readString
import Printer.printString

class ReplSpec extends AnyFlatSpec with Matchers {

  "Print" must "be consistent with read for numbers" in {
    printString(readString("123")) mustEqual "123"
    printString(readString("123 ")) mustEqual "123"
  }

  it must "be consistent with read for symbols" in {
    printString(readString("abc")) mustEqual "abc"
    printString(readString("abc ")) mustEqual "abc"
  }

  it must "be consistent with read for lists" in {
    printString(readString("(123)")) mustEqual "(123)"
    printString(readString("(123 456)")) mustEqual "(123 456)"
    printString(readString("( 123 456 789 )")) mustEqual "(123 456 789)"
    printString(readString("( + 2 (* 3 4) )")) mustEqual "(+ 2 (* 3 4))"
  }

  it must "be consistent with read for vectors" in {
    printString(readString("[ +   1   [+   2 3   ]   ]")) mustEqual "[+ 1 [+ 2 3]]"
  }

  it must "be consistent with read for maps" in {
    printString(readString("{  }")) mustEqual "{}"
    printString(readString("{\"abc\" 1}")) mustEqual "{\"abc\" 1}"
    printString(readString("{:test (1 2 3)}")) mustEqual "{:test (1 2 3)}"
  }

  it must "correctly handle String quotes" in {
    printString(readString("\"\"")) mustEqual "\"\""
    printString(readString("\"abc\"")) mustEqual "\"abc\""
  }
}
