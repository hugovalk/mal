package step1

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import Reader.readString
import Printer.printString

class ReplSpec extends AnyFlatSpec with Matchers {

  "Print" must "be consistent with read" in {
    printString(readString("123")) mustEqual "123"
    printString(readString("(123)")) mustEqual "(123)"
    printString(readString("123 ")) mustEqual "123"
    printString(readString("abc")) mustEqual "abc"
    printString(readString("abc ")) mustEqual "abc"
    printString(readString("(123 456)")) mustEqual "(123 456)"
    printString(readString("(123 456")) mustEqual "(123 456)"
    printString(readString("( 123 456 789 )")) mustEqual "(123 456 789)"
    printString(readString("( + 2 (* 3 4) )")) mustEqual "(+ 2 (* 3 4))"
  }

}
