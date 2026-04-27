package ex1

import ex1.*
import ex1.Parsers.charParser
import org.scalatest.*
import org.scalatest.matchers.should.Matchers.*

class ScalaParserTest extends org.scalatest.funsuite.AnyFunSuite:
  def parser = new BasicParser(Set('a', 'b', 'c'))
  def parserNE = new NonEmptyParser(Set('0', '1'))
  def parserNTC = new NotTwoConsecutiveParser(Set('X', 'Y', 'Z'))
  def parserNTCNE = new BasicParser(Set('X', 'Y', 'Z')) with NotTwoConsecutive[Char] with NonEmpty[Char]
  def sparser: Parser[Char] = "abc".charParser()
  def nparser = new BasicParser(Set('a', 'b', 'c')) with ShortenThenN[Char](2)
  test("BasicParser tests"):
    parser.parseAll("aabc".toList) should be (true)
    parser.parseAll("aabcdc".toList) should be (false)
    parser.parseAll("".toList) should be (true)
  
  test("NotEmptyParser tests"):
    parserNE.parseAll("0101".toList) should be (true)
    parserNE.parseAll("0123".toList) should be (false)
    parserNE.parseAll(List()) should be (false)
  
  test("NotTwoConsecutiveParser tests"):
    parserNTC.parseAll("XYZ".toList) should be (true)
    parserNTC.parseAll("XYYZ".toList) should be (false)
    parserNTC.parseAll("".toList) should be (true)
  
  test("NotEmptyAndNotTwoConsecutiveParser tests"):
    parserNTCNE.parseAll("XYZ".toList) should be (true)
    parserNTCNE.parseAll("XYYZ".toList) should be (false)
    parserNTCNE.parseAll("".toList) should be (false)
  
  test("StringParser tests"):
    sparser.parseAll("aabc".toList) should be (true)
    sparser.parseAll("aabcdc".toList) should be (false)
    sparser.parseAll("".toList) should be (true)

  test("ShortenThenNParser tests"):
    nparser.parseAll("".toList) should be(true)
    nparser.parseAll("a".toList) should be(true)
    nparser.parseAll("ab".toList) should be(true)
    nparser.parseAll("abc".toList) should be(false)
    nparser.parseAll("aba".toList) should be(false)
