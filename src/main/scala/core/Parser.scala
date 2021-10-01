package core

import fp.Alternative
import fp.Alternative.parserAlternative.combineK
import fp.Syntax.{AlternatieOps, TraversableApplicativeOps}

object Parser {
  type Parser[A] = String => Either[ParseError, (String, A)]

  case class ParseError(cause: String)

  def parseChar(char: Char): Parser[Char] = {
    case "" => Left(ParseError("Unexpected end of input."))
    case str =>
      val head = str.head
      val tail = str.tail
      if (head == char) Right(tail, head)
      else Left(ParseError(s"Expected $char but received $head"))
  }

  def parseString(string: String): Parser[String] = string.map(parseChar).sequenceA.map(_.mkString)

  val parseJsonNull: Parser[JsonValue.JsonNull.type] = parseString("null").map(_ => JsonValue.JsonNull)

  val parseJsonBool: Parser[JsonValue.JsonBool] =
    parseString("true").map(_ => JsonValue.JsonBool(true)) combineK
      parseString("false").map(_ => JsonValue.JsonBool(false))

  // Todo: ADD remaining JSON value parsers
  val parseJson: Parser[JsonValue] =
    combineK[JsonValue](
      parseJsonBool,
      parseJsonNull
    )

}
