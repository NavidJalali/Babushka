package core

import fp.Alternative.parserAlternative
import fp.Syntax.{AlternativeOps, TraversableApplicativeOps}

object Parser {
  case class ParseError(cause: String)

  type Parsed[A] = Either[ParseError, (String, A)]

  type Parser[A] = String => Parsed[A]

  implicit class JsonParserOps(private val self: Parser[JsonValue]) {
    def <&>(other: Parser[JsonValue]): Parser[JsonValue] = parserAlternative.combineK(self, other)
  }

  implicit class OptionParserOps[A](private val self: Parser[Option[A]]) {
    def flattenOption: Parser[A] =
      input => self(input).flatMap {
        case (str, Some(a)) => Right((str, a))
        case (_, None) => Left(ParseError("Failed to lift optional to value."))
      }
  }

  def spanParser(predicate: Char => Boolean): Parser[String] = input => Right(input.span(predicate).swap)

  def nonEmpty(p: Parser[String]): Parser[String] = input => p(input).flatMap {
    case tupled@(_, value) =>
      if (value.isEmpty) Left(ParseError("Expected nonempty string but received empty."))
      else Right(tupled)
  }

  val parseInt: Parser[Int] = nonEmpty(spanParser(_.isDigit)).map(_.toIntOption).flattenOption

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

  val parseJsonNumber: Parser[JsonValue.JsonNumber] = {
    parseInt
      .zipLeft(parseChar('.'))
      .product(parseInt)
      .map { case (big, small) => s"$big.$small".toDoubleOption.map(JsonValue.JsonNumber) }
      .flattenOption
      .combineK(parseInt.map(int => JsonValue.JsonNumber(int.toDouble)))
  }

  // Todo: ADD remaining JSON value parsers
  val parseJson: Parser[JsonValue] =
    parseJsonNull <&> parseJsonBool <&> parseJsonNumber
}
