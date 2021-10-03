package core

import fp.Alternative.parserAlternative
import fp.Syntax.{AlternativeOps, TraversableApplicativeOps}

object Parser {
  case class ParseError(cause: String)

  type Parsed[A] = Either[ParseError, (String, A)]

  object Parsed {
    def fromOption[A](opt: Option[(String, A)], message: => String): Parsed[A] =
      opt.fold[Parsed[A]](Left(ParseError(message)))(Right.apply)
  }

  type Parser[A] = String => Parsed[A]


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

  def doubleFromString(double: String, rest: String): Parsed[JsonValue.JsonNumber] =
    Parsed.fromOption(
      double.toDoubleOption.map(JsonValue.JsonNumber).map((rest, _)),
      message = if (double.isEmpty) s"Parsed empty string as Double." else s"$double is not a Double."
    )

  val parseJsonNumber: Parser[JsonValue.JsonNumber] =
    input => input.span(_.isDigit) match {
      case (digits, "") => doubleFromString(digits, "")
      case (digits, xs) =>
        val head = xs.head
        val tail = xs.tail
        if (head == '.')
          tail.span(_.isDigit) match {
            case (small, rest) => doubleFromString(s"$digits.$small", rest)
          }
        else
          doubleFromString(digits, xs)
    }

  implicit class JsonParserOps(private val self: Parser[JsonValue]) {
    def <&>(other: Parser[JsonValue]): Parser[JsonValue] = parserAlternative.combineK(self, other)
  }

  // Todo: ADD remaining JSON value parsers
  val parseJson: Parser[JsonValue] =
    parseJsonNull <&> parseJsonBool <&> parseJsonNumber
}
