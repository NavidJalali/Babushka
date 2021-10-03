package core

import fp.Alternative.parserAlternative.<+>
import fp.Syntax.{AlternativeOps, TraversableApplicativeOps}

object Parser {
  case class ParseError(cause: String)

  type Parsed[A] = Either[ParseError, (String, A)]

  object Parsed {
    def fromOption[A](opt: Option[(String, A)], message: String): Parsed[A] =
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
      message = s"$double is not a Double."
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

  // Todo: ADD remaining JSON value parsers
  val parseJson: Parser[JsonValue] =
    <+>[JsonValue](parseJsonNull,
      <+>[JsonValue](parseJsonBool, parseJsonNumber)
    )
}

object Test {
  def main(args: Array[String]): Unit = {
    import Parser._


    println(
      parseJson(""),
      parseJson("true"),
      parseJson("123.3125asdasd"),
      parseJson("nullq2e"),

    )
  }
}