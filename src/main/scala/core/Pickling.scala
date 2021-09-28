package core

import fp.Syntax.TraversableApplicativeOps

object Pickling {
  type Parser[A] = List[Char] => Either[ParseError, (List[Char], A)]

  case class ParseError(cause: String)

  object Parser {
    val parseChar: Char => Parser[Char] =
      char => {
        case Nil => Left(ParseError("Unexpected end of input."))
        case x :: xs => if (x == char) Right(xs, x) else Left(ParseError(s"Expected $char but received $x"))
      }

    val parseCharList: List[Char] => Parser[List[Char]] =
      expected => expected.map(parseChar).sequenceA
  }
}
