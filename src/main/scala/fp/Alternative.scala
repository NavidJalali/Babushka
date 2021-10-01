package fp

import core.Parser.{ParseError, Parser}

trait Alternative[F[_]] extends Applicative[F] with MonoidK[F]

object Alternative {
  import Applicative.parserApplicative

  implicit val parserAlternative: Alternative[Parser] = new Alternative[Parser] {
    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] = parserApplicative.ap(ff)(fa)

    override def pure[A](a: A): Parser[A] = parserApplicative.pure(a)

    override def empty[A]: Parser[A] = input => Left(ParseError(s"Empty parser called for $input"))

    override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] =
      input => x(input).orElse(y(input)).orElse(empty(input))

    override def map[A, B](ff: A => B)(fa: Parser[A]): Parser[B] = parserApplicative.map(ff)(fa)
  }
}