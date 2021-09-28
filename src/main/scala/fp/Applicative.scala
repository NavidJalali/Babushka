package fp

import core.Pickling.Parser

trait Applicative[F[_]] extends Functor[F] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ap(map((a: A) => (b: B) => (a, b))(fa))(fb)

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def pure[A](a: A): F[A]

  override def map[A, B](ff: A => B)(fa: F[A]): F[B] = ap(pure(ff))(fa)
}

object Applicative {
  implicit val parserApplicative: Applicative[Parser] = new Applicative[Parser] {
    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
      input => ff(input).flatMap {
        case (rest, transform) => fa(rest).map { case (c, a) => (c, transform(a)) }
      }

    override def pure[A](a: A): Parser[A] = ignored => Right((ignored, a))
  }
}
