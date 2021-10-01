package fp

import core.Parser.Parser

trait Applicative[F[_]] extends Functor[F] {
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = ap(map((a: A) => (b: B) => (a, b))(fa))(fb)

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  def pure[A](a: A): F[A]
}

object Applicative {
  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = for {
      f <- ff
      a <- fa
    } yield f(a)

    override def pure[A](a: A): Option[A] = Some(a)

    override def map[A, B](ff: A => B)(fa: Option[A]): Option[B] = fa.map(ff)
  }

  implicit val parserApplicative: Applicative[Parser] = new Applicative[Parser] {
    import Functor.parserFunctor

    override def ap[A, B](ff: Parser[A => B])(fa: Parser[A]): Parser[B] =
      input => ff(input).flatMap { case (i, f) => fa(i).map { case (c, a) => (c, f(a)) } }

    override def pure[A](a: A): Parser[A] = ignored => Right((ignored, a))

    override def map[A, B](ff: A => B)(fa: Parser[A]): Parser[B] = parserFunctor.map(ff)(fa)
  }
}
