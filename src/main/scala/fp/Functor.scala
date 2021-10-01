package fp

import core.Parser.Parser

trait Functor[T[_]] {
  def map[A, B](ff: A => B)(fa: T[A]): T[B]
}

object Functor {
  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](ff: A => B)(fa: List[A]): List[B] = fa.map(ff)
  }

  implicit val parserFunctor: Functor[Parser] = new Functor[Parser] {
    override def map[A, B](f: A => B)(a: Parser[A]): Parser[B] =
      str => a(str).map { case (rest, a) => (rest, f(a)) }
  }
}
