package fp

object Syntax {
  implicit class TraversableApplicativeOps[F[_] : Traversable, G[_] : Applicative, A](private val in: F[G[A]]) {
    def sequenceA: G[F[A]] = in.traverse(identity)
  }

  implicit class TraversableOps[F[_] : Traversable, A](private val in: F[A]) {
    def traverse[G[_] : Applicative, B](f: A => G[B]): G[F[B]] =
      implicitly[Traversable[F]].traverse(in)(f)
  }

  implicit class FunctorOps[F[_] : Functor, A](private val fa: F[A]) {
    def map[B](f: A => B): F[B] = implicitly[Functor[F]].map(f)(fa)
  }

  implicit class ApplicativeOps[F[_] : Applicative, A](private val fa: F[A]) {
    private val applicative = implicitly[Applicative[F]]

    def product[B](fb: F[B]): F[(A, B)] = applicative.product(fa, fb)

    def ap[B](ff: F[A => B]): F[B] = applicative.ap(ff)(fa)

    def map[B](ff: A => B): F[B] = applicative.map(ff)(fa)
  }

  implicit class AlternatieOps[F[_] : Alternative, A](private val fa: F[A]) {
    private val alternative = implicitly[Alternative[F]]

    def product[B](fb: F[B]): F[(A, B)] = alternative.product(fa, fb)

    def ap[B](ff: F[A => B]): F[B] = alternative.ap(ff)(fa)

    def map[B](ff: A => B): F[B] = alternative.map(ff)(fa)

    def combineK(y: F[A]): F[A] = alternative.combineK(fa, y)

    def <+>(y: F[A]): F[A] = alternative.combineK(fa, y)
  }
}
