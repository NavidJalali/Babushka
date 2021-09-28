package fp

object Syntax {
  implicit class TraversableApplicativeOps[F[_]: Traversable, G[_]: Applicative, A](private val in: F[G[A]]) {
    def sequenceA: G[F[A]] = implicitly[Traversable[F]].traverse(in)(identity)
  }
}
