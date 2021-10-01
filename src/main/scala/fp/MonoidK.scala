package fp

trait MonoidK[F[_]] extends SemigroupK[F] {
  def empty[A]: F[A]
}
