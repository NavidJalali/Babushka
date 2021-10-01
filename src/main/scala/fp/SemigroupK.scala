package fp

trait SemigroupK[F[_]] {
  def combineK[A](x: F[A], y: F[A]): F[A]
  def <+>[A](x: F[A], y: F[A]): F[A] = combineK(x, y)
}
