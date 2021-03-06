package fp

trait Traversable[F[_]] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]]
}

object Traversable {
  implicit val listTraversable: Traversable[List] = new Traversable[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit applicativeG: Applicative[G]): G[List[B]] =
      fa.foldRight(applicativeG.pure(List.empty[B])) {
        case (a, gbs) => applicativeG.map[(B, List[B]), List[B]] { case (b, bs) => b :: bs }(applicativeG.product(f(a), gbs))
      }
  }

  implicit val vectorTraversable: Traversable[IndexedSeq] = new Traversable[IndexedSeq] {
    override def traverse[G[_], A, B](fa: IndexedSeq[A])(f: A => G[B])(implicit applicativeG: Applicative[G]): G[IndexedSeq[B]] =
      fa.foldRight(applicativeG.pure(IndexedSeq.empty[B])) {
        case (a, gbs) => applicativeG.map[(B, IndexedSeq[B]), IndexedSeq[B]] { case (b, bs) => b +: bs }(applicativeG.product(f(a), gbs))
      }
  }
}
