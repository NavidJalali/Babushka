package fp

trait Semigroup[A] {
  def combine(x: A, y: A): A
  def |+|(x: A, y: A): A = combine(x, y)
}
