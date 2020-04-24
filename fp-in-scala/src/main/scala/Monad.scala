package fp.in.scala

trait Monad[A] {
  def pure[A]: Monad[A]
  def flatMap[A](m: Monad[A], f: A => Monad[A]): Monad[A]

  def bind = flatMap _
  def >> = flatMap _
}
