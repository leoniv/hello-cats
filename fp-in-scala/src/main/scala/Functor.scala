package fp.in.scala

trait Functor[A] {
  def fmap[A, B](f: Functor[A])(g: A => B): Functor[B]
  def liftF[A, B](g: A => B): Functor[A] => Functor[B] = fmap(_)(g)
}

