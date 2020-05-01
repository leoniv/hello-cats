package fp.in.scala

import java.util.concurrent.{ExecutorService, Future, TimeUnit, Callable}

object Par {
  type Par[A] = Function1[ExecutorService, Future[A]]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def cancel(x: Boolean): Boolean = false
    def isCancelled(): Boolean = false
    def isDone(): Boolean = true
    def get(wait: Long, tu: TimeUnit): A = get
  }

  implicit def parMonad: Monad[Par] = new Monad[Par] {
    def ret[A](a: => A): Par[A] = unit(a)
    def flatMap[A, B](m: Par[A])(f: A => Par[B]): Par[B] =
      (es: ExecutorService) => f(m(es).get)(es)
    @annotation.tailrec
    def tailRecM[A, B](a: A)(f: A => Par[Either[A, B]]): Par[B] =
      as => f(a)(as).get match {
        case Left(a) => tailRecM(a)(f)(as)
        case Right(b) => UnitFuture(b)
      }
  }

  def unit[A](a: A): Par[A] = _ => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def map2[A, B](l: Par[A], r: Par[A])(f: (A, A) => B): Par[B] =
    (e: ExecutorService) => {
      val af = run(e)(l)
      val bf = run(e)(r)
      unit(f(af.get, bf.get))(e)
    }
  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) =>
      es.submit(
        new Callable[A] { def call = run(es)(a).get }
      )
  def run[A](e: ExecutorService)(p: Par[A]): Future[A] = p(e)

  object Example {
    def sum(ints: IndexedSeq[Int]): Par[Int] =
      if (ints.size <= 1) Par.unit(ints.headOption.getOrElse(0))
      else {
        val (l, r) = ints.splitAt(ints.size / 2)
        Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
      }

//    def program =
//      for {
//        sum <- sum((0 until 10000).toVector)
//      } yield sum
  }
}
