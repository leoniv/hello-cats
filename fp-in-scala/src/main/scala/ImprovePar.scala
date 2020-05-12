import java.util.concurrent.ExecutorService
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Future
import java.util.concurrent.Callable

object ImprovePar {
  type Par[A] = ExecutorService => Future[A]

  trait Future[A] {
    def apply(k: A => Unit): Unit
  }

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]()
    val cdl = new CountDownLatch(1)
    p(es) { a => ref.set(a); cdl.countDown }
    cdl.await()
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es =>
      new Future[A] {
        def apply(k: A => Unit): Unit = k(a)
      }

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      new Future[A] {
        def apply(k: A => Unit): Unit =
          eval(es)(a(es)(k))
      }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit[Unit](new Callable[Unit] { def call = r })

  def map2[A, B, C](l: => Par[A])(r: => Par[B])(f: (A, B) => C): Par[C] =
    es =>
      new Future[C] {
        def apply(k: C => Unit): Unit = {
          val cdl = new CountDownLatch(1);
          var a: Option[A] = None
          var b: Option[B] = None

          l(es)(a1 => { a = Some(a1); cdl.countDown() })
          r(es)(b1 => { b = Some(b1); cdl.countDown() })

          cdl.await()
          k(f(a.get, b.get))
        }
      }
}
