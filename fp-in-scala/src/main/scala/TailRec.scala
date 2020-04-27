package fp.in.scala

object TailRecExample {

  sealed trait TailRec[A] { self =>
    def flatMap[B](f: A => TailRec[B]): TailRec[B] = FlatMap(self, f)
    def map[B](f: A => B) = flatMap(f andThen { Return(_) })
    def run = TailRec.run(self)
    override def toString(): String = "TailRec"
  }

  object TailRec extends Monad[TailRec] {
    def flatMap[A, B](m: TailRec[A])(f: A => TailRec[B]): TailRec[B] =
      m flatMap f
    def ret[A](a: => A): TailRec[A] = Return(a)

    @annotation.tailrec
    def tailRecM[A, B](a: A)(f: A => TailRec[Either[A, B]]): TailRec[B] = f(a).run match {
      case Left(value) => tailRecM(a)(f)
      case Right(value) => Return(value)
    }
    def apply[A](a: A): TailRec[A] = ret(a)

    @annotation.tailrec
    def run[A](io: TailRec[A]): A = io match {
      case Return(a)       => a
      case Suspend(resume) => resume()
      case FlatMap(x, f) =>
        x match {
          case Return(a)       => run(f(a))
          case Suspend(resume) => run(f(resume()))
          case FlatMap(y, g)   => run(y flatMap (ya => g(ya) flatMap f))
        }
    }
  }

  final case class Return[A](a: A) extends TailRec[A]
  final case class Suspend[A](resume: () => A) extends TailRec[A]
  final case class FlatMap[A, B](sub: TailRec[A], k: A => TailRec[B])
      extends TailRec[B]

  def printLine(s: String): TailRec[Unit] = Suspend(() => Return(println(s)))

  def program: TailRec[Unit] =
    for {
      s <- printLine(s"Echo")
    } yield ()

  def forever = TailRec.forever(printLine("It's still doing ..."))

  val f: Int => TailRec[Int] = x => for {
    x <- Return(x)
  } yield x

  def longLoop: Int => TailRec[Int] =
    List.fill(10000)(f).foldLeft(f) { (a, b) => (x: Int) => FlatMap(a(x), b) }

  def processLongLoop = longLoop(42).run
}
