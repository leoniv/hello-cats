package fp.in.scala

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)

  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S) = self.run(s) match {
      case (a, s1) => (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]) = new ST[S, B] {
    def run(s: S) = self.run(s) match {
      case (a, s1) => f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: A) = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S) = (memo, s)
    }
  }

  def runST[A](st: RunST[A]): A =
    st.apply[Unit].run(())._1
}

sealed trait STRef[S, A] {
  protected var value: A

  def read: ST[S, A] = ST(value)

  def write(a: A): ST[S, A] = new ST[S, A] {
    def run(s: S) = {
      value = a
      (value, s)
    }
  }
}

object STRef {
  def apply[S, A](a: A): ST[S, STRef[S, A]] = ST(
    new STRef[S, A] {
      var value = a
    }
  )
}

trait RunST[A] {
  def apply[S]: ST[S, A]
}

object test {

  val program = new RunST[(Int, Int)] {
    def apply[S] = {
      for {
        a <- STRef[S, Int](1)
        b <- STRef(3)
        av <- a.read
        bv <- b.read
        _ <- a.write(bv + 1)
        _ <- b.write(av + 1)
        a <- a.read
        b <- b.read
      } yield (a, b)
    }
  }

  ST.runST(program)
}
