import cats.implicits._
import cats.effect._
import java.io._

object Main extends IOApp {

  def srcFile(file: File): Resource[IO, FileInputStream] =
    Resource.fromAutoCloseable(IO.delay(new FileInputStream(file)))

  def destFile(file: File): Resource[IO, FileOutputStream] =
    Resource.fromAutoCloseable(IO.delay(new FileOutputStream(file)))

  def transfer(src: InputStream, dest: OutputStream): IO[Long] = {
    def go(
        src: InputStream,
        dest: OutputStream,
        buf: Array[Byte],
        total: Long
    ): IO[Long] =
      for {
        count <- IO(src.read(buf))
        total <- if (count < 1) IO(total) else go(src, dest, buf, total + count)
      } yield total
    go(src, dest, new Array[Byte](1024), 0L)
  }

  def copy(src: File, dest: File): IO[Long] =
    (for {
      src <- srcFile(src)
      dest <- destFile(dest)
    } yield (src -> dest)).use((transfer _).tupled)

  def copy0(src: File, dest: File): Resource[IO, Long] =  for {
    src <- srcFile(src)
    dest <- destFile(dest)
    total <- Resource.liftF(transfer(src, dest))
  } yield total

  override def run(args: List[String]): IO[ExitCode] = args match {
    case (src :: dest :: tail) =>
      copy0(new File(src), new File(dest))
        .map(total => println(s"Copy $total")).use(_ => IO(ExitCode.Success))
    case _ => IO(println("Usage: copy src dest")) *> IO(ExitCode.Error)
  }

}
