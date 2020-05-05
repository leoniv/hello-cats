import cats.implicits._
import cats.effect._
import java.io._

object Main extends IOApp {

  def srcFile[F[_]: Sync](file: File): Resource[F, FileInputStream] =
    Resource.fromAutoCloseable(Sync[F].delay(new FileInputStream(file)))

  def destFile[F[_]: Sync](file: File): Resource[F, FileOutputStream] =
    Resource.fromAutoCloseable(Sync[F].delay(new FileOutputStream(file)))

  def transfer[F[_]: Sync](src: InputStream, dest: OutputStream): F[Long] = {
    def go(
        src: InputStream,
        dest: OutputStream,
        buf: Array[Byte],
        total: Long
    ): F[Long] =
      for {
        count <- Sync[F].delay(src.read(buf))
        total <- if (count < 1) Sync[F].delay(total) else go(src, dest, buf, total + count)
      } yield total
    go(src, dest, new Array[Byte](1024), 0L)
  }

  def copy[F[_]: Sync](src: File, dest: File): F[Long] =
    (for {
      src <- srcFile(src)
      dest <- destFile(dest)
    } yield (src -> dest)).use((transfer[F] _).tupled)

  def copy0[F[_]: Sync](src: File, dest: File): Resource[F, Long] =  for {
    src <- srcFile[F](src)
    dest <- destFile[F](dest)
    total <- Resource.liftF(transfer(src, dest))
  } yield total

  override def run(args: List[String]): IO[ExitCode] = args match {
    case (src :: dest :: tail) =>
      copy0[IO](new File(src), new File(dest))
        .map(total => println(s"Copy $total")).use(_ => IO(ExitCode.Success))
    case _ => IO(println("Usage: copy src dest")) *> IO(ExitCode.Error)
  }

}
