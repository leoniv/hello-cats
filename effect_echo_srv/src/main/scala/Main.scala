import cats.effect._
import cats.effect.syntax.all._
import java.net.{ServerSocket, Socket}
import java.io._
import cats.implicits._
import cats.effect.ExitCase.Completed
import cats.effect.concurrent.MVar

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    def close(s: ServerSocket): IO[Unit] =
      IO(s.close()).handleErrorWith(_ => IO.unit)
    IO { new ServerSocket(args.headOption.map(_.toInt).getOrElse(4242)) }
      .bracket { serverSocket =>
        IO { println("Server start") } >> server[IO](serverSocket) *> IO {
          ExitCode.Success
        }
      } { server => close(server) >> IO { println("Server finished") } }
  }

  def server[F[_]: Concurrent](serverSocket: ServerSocket) =
    for {
      cancel <- MVar[F].empty[Unit]
      server <- serve(serverSocket, cancel).start
      _ <- cancel.read
      _ <- server.cancel.start
    } yield ()

  def echoProtocol[F[_]: Sync](
      socket: Socket,
      cancel: MVar[F, Unit]
  ): F[Unit] = {
    val in = Resource.fromAutoCloseable(
      Sync[F].delay(
        new BufferedReader(new InputStreamReader(socket.getInputStream()))
      )
    )
    val out =
      Resource.fromAutoCloseable(
        Sync[F].delay(
          new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()))
        )
      )

    def loop(in: BufferedReader, out: BufferedWriter): F[Unit] =
      for {
        mess <- Sync[F].delay(in.readLine())
        _ <- mess match {
          case "RISE" => throw new Error("RISE")
          case "STOP" => cancel.put(())
          case ""     => Sync[F].unit
          case _ =>
            Sync[F].delay { out.write(mess); out.newLine(); out.flush() } *> loop(
              in,
              out
            )
        }

      } yield ()

    (for {
      in <- in
      out <- out
    } yield (in, out)).use((loop _).tupled)
  }

  def serve[F[_]: Concurrent](
      server: ServerSocket,
      cancel: MVar[F, Unit]
  ): F[Unit] = {
    def close(socket: Socket): F[Unit] =
      Sync[F].delay(socket.close()).handleError(_ => Sync[F].unit)

    for {
      session <- Sync[F]
        .delay(server.accept())
        .bracketCase(socket =>
          echoProtocol(socket, cancel).guarantee(close(socket)).start
        ) { (socket, exit) =>
          exit match {
            case Completed => Sync[F].unit
            case _         => close(socket)
          }
        }
        _ <-  Concurrent[F].start(cancel.read.flatMap(_ => session.cancel))
      _ <- serve(server, cancel)
    } yield ()
  }
}
