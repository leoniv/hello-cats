package hello_cats.async_echo_srv

import cats.effect._
import cats.effect.syntax.all._
import cats.implicits._
import java.nio.channels.AsynchronousSocketChannel
import java.nio.channels.AsynchronousServerSocketChannel
import java.nio.channels.CompletionHandler
import cats.effect.ExitCase.Completed
import java.nio.ByteBuffer
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import cats.effect.concurrent.MVar

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    def close(s: AsynchronousServerSocketChannel) =
      IO.delay(s.close()).handleErrorWith(_ => IO.unit)
    IO { AsynchronousServerSocketChannel.open() }.bracket { serverSocket =>
      IO { println("Server start") } *> IO {
        serverSocket.bind(new InetSocketAddress(4242))
      } >> server[IO](serverSocket) *> IO { ExitCode.Success }
    } { serverSocket => close(serverSocket) >> IO { println("Server stop") } }
  }

  def server[F[_]: Concurrent](
      serverSocket: AsynchronousServerSocketChannel
  ): F[Unit] =
    for {
      cancel <- MVar[F].empty[Unit]
      socket <- serve[F](serverSocket, cancel).start
      _ <- cancel.read
      _ <- socket.cancel.start
    } yield ()

  def echoProtocol[F[_]: Async](
      socket: AsynchronousSocketChannel,
      cancel: MVar[F, Unit]
  ): F[Unit] = {
    def read(
        socket: AsynchronousSocketChannel,
        buff: ByteBuffer
    ): F[(Int, ByteBuffer)] = {
      Async[F].async { cb =>
        val handler = new CompletionHandler[Integer, ByteBuffer] {
          def completed(x: Integer, b: ByteBuffer): Unit = cb((x.toInt, b).asRight)
          def failed(e: Throwable, b: ByteBuffer): Unit = cb(e.asLeft)
        }
        socket.read(buff, buff, handler)
      }
    }
    def write(socket: AsynchronousSocketChannel, buff: ByteBuffer): F[Unit] = {
      Async[F].async { cb =>
        val handler = new CompletionHandler[Integer, Unit] {
          def completed(x: Integer, a: Unit): Unit = cb(().asRight)
          def failed(x: Throwable, a: Unit): Unit = cb(x.asLeft)
        }
        socket.write(buff, (), handler)
      }
    }

    def loop(
        socket: AsynchronousSocketChannel,
        cancel: MVar[F, Unit],
        fbuff: F[ByteBuffer]
    ): F[Unit] =
      for {
        bf <- fbuff
        cb <- read(socket, bf)
        mess <- Async[F].delay(new String(cb._2.array(), StandardCharsets.UTF_8))
        _ <- mess.take(cb._1).trim match {
          case ""     => Async[F].unit
          case "STOP" => cancel.put(())
          case _ =>
            write(socket, bf.flip()) *> loop(socket, cancel, Sync[F].delay {
              bf.clear()
            })
        }
      } yield ()

    loop(socket, cancel, Sync[F].delay { ByteBuffer.allocate(32) })
  }

  def accept[F[_]: Async](
      serverSocket: AsynchronousServerSocketChannel
  ): F[AsynchronousSocketChannel] = {
    Async[F].async { cb =>
      val handler = new CompletionHandler[AsynchronousSocketChannel, Unit] {
        def completed(result: AsynchronousSocketChannel, a: Unit) =
          cb(result.asRight)
        def failed(exc: Throwable, a: Unit) = cb(exc.asLeft)
      }
      serverSocket.accept((), handler)
    }
  }

  def serve[F[_]: Concurrent](
      serverSocket: AsynchronousServerSocketChannel,
      cancel: MVar[F, Unit]
  ): F[Unit] = {
    def close(socket: AsynchronousSocketChannel): F[Unit] = {
      Async[F].delay(socket.close()).handleErrorWith(_ => Async[F].unit)
    }
    for {
      session <- accept[F](serverSocket).bracketCase { socket =>
        echoProtocol(socket, cancel).guarantee(close(socket)).start
      } { (socket, exit) =>
        exit match {
          case Completed => Async[F].unit
          case _         => close(socket)
        }
      }
      _ <- Concurrent[F].start(cancel.read.flatMap(_ => session.cancel))
      _ <- serve(serverSocket, cancel)
    } yield ()
  }
}
