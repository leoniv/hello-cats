package hello_cats
import cats.implicits._
import cats.Monad
import scala.annotation.tailrec

object EldisTest {
  final case class FooId(value: Int) extends AnyVal
  final case class Foo(
    id: FooId,
    references: List[FooId],
    data: String
  )

  trait FooRepository[F[_]] {
    def read(ids: List[FooId]): F[Set[Foo]]
  }

  def toIds(xs: Set[Foo]) = xs.toList.flatMap(_.references)

  def readClosure[F[_]: Monad](
    repo: FooRepository[F], ids: List[FooId]): F[Set[Foo]] =

    Monad[F].tailRecM(Set[Foo]() -> ids) {  case (acc, ids) =>
      if (ids.isEmpty) Monad[F].pure(Right(acc))
      else for {
        sel <- repo.read(ids)
      } yield Left(acc ++ sel, toIds(sel -- acc))
    }

  def readClosureReq[F[_]: Monad](
    repo: FooRepository[F], ids: List[FooId]): F[Set[Foo]] = {

    def readRepo(ss: Set[Foo]) = repo.read(
      ss.toList.flatMap(_.references)
    )

    def inner(acc: F[Set[Foo]], sel: F[Set[Foo]]): F[Set[Foo]] = for {
      ss <- sel
      as <- acc
      nacc <- (as ++ ss).pure[F]
      nsel <- (ss -- as).pure[F]
      result <- if (!nsel.isEmpty) inner(nacc.pure[F], readRepo(nsel))
        else nacc.pure[F]
    } yield result

    inner(Monad[F].pure(Set[Foo]()), repo.read(ids))
  }
}
