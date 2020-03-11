package hello_cats

import org.scalatest.matchers.should._
import org.scalatest.flatspec._
import hello_cats.EldisTest._
import cats.implicits._
import org.scalatest.prop.TableDrivenPropertyChecks._

class EldisTestSpec extends AnyFlatSpec
    with Matchers {

  def toFoo(x: (Int, List[Int])) = x match {
    case (id, refs) =>
      FooId(id) -> Foo(FooId(id), refs.map(FooId(_)), "Data")
  }

  val data: Map[FooId, Foo] =  Map(
    1 -> List(2, 3),
    2 -> List(4, 5),
    3 -> List(6, 7),
    4 -> List(8, 9),
    5 -> List(10),
    6 -> List(), 7 -> List(), 8 -> List(), 10 -> List(),
    9 -> List(2, 3)
  ).map(toFoo _)

  def repo = new FooRepository[Option] {
    override def read(ids: List[FooId]) = Some(
      ids.map(data.get _).collect{ case Some(x) => x }.toSet[Foo]
    )
  }

  val tests = Table(
    ("Input Id", "Expected Foo's"),
    List(FooId(0)) -> Set[Int](),
    List(FooId(3)) -> Set(3, 6, 7),
    List(FooId(3), FooId(6)) -> Set(3, 6, 7),
    List(FooId(2)) -> Set(2, 4, 5, 8, 9, 10, 3, 6, 7)
  )

  forAll (tests) { (ids: List[FooId], expect: Set[Int]) =>
    s"#readClosure($ids)" should "returns set of Foo" in {
        for {
          closure <- readClosure(repo, ids)
        } yield (closure.map(_.id.value)
            should be(expect)
        )
    }
  }

}
