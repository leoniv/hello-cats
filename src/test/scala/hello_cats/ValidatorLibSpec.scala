package hello_cats

import org.scalatest.matchers.should._
import cats.data.Validated._
import hello_cats.ValidatorLib._
import org.scalatest.freespec.AnyFreeSpec
import cats.instances.list._

class ValidatorLibSpec extends AnyFreeSpec with Matchers {
  case class Input(name: String, age: String, email: String)
  case class User(name: String, age: Int, email: String)

  "Predicate" - {
    val error = (m: String) => List(m)
    val invalid = (m: String) => Predicate.lift(error(m), (_: Any) => false)
    val valid = (_: String) => Predicate.lift(error(""), (_: Any) => true)
    "Or" in {
       assert((invalid("1") or invalid("2") apply "") == Invalid(List("1","2")))
       assert((valid("") or valid("") apply "value") == Valid("value"))
       assert((valid("") or invalid("2") apply "value") == Valid("value"))
       assert((invalid("1") or valid("") apply "value") == Valid("value"))
    }

    "And" in {
      assert((invalid("1") and invalid("2") apply "") == Invalid(List("1","2")))
      assert((invalid("1") and valid("") apply "") == Invalid(List("1")))
      assert((valid("") and invalid("2") apply "") == Invalid(List("2")))
      assert((valid("") and valid("2") apply "value") == Valid("value"))
    }
  }


  "#makeUser" - {
    def makeUser(in: Input): ValidatedL[User] = ???

    "When input contains valid data should make instance of User" in {
      makeUser(Input("Name", "42", "name@example.com")) should
        be (Valid(User("Name", 42, "name@example.com")))
    }

    "When input contains invalid data sould returns errors" in {
      makeUser(Input("", "-1", "bad name@badhost")) should
        be (Invalid(List("Must be none empty string",
                      "Must be number more than 16",
                      "Must be one word",
                      "Must have dot")
                    )
                  )
    }
  }
}
