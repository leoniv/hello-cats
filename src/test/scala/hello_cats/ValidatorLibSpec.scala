package hello_cats

import org.scalatest.matchers.should._
import cats.data.Validated._
import hello_cats.ValidatorLib._
import org.scalatest.freespec.AnyFreeSpec
import cats.instances.list._
import cats.instances.either._

class ValidatorLibSpec extends AnyFreeSpec with Matchers {
  case class User(name: String, age: Int, email: String)

  "Predicate" - {
    val error = (m: String) => List(m)
    val invalid = (m: String) => Predicate.lift(error(m), (_: Any) => false)
    val valid = (_: String) => Predicate.lift(error(""), (_: Any) => true)

    "Or" in {
      assert((invalid("1") or invalid("2") apply "") == Invalid(List("1", "2")))
      assert((valid("") or valid("") apply "value") == Valid("value"))
      assert((valid("") or invalid("2") apply "value") == Valid("value"))
      assert((invalid("1") or valid("") apply "value") == Valid("value"))
    }

    "And" in {
      assert(
        (invalid("1") and invalid("2") apply "") == Invalid(List("1", "2"))
      )
      assert((invalid("1") and valid("") apply "") == Invalid(List("1")))
      assert((valid("") and invalid("2") apply "") == Invalid(List("2")))
      assert((valid("") and valid("2") apply "value") == Valid("value"))
    }
  }

  "Helpers" - {
    "#readKey" in {
      readKey("k").run(Map()) should be(Left(error("Key `k' not found")))
      readKey("k").run(Map("k" -> 1)) should be(Right(1))
    }

    "#mustBeKey" in {
      mustBeKey("k").run(Map()) should be(Invalid(error("Key `k' not found")))
      mustBeKey("k").run(Map("k" -> 1)) should be(Valid(Map("k" -> 1)))
    }

    "#isInt" in {
      isInt("-123") should be(Valid("-123"))
      isInt("123") should be(Valid("123"))
      isInt("123") should be(Valid("123"))
      isInt("abc") should be(Invalid(error("Must be a number")))
    }
  }

  def const[A, B](a: A)(b: B): A = a

  "Example #makeUser" - {
    val validInput = Map(
      "name" -> "Name",
      "age" -> "42",
      "email" -> "name@example.com"
    )

    val invalidInput = Map(
      "name" -> "",
      "age" -> "NaN",
      "email" -> "example.com"
    )

    def readAge: Check[Input[String], Int] = for {
      age <- readKey[String]("age")
      _ <- pred2check(isInt.contramap(const[String, Input[String]](age)))
    } yield age.toInt

    def validateAge: Predicate[Errors, Input[_]] = ??? /*
    check2pred(
      readKey[String]("age") flatMap { age =>
        check(in => isInt.contramap(in => age))
      }

    // flatMap ((age: String) => pred2check(isInt(age)))
    )
    */

    def readInputMonadic =
      for {
        name <- readKey[String]("name")
        age <- readKey[String]("age")
        email <- readKey[String]("email")
      } yield (name, age, email)

    def readInputApplicative = (
      mustBeKey("name") and
      validateAge and
      mustBeKey("email")
    )

    "Fail-fast input read" - {
      "Read succsess" in {
        readInputMonadic.run(validInput) should
          be(Right(("Name", "42", "name@example.com")))
      }

      "Read failure" in {
        readInputMonadic.run(validInput - "age") should
          be(Left(error("Key `age' not found")))
      }
    }

    "Multy error input read" - {
      "Read success" in {
        readInputApplicative(validInput) should
          be(Valid(validInput))
      }

      "Read error" - {
        "When key not found" in {
          readInputApplicative { validInput - "name" - "email" } should
            be(
              Invalid(
                "Key `name' not found" ::
                  error("Key `email' not found")
              )
            )
        }

        "When value invalid" in {
          readInputApplicative { invalidInput - "name" - "email" } should
            be(
              Invalid(
                "Key `name' not found" ::
                  "Must be a number" ::
                  error("Key `email' not found")
              )
            )
        }
      }
    }

//    def makeUser(in: Input[String]) = {
//      (checkName, checkAge, checkEmail)
//    }
//
//
//    "When input contains valid data should make instance of User" in {
//      makeUser(validInput) should
//        be (Valid(User("Name", 42, "name@example.com")))
//    }
//
//    "When input contains invalid data sould returns errors" in {
//      makeUser(invalidInput) should
//        be (Invalid(List("Must be none empty string",
//                      "Must be a number",
//                      "Must be one word",
//                      "Must have dot")
//                    )
//                  )
//    }
  }
}
