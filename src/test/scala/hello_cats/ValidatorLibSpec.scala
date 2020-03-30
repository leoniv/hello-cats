package hello_cats

import org.scalatest.matchers.should._
import cats.data.Validated._
import hello_cats.ValidatorLib._
import org.scalatest.freespec.AnyFreeSpec
import cats.instances.list._
import cats.instances.either._

class ValidatorLibSpec extends AnyFreeSpec with Matchers {
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

    "#isInt" in {
      isInt("-123") should be(Valid("-123"))
      isInt("123") should be(Valid("123"))
      isInt("123") should be(Valid("123"))
      isInt("abc") should be(Invalid(error("Must be a number")))
    }

    "#moreThan" in {
      moreThan(10).run(11) should be(Valid(11))
      moreThan(10).run(10) should be(Invalid(error("Must be more than 10")))
    }

    "#lessThan" in {
      lessThan(10).run(9) should be(Valid(9))
      lessThan(10).run(10) should be(Invalid(error("Must be less than 10")))
    }
  }

  "Examples" - {
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

    type InputS = Input[String]

    def readAge: Check[InputS, Int] =
      for {
        age <- readKey[String]("age")
        _ <- checkWith[InputS, String](isInt)(age)
        _ <- checkWith[InputS, Int](
          moreThan(14) and lessThan(100)
        )(age.toInt)
      } yield age.toInt

    def readName: Check[InputS, String] =
      for {
        name <- readKey[String]("name")
        _ <- checkWith(alphanumeric and longerThan(3))(name)
      } yield name

    def readEmail: Check[InputS, String] =
      for {
        email <- readKey[String]("email")
      } yield email

    def readInputMonadic =
      for {
        name <- readName
        age <- readAge
        email <- readEmail
      } yield (name, age, email)

    def readInputApplicative =
      for {
        _ <- pred2check(validateInputApplicative)
        r <- readInputMonadic
      } yield r

    def validateInputApplicative: Predicate[Errors, InputS] = (
      check2pred[InputS](readName) and
        check2pred[InputS](readAge) and
        check2pred[InputS](readEmail)
    )

    "Fail-fast input read" - {
      "Read succsess" in {
        readInputMonadic.run(validInput) should
          be(Right(("Name", 42, "name@example.com")))
      }

      "Read failure" in {
        readInputMonadic.run(validInput - "age") should
          be(Left(error("Key `age' not found")))
      }
    }

    "Multy-error input read" - {
      "Read success" in {
        readInputApplicative(validInput) should
          be(Right(("Name", 42, "name@example.com")))
      }

      "Read error" - {
        "When key not found" in {
          readInputApplicative { validInput - "name" - "email" } should
            be(
              Left(
                "Key `name' not found" ::
                  error("Key `email' not found")
              )
            )
        }

        "When value invalid" in {
          readInputApplicative { invalidInput - "name" - "email" } should
            be(
              Left(
                "Key `name' not found" ::
                  "Must be a number" ::
                  error("Key `email' not found")
              )
            )
        }
      }
    }
  }
}
