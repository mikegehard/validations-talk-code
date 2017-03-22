package validations.validated

import java.time.LocalDateTime

import cats.data.Validated.{invalidNel, valid}
import cats.data.{NonEmptyList, Validated}
import cats.implicits._

class Car(name: String, modelYear: Int) {
  override def toString: String =
    s"Car: name=$name, model year=$modelYear"
}

object Car {
  // This type signature now tells you that a non-empty list of CarErrors or a car is returned.
  // This is similar to Either[List[CarError], Car] but has easy ways accumulate errors that you would have
  // to write yourself if you used Either[List[CarError], Car]
  def apply(name: String, modelYear: Int): Validated[NonEmptyList[CarError], Car] = {
    // Combine the validated fields into the car
    // Instead of forming a monad to chain results,
    // Validated is an applicative functor that allows us to
    // consolidate all of the errors or the successful results
    // The success case is a tuple with the valid values
    // or the failure case is a Non-Empty list of CarErrors.
    (
      validateName(name)
        |@| validateModelYear(modelYear)
      ).map(
      (name, year) => new Car(name, year)
    )
  }

  private def validateName(value: String): Validated[NonEmptyList[CarError], String] =
    if (invalidName(value)) {
      invalidNel(new NameEmptyError)
    } else {
      valid(value)
    }

  private def validateModelYear(value: Int): Validated[NonEmptyList[CarError], Int] =
    if (invalidModelYear(value)) {
      invalidNel(new ModelYearOutsideValidRangeError)
    } else {
      valid(value)
    }

  private def invalidName(value: String): Boolean =
    value.isEmpty

  private def invalidModelYear(value: Int): Boolean =
    value < 1900 || value > LocalDateTime.now().getYear
}

/** ******** Errors *****************/
sealed trait CarError {
  val message: String
}

class NameEmptyError extends CarError {
  override val message: String =
    "Invalid car name"
}

class ModelYearOutsideValidRangeError extends CarError {
  override val message: String =
    "Invalid car model name"
}

/** ******** Main program *****************/
object Main {
  def main(args: Array[String]): Unit = {
    // Data you get from your user which we can't assume is correct
    val validName = "Ford"
    val invalidName = ""

    val validModelYear = 2015
    val invalidModelYear = 1200

    val validCar: Validated[NonEmptyList[CarError], Car] = Car(validName, validModelYear)

    validCar.fold(handleInvalidCar, handleValidCar)

    val partiallyValidCar: Validated[NonEmptyList[CarError], Car] = Car(validName, invalidModelYear)

    partiallyValidCar.fold(handleInvalidCar, handleValidCar)

    val invalidCar: Validated[NonEmptyList[CarError], Car] = Car(invalidName, invalidModelYear)

    invalidCar.fold(handleInvalidCar, handleValidCar)
  }

  // Now I have a list of all of the things that went wrong.
  private def handleInvalidCar(es: NonEmptyList[CarError]) =
    println(s"******* Invalid car! Reasons: ${es.map(_.message).toList.mkString(", ")} *******")

  private def handleValidCar(c: Car): Unit =
    println(s"******* $c *******")
}
