package validations.validated

import java.time.LocalDateTime

import cats.data.Validated.{invalidNel, valid}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._

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

class Car(name: String, modelYear: Int) {
  override def toString: String =
    s"Car: name=$name, model year=$modelYear"
}

/** ******** Domain objects *****************/
object Car {
  // This type signature now tells you that a non-empty list of CarErrors or a car is returned.
  // This is similar to Either[List[CarError], Car] but has easy ways accumulate errors that you would have
  // to write yourself if you used Either[List[CarError], Car]
  def apply(name: String, modelYear: Int): Validated[NonEmptyList[CarError],Car] = {
    val nameResult: ValidatedNel[CarError, String] = if (name.isEmpty) {
      invalidNel(new NameEmptyError)
    } else {
      valid(name)
    }
    val yearResult: ValidatedNel[CarError, Int] = if (modelYear < 1900 || modelYear > LocalDateTime.now().getYear) {
      invalidNel(new ModelYearOutsideValidRangeError)
    } else {
      valid(modelYear)
    }

    // Combine the validated fields into the car
    // Instead of forming a monad to chain results,
    // Validated is an applicative functor that allows us to
    // consolidate all of the errors or the successful results
    // Either the success case is a tuple with the valid values
    // or the failure case is a Non-Empty list of CarErrors.
    (nameResult |@| yearResult).map(
      (name, year) => new Car(name, year)
    )
  }
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
