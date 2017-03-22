package validations.either

import java.time.LocalDateTime

class Car(name: String, modelYear: Int) {
  override def toString: String =
    s"Car: name=$name, model year=$modelYear"
}

/** ******** Domain objects *****************/
object Car {
  // This type signature now tells you that either an error can happen or a car is created.
  def apply(name: String, modelYear: Int): Either[CarError, Car] = {
    if (name.isEmpty) {
      Left(new NameEmptyError)
    } else if (modelYear < 1900 || modelYear > LocalDateTime.now().getYear) {
      Left(new ModelYearOutsideValidRangeError)
    } else {
      Right(new Car(name, modelYear))
    }
  }
}

/** ******** Errors *****************/
sealed trait CarError {
  val message: String
}

sealed class NameEmptyError extends CarError {
  override val message: String =
    "Invalid car name!"
}

sealed class ModelYearOutsideValidRangeError extends CarError {
  override val message: String =
    "Invalid car model name!"
}

/** ******** Main program *****************/
object Main {
  def main(args: Array[String]): Unit = {
    // Data you get from your user which we can't assume is correct
    val validName = "Ford"
    val invalidName = ""

    val validModelYear = 2015
    val invalidModelYear = 1200

    val validCar: Either[CarError, Car] = Car(validName, validModelYear)

    validCar.fold(handleInvalidCar, handleValidCar)

    val invalidCar: Either[CarError, Car] = Car(invalidName, invalidModelYear)

    // This is better but we only learn about the first error that happens that
    // makes the whole car invalid.
    // A user would need to submit data multiple times to get a valid car.
    invalidCar.fold(handleInvalidCar, handleValidCar)
  }

  // You know what went wrong but you only get the first one.
  // This isn't great because you can't tell the user what they need to fix.
  private def handleInvalidCar(e: CarError) =
    println(s"******* Invalid car! Reason: ${e.message} *******")

  private def handleValidCar(c: Car): Unit =
    println(s"******* $c *******")
}
