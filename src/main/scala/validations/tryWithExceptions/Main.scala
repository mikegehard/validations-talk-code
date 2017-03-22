package validations.tryWithExceptions

import java.time.LocalDateTime

import scala.util.Try

// Make constructor private so you can't construct an invalid Car.
class Car private(name: String, modelYear: Int) {
  override def toString: String =
    s"Car: name=$name, model year=$modelYear"
}

/** ******** Domain objects *****************/
object Car {
  // Put validations here so that you can only create valid cars.
  // This type signature reflects reality because Try signals an exception could occur
  def apply(name: String, modelYear: Int): Try[Car] = {
    Try(
      if (name.isEmpty) {
        throw new NameEmptyError
      } else if (modelYear < 1900 || modelYear > LocalDateTime.now().getYear) {
        throw new ModelYearOutsideValidRangeError
      } else {
        new Car(name, modelYear)
      }
    )
  }
}

/** ******** Errors *****************/
sealed trait CarError extends Exception

class NameEmptyError extends CarError {
  override def getMessage: String = "Invalid car name!"
}

class ModelYearOutsideValidRangeError extends CarError {
  override def getMessage: String = "Invalid car model name!"
}


/** ******** Main program *****************/
object Main {
  def main(args: Array[String]): Unit = {
    // Data you get from your user which we can't assume is correct
    val validName = "Ford"
    val invalidName = ""

    val validModelYear = 2015
    val invalidModelYear = 1200

    val validCar: Try[Car] = Car(validName, validModelYear)
    validCar.fold(handleInvalidCar, handleValidCar)

    val invalidCar: Try[Car] = Car(invalidName, invalidModelYear)
    invalidCar.fold(handleInvalidCar, handleValidCar)
  }

  // Only one error is handled even if there are multiple.
  private def handleInvalidCar(e: Throwable): Unit =
    println(s"******* Invalid car! ${e.getMessage} *******")

  private def handleValidCar(c: Car): Unit =
    println(s"******* $c *******")
}
