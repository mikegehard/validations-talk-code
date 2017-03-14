package validations.either

import java.time.LocalDateTime

/** ******** Errors *****************/
sealed trait CarError {
  val message: String
}

class NameEmptyError extends CarError {
  override val message: String = "Invalid car name!"
}

class ModelYearOutsideValidRangeError extends CarError {
  override val message: String = "Invalid car model name!"
}

class DisplacementOutOfValidRangeError extends CarError {
  override val message: String = "Invalid engine displacement!"
}


class Car(name: String, modelYear: Int, engine: Engine) {
  override def toString: String = {
    s"Car: name=$name, model year=$modelYear, engine=$engine"
  }
}

/** ******** Domain objects *****************/
object Car {
  // This type signature now tells you that either an error can happen or a car is created.
  def apply(name: String, modelYear: Int, engine: Engine): Either[CarError, Car] = {
    if (name.isEmpty) {
      Left(new NameEmptyError)
    } else if (modelYear < 1900 || modelYear > LocalDateTime.now().getYear) {
      Left(new ModelYearOutsideValidRangeError)
    } else {
      Right(new Car(name, modelYear, engine))
    }
  }
}

class Engine(displacementInCubicCentimeters: Int) {
  override def toString: String = {
    s"Engine: displacement=$displacementInCubicCentimeters"
  }
}

object Engine {
  // This type signature now tells you that either an error can happen or an engine is created.
  def apply(displacementInCubicCentimeters: Int): Either[CarError, Engine] = {
    if (displacementInCubicCentimeters < 650 || displacementInCubicCentimeters > 5700) {
      Left(new DisplacementOutOfValidRangeError)
    } else {
      Right(new Engine(displacementInCubicCentimeters))
    }
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

    val validDisplacement = 1000
    val invalidDisplacement = 2

    val validCar: Either[CarError, Car] = for {
      e <- Engine(validDisplacement)
      c <- Car(validName, validModelYear, e)
    } yield c

    validCar.fold(handleInvalidCar, handleValidCar)

    val invalidCar: Either[CarError, Car] = for {
      e <- Engine(invalidDisplacement)
      c <- Car(invalidName, invalidModelYear, e)
    } yield c

    // This is better but we only learn about the first error that happens that
    // makes the whole car invalid.
    // A user would need to submit data multiple times to get a valid car.
    invalidCar.fold(handleInvalidCar, handleValidCar)
  }

  private def handleInvalidCar(e: CarError) =
    println(s"******* Invalid car! Reason: ${e.message}! *******")

  private def handleValidCar(c: Car): Unit =
    println(s"******* $c *******")
}
