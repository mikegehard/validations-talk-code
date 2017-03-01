package validations.exceptions

import java.time.LocalDateTime

/** ******** Errors *****************/
sealed trait CarError extends Exception {
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
  // This type signature lies because it can return a car or throw an exception.
  def apply(name: String, modelYear: Int, engine: Engine): Car = {
    if (name.isEmpty) {
      throw new NameEmptyError
    } else if (modelYear < 1900 || modelYear > LocalDateTime.now().getYear) {
      throw new ModelYearOutsideValidRangeError
    } else {
      new Car(name, modelYear, engine)
    }
  }
}

class Engine(displacementInCubicCentimeters: Int) {
  override def toString: String = {
    s"Engine: displacement=$displacementInCubicCentimeters"
  }
}

object Engine {
  // This type signature lies because it can return an engine or throw an exception.
  def apply(displacementInCubicCentimeters: Int): Engine = {
    if (displacementInCubicCentimeters < 650 || displacementInCubicCentimeters > 5700) {
      throw new DisplacementOutOfValidRangeError
    } else {
      new Engine(displacementInCubicCentimeters)
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

    val validCar = Car(validName, validModelYear, Engine(validDisplacement))
    println(s"******* $validCar *******")

    // This isn't very composable in a functional codebase
    try {
      // This has multiple problems with it but we only find out about one of them.
      val invalidCar = Car(invalidName, invalidModelYear, Engine(invalidDisplacement))
      println(s"******* $invalidCar *******")
    } catch {
      case e: CarError => println(s"******* ${e.message} *******")
    }
  }
}
