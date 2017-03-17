package validations.exceptions

import java.time.LocalDateTime

/** ******** Errors *****************/
sealed trait CarError extends Exception {
  val message: String
}

class NameEmptyError extends CarError {
  override val message: String =
    "Invalid car name!"
}

class ModelYearOutsideValidRangeError extends CarError {
  override val message: String =
    "Invalid car model name!"
}

class Car(name: String, modelYear: Int) {
  override def toString: String =
    s"Car: name=$name, model year=$modelYear"
}

/** ******** Domain objects *****************/
object Car {
  // This type signature lies because it can return a car or throw an exception.
  def apply(name: String, modelYear: Int): Car = {
    if (name.isEmpty) {
      throw new NameEmptyError
    } else if (modelYear < 1900 || modelYear > LocalDateTime.now().getYear) {
      throw new ModelYearOutsideValidRangeError
    } else {
      new Car(name, modelYear)
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

    val validCar: Car = Car(validName, validModelYear)
    println(s"******* $validCar *******")

    // This isn't very composible in a functional codebase
    try {
      // This has multiple problems with it but we only find out about one of them.
      val invalidCar: Car = Car(invalidName, invalidModelYear)
      println(s"******* $invalidCar *******")
    } catch {
      case e: CarError => println(s"******* ${e.message} *******")
    }
  }
}
