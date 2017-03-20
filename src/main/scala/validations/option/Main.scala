package validations.option

import java.time.LocalDateTime

class Car(name: String, modelYear: Int) {
  override def toString: String = {
    s"Car: name=$name, model year=$modelYear"
  }
}

/** ******** Domain objects *****************/
object Car {
  // This type signature tells you that something can go wrong.
  def apply(name: String, modelYear: Int): Option[Car] = {
    if (name.isEmpty) {
      None
    } else if (modelYear < 1900 || modelYear > LocalDateTime.now().getYear) {
      None
    } else {
      Option(new Car(name, modelYear))
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

    val validCar: Option[Car] = Car(validName, validModelYear)

    validCar.fold(handleInvalidCar)(handleValidCar)

    val invalidCar: Option[Car] = Car(invalidName, invalidModelYear)

    // Much nicer than the exception case because we now tell you what happens in both cases, success and failure.
    // Downside here is we have no idea about went wrong, we just know that we have an invalid car.
    invalidCar.fold(handleInvalidCar)(handleValidCar)
  }

  // No way to tell what went wrong...you just know that a car didn't get created.
  private def handleInvalidCar() =
    println("******* Invalid car! *******")

  private def handleValidCar(c: Car): Unit =
    println(s"******* $c *******")
}
