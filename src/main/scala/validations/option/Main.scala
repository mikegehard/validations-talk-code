package validations.option

import java.time.LocalDateTime

class Car(name: String, modelYear: Int, engine: Engine) {
  override def toString: String = {
    s"Car: name=$name, model year=$modelYear, engine=$engine"
  }
}

/** ******** Domain objects *****************/
object Car {
  // This type signature tells you that something can go wrong.
  def apply(name: String, modelYear: Int, engine: Engine): Option[Car] = {
    if (name.isEmpty) {
      None
    } else if (modelYear < 1900 || modelYear > LocalDateTime.now().getYear) {
      None
    } else {
      Option(new Car(name, modelYear, engine))
    }
  }
}

class Engine(displacementInCubicCentimeters: Int) {
  override def toString: String = {
    s"Engine: displacement=$displacementInCubicCentimeters"
  }
}

object Engine {
  // This type signature tells you that something can go wrong.
  def apply(displacementInCubicCentimeters: Int): Option[Engine] = {
    if (displacementInCubicCentimeters < 650 || displacementInCubicCentimeters > 5700) {
      None
    } else {
      Option(new Engine(displacementInCubicCentimeters))
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

    val validCar: Option[Car] = for {
      engine <- Engine(validDisplacement)
      car <- Car(validName, validModelYear, engine)
    } yield car

    println("************* Valid car *****************")
    validCar.fold(handleInvalidCar)(handleValidCar)

    val invalidCar: Option[Car] = for {
      engine <- Engine(invalidDisplacement)
      car <- Car(invalidName, invalidModelYear, engine)
    } yield car

    // Much nicer than the exception case because we now tell you what happens in both cases, success and failure.
    // Downside here is we have no idea about went wrong, we just know that we have an invalid car.
    println("************* Invalid car *****************")
    invalidCar.fold(handleInvalidCar)(handleValidCar)
  }

  private def handleInvalidCar() =
    println("******* Invalid car! *******")

  private def handleValidCar(c: Car): Unit =
    println(s"******* $c *******")
}
