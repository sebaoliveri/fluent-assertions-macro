package org.nulluncertainty.macros

import org.nulluncertainty.assertion.AssertionFailureException
import org.scalatest.{Matchers, WordSpec}

import scala.util.Try

class EnabledAssertionsSpec extends WordSpec with Matchers {

  "test" in {
    Try(Discount(percentage = 101))
    .recover { case AssertionFailureException(errors) =>
      errors.map(println)
    }
  }
}
