package org.nulluncertainty.macros

import org.nulluncertainty.assertion.AssertionFailureException
import org.nulluncertainty.macros.EnabledAssertionsImpl.Error
import org.scalatest.{Matchers, WordSpec}

import scala.util.{Failure, Success, Try}

class EnabledAssertionsSpec extends WordSpec with Matchers {

  def assertSuccess(theTry: Try[_]): Unit =
    theTry match {
      case Success(_) =>
      case Failure(_) => fail()
    }
  def assertFailure(theTry: Try[_], expectedErrors: Error*): Unit =
    theTry match {
      case Success(_) => fail()
      case Failure(_@AssertionFailureException(errors)) => errors should be(expectedErrors)
    }

  "ConstrainedNumber must report the first violated constraint" when {
    "Int_GreaterThanAndLessThan5" in {
      assertFailure(Try(ConstrainedIntGreaterThan10AndLessThan5(7)),
        Error(property = "number", violatedConstraint = "GreaterThan", message = "7 is not greater than 10"))
    }

    "Wrapped_Int_GreaterThanAndLessThan5" in {
      assertFailure(Try(IntWrapperGreaterThan10AndLessThan5(WrappedIntGreaterThan10AndLessThan5(7))),
        Error(property = "wrapped.number", violatedConstraint = "GreaterThan", message = "7 is not greater than 10"))
    }

    "Some_Int_GreaterThanAndLessThan5" in {
      assertSuccess(Try(ConstrainedSomeIntGreaterThan10AndLessThan5(None)))
      assertFailure(Try(ConstrainedSomeIntGreaterThan10AndLessThan5(Some(7))),
        Error(property = "number", violatedConstraint = "GreaterThan", message = "7 is not greater than 10"))
    }

    "Wrapped_Some_Int_GreaterThanAndLessThan5" in {
      assertSuccess(Try(SomeIntWrapperGreaterThan10AndLessThan5(WrappedSomeIntGreaterThan10AndLessThan5(None))))
      assertFailure(Try(SomeIntWrapperGreaterThan10AndLessThan5(WrappedSomeIntGreaterThan10AndLessThan5(Some(7)))),
        Error(property = "wrapped.number", violatedConstraint = "GreaterThan", message = "7 is not greater than 10"))
    }
  }

  //EqualsTo for Numbers and Maybe Numbers

  "Int_EqualsTo" in {
    assertSuccess(Try(ConstrainedIntEqualsTo10(10)))
    assertFailure(Try(ConstrainedIntEqualsTo10(11)),
      Error(property = "number", violatedConstraint = "EqualsTo", message = "11 is not equal to 10"))
  }

  "Wrapped_Int_EqualsTo" in {
    assertSuccess(Try(IntWrapperEqualsTo10(WrappedIntEqualsTo10(10))))
    assertFailure(Try(IntWrapperEqualsTo10(WrappedIntEqualsTo10(11))),
      Error(property = "wrapped.number", violatedConstraint = "EqualsTo", message = "11 is not equal to 10"))
  }

  "Some_Int_EqualsTo" in {
    assertSuccess(Try(ConstrainedSomeIntEqualsTo10(None)))
    assertSuccess(Try(ConstrainedSomeIntEqualsTo10(Some(10))))
    assertFailure(Try(ConstrainedSomeIntEqualsTo10(Some(11))),
      Error(property = "number", violatedConstraint = "EqualsTo", message = "11 is not equal to 10"))
  }

  "Wrapped_Some_Int_EqualsTo" in {
    assertSuccess(Try(SomeIntWrapperEqualsTo10(WrappedSomeIntEqualsTo10(None))))
    assertSuccess(Try(SomeIntWrapperEqualsTo10(WrappedSomeIntEqualsTo10(Some(10)))))
    assertFailure(Try(SomeIntWrapperEqualsTo10(WrappedSomeIntEqualsTo10(Some(11)))),
      Error(property = "wrapped.number", violatedConstraint = "EqualsTo", message = "11 is not equal to 10"))
  }

  //GreaterThan for Numbers and Maybe Numbers

  "Int_GreaterThan" in {
    assertSuccess(Try(ConstrainedIntGreaterThan10(11)))
    assertFailure(Try(ConstrainedIntGreaterThan10(10)),
      Error(property = "number", violatedConstraint = "GreaterThan", message = "10 is not greater than 10"))
  }

  "Wrapped_Int_GreaterThan" in {
    assertSuccess(Try(IntWrapperGreaterThan10(WrappedIntGreaterThan10(11))))
    assertFailure(Try(IntWrapperGreaterThan10(WrappedIntGreaterThan10(10))),
      Error(property = "wrapped.number", violatedConstraint = "GreaterThan", message = "10 is not greater than 10"))
  }

  "Some_Int_GreaterThan" in {
    assertSuccess(Try(ConstrainedSomeIntGreaterThan10(None)))
    assertSuccess(Try(ConstrainedSomeIntGreaterThan10(Some(11))))
    assertFailure(Try(ConstrainedSomeIntGreaterThan10(Some(10))),
      Error(property = "number", violatedConstraint = "GreaterThan", message = "10 is not greater than 10"))
  }

  "Wrapped_Some_Int_GreaterThan" in {
    assertSuccess(Try(SomeIntWrapperGreaterThan10(WrappedSomeIntGreaterThan10(None))))
    assertSuccess(Try(SomeIntWrapperGreaterThan10(WrappedSomeIntGreaterThan10(Some(11)))))
    assertFailure(Try(SomeIntWrapperGreaterThan10(WrappedSomeIntGreaterThan10(Some(10)))),
      Error(property = "wrapped.number", violatedConstraint = "GreaterThan", message = "10 is not greater than 10"))
  }

  //GreaterThanOrEqualTo for Numbers and Maybe Numbers

  "Int_GreaterThanOrEqualTo" in {
    assertSuccess(Try(ConstrainedIntGreaterThanOrEqualTo10(10)))
    assertFailure(Try(ConstrainedIntGreaterThanOrEqualTo10(9)),
      Error(property = "number", violatedConstraint = "GreaterThanOrEqualTo", message = "9 is not greater than or equal to 10"))
  }

  "Wrapped_Int_GreaterThanOrEqualTo" in {
    assertSuccess(Try(IntWrapperGreaterThanOrEqualTo10(WrappedIntGreaterThanOrEqualTo10(10))))
    assertFailure(Try(IntWrapperGreaterThanOrEqualTo10(WrappedIntGreaterThanOrEqualTo10(9))),
      Error(property = "wrapped.number", violatedConstraint = "GreaterThanOrEqualTo", message = "9 is not greater than or equal to 10"))
  }

  "Some_Int_GreaterThanOrEqualTo" in {
    assertSuccess(Try(ConstrainedSomeIntGreaterThanOrEqualTo10(None)))
    assertSuccess(Try(ConstrainedSomeIntGreaterThanOrEqualTo10(Some(10))))
    assertFailure(Try(ConstrainedSomeIntGreaterThanOrEqualTo10(Some(9))),
      Error(property = "number", violatedConstraint = "GreaterThanOrEqualTo", message = "9 is not greater than or equal to 10"))
  }

  "Wrapped_Some_Int_GreaterThanOrEqualTo" in {
    assertSuccess(Try(SomeIntWrapperGreaterThanOrEqualTo10(WrappedSomeIntGreaterThanOrEqualTo10(None))))
    assertSuccess(Try(SomeIntWrapperGreaterThanOrEqualTo10(WrappedSomeIntGreaterThanOrEqualTo10(Some(10)))))
    assertFailure(Try(SomeIntWrapperGreaterThanOrEqualTo10(WrappedSomeIntGreaterThanOrEqualTo10(Some(9)))),
      Error(property = "wrapped.number", violatedConstraint = "GreaterThanOrEqualTo", message = "9 is not greater than or equal to 10"))
  }

  //LessThan for Numbers and Maybe Numbers

  "Int_LessThan" in {
    assertSuccess(Try(ConstrainedIntLessThan10(9)))
    assertFailure(Try(ConstrainedIntLessThan10(10)),
      Error(property = "number", violatedConstraint = "LessThan", message = "10 is not less than 10"))
  }

  "Wrapped_Int_LessThan" in {
    assertSuccess(Try(IntWrapperLessThan10(WrappedIntLessThan10(9))))
    assertFailure(Try(IntWrapperLessThan10(WrappedIntLessThan10(10))),
      Error(property = "wrapped.number", violatedConstraint = "LessThan", message = "10 is not less than 10"))
  }

  "Some_Int_LessThan" in {
    assertSuccess(Try(ConstrainedSomeIntLessThan10(None)))
    assertSuccess(Try(ConstrainedSomeIntLessThan10(Some(9))))
    assertFailure(Try(ConstrainedSomeIntLessThan10(Some(10))),
      Error(property = "number", violatedConstraint = "LessThan", message = "10 is not less than 10"))
  }

  "Wrapped_Some_Int_LessThan" in {
    assertSuccess(Try(SomeIntWrapperLessThan10(WrappedSomeIntLessThan10(None))))
    assertSuccess(Try(SomeIntWrapperLessThan10(WrappedSomeIntLessThan10(Some(9)))))
    assertFailure(Try(SomeIntWrapperLessThan10(WrappedSomeIntLessThan10(Some(10)))),
      Error(property = "wrapped.number", violatedConstraint = "LessThan", message = "10 is not less than 10"))
  }

  //LessThanOrEqualTo for Numbers and Maybe Numbers

  "Int_LessThanOrEqualTo" in {
    assertSuccess(Try(ConstrainedIntLessThanOrEqualTo10(10)))
    assertFailure(Try(ConstrainedIntLessThanOrEqualTo10(11)),
      Error(property = "number", violatedConstraint = "LessThanOrEqualTo", message = "11 is not less than or equal to 10"))
  }

  "Wrapped_Int_LessThanOrEqualTo" in {
    assertSuccess(Try(IntWrapperLessThanOrEqualTo10(WrappedIntLessThanOrEqualTo10(10))))
    assertFailure(Try(IntWrapperLessThanOrEqualTo10(WrappedIntLessThanOrEqualTo10(11))),
      Error(property = "wrapped.number", violatedConstraint = "LessThanOrEqualTo", message = "11 is not less than or equal to 10"))
  }

  "Some_Int_LessThanOrEqualTo" in {
    assertSuccess(Try(ConstrainedSomeIntLessThanOrEqualTo10(None)))
    assertSuccess(Try(ConstrainedSomeIntLessThanOrEqualTo10(Some(10))))
    assertFailure(Try(ConstrainedSomeIntLessThanOrEqualTo10(Some(11))),
      Error(property = "number", violatedConstraint = "LessThanOrEqualTo", message = "11 is not less than or equal to 10"))
  }

  "Wrapped_Some_Int_LessThanOrEqualTo" in {
    assertSuccess(Try(SomeIntWrapperLessThanOrEqualTo10(WrappedSomeIntLessThanOrEqualTo10(None))))
    assertSuccess(Try(SomeIntWrapperLessThanOrEqualTo10(WrappedSomeIntLessThanOrEqualTo10(Some(10)))))
    assertFailure(Try(SomeIntWrapperLessThanOrEqualTo10(WrappedSomeIntLessThanOrEqualTo10(Some(11)))),
      Error(property = "wrapped.number", violatedConstraint = "LessThanOrEqualTo", message = "11 is not less than or equal to 10"))
  }

  //InclusiveRange for Numbers and Maybe Numbers

  "Int_InclusiveRange" in {
    assertSuccess(Try(ConstrainedIntInclusiveRangeFrom0To100(0)))
    assertSuccess(Try(ConstrainedIntInclusiveRangeFrom0To100(50)))
    assertSuccess(Try(ConstrainedIntInclusiveRangeFrom0To100(100)))
    assertFailure(Try(ConstrainedIntInclusiveRangeFrom0To100(101)),
      Error(property = "number", violatedConstraint = "InclusiveRange", message = "101 is not within the inclusive range of 0 to 100"))
    assertFailure(Try(ConstrainedIntInclusiveRangeFrom0To100(-1)),
      Error(property = "number", violatedConstraint = "InclusiveRange", message = "-1 is not within the inclusive range of 0 to 100"))
  }

  "Wrapped_Int_InclusiveRange" in {
    assertSuccess(Try(IntWrapperInclusiveRangeFrom0To100(WrappedIntInclusiveRangeFrom0To100(0))))
    assertSuccess(Try(IntWrapperInclusiveRangeFrom0To100(WrappedIntInclusiveRangeFrom0To100(50))))
    assertSuccess(Try(IntWrapperInclusiveRangeFrom0To100(WrappedIntInclusiveRangeFrom0To100(100))))
    assertFailure(Try(IntWrapperInclusiveRangeFrom0To100(WrappedIntInclusiveRangeFrom0To100(101))),
      Error(property = "wrapped.number", violatedConstraint = "InclusiveRange", message = "101 is not within the inclusive range of 0 to 100"))
    assertFailure(Try(IntWrapperInclusiveRangeFrom0To100(WrappedIntInclusiveRangeFrom0To100(-1))),
      Error(property = "wrapped.number", violatedConstraint = "InclusiveRange", message = "-1 is not within the inclusive range of 0 to 100"))
  }

  "Some_Int_InclusiveRange" in {
    assertSuccess(Try(ConstrainedSomeIntInclusiveRangeFrom0To100(None)))
    assertSuccess(Try(ConstrainedSomeIntInclusiveRangeFrom0To100(Some(0))))
    assertSuccess(Try(ConstrainedSomeIntInclusiveRangeFrom0To100(Some(50))))
    assertSuccess(Try(ConstrainedSomeIntInclusiveRangeFrom0To100(Some(100))))
    assertFailure(Try(ConstrainedSomeIntInclusiveRangeFrom0To100(Some(101))),
      Error(property = "number", violatedConstraint = "InclusiveRange", message = "101 is not within the inclusive range of 0 to 100"))
    assertFailure(Try(ConstrainedSomeIntInclusiveRangeFrom0To100(Some(-1))),
      Error(property = "number", violatedConstraint = "InclusiveRange", message = "-1 is not within the inclusive range of 0 to 100"))
  }

  "Wrapped_Some_Int_InclusiveRange" in {
    assertSuccess(Try(SomeIntWrapperInclusiveRangeFrom0To100(WrappedSomeIntInclusiveRangeFrom0To100(None))))
    assertSuccess(Try(SomeIntWrapperInclusiveRangeFrom0To100(WrappedSomeIntInclusiveRangeFrom0To100(Some(0)))))
    assertSuccess(Try(SomeIntWrapperInclusiveRangeFrom0To100(WrappedSomeIntInclusiveRangeFrom0To100(Some(50)))))
    assertSuccess(Try(SomeIntWrapperInclusiveRangeFrom0To100(WrappedSomeIntInclusiveRangeFrom0To100(Some(100)))))
    assertFailure(Try(SomeIntWrapperInclusiveRangeFrom0To100(WrappedSomeIntInclusiveRangeFrom0To100(Some(101)))),
      Error(property = "wrapped.number", violatedConstraint = "InclusiveRange", message = "101 is not within the inclusive range of 0 to 100"))
    assertFailure(Try(SomeIntWrapperInclusiveRangeFrom0To100(WrappedSomeIntInclusiveRangeFrom0To100(Some(-1)))),
      Error(property = "wrapped.number", violatedConstraint = "InclusiveRange", message = "-1 is not within the inclusive range of 0 to 100"))
  }

  //ExclusiveRange for Numbers and Maybe Numbers

  "Int_ExclusiveRange" in {
    assertSuccess(Try(ConstrainedIntExclusiveRangeFrom0To100(1)))
    assertSuccess(Try(ConstrainedIntExclusiveRangeFrom0To100(50)))
    assertSuccess(Try(ConstrainedIntExclusiveRangeFrom0To100(99)))
    assertFailure(Try(ConstrainedIntExclusiveRangeFrom0To100(0)),
      Error(property = "number", violatedConstraint = "ExclusiveRange", message = "0 is not within the exclusive range of 0 to 100"))
    assertFailure(Try(ConstrainedIntExclusiveRangeFrom0To100(100)),
      Error(property = "number", violatedConstraint = "ExclusiveRange", message = "100 is not within the exclusive range of 0 to 100"))
  }

  "Wrapped_Int_ExclusiveRange" in {
    assertSuccess(Try(IntWrapperExclusiveRangeFrom0To100(WrappedIntExclusiveRangeFrom0To100(1))))
    assertSuccess(Try(IntWrapperExclusiveRangeFrom0To100(WrappedIntExclusiveRangeFrom0To100(50))))
    assertSuccess(Try(IntWrapperExclusiveRangeFrom0To100(WrappedIntExclusiveRangeFrom0To100(99))))
    assertFailure(Try(IntWrapperExclusiveRangeFrom0To100(WrappedIntExclusiveRangeFrom0To100(0))),
      Error(property = "wrapped.number", violatedConstraint = "ExclusiveRange", message = "0 is not within the exclusive range of 0 to 100"))
    assertFailure(Try(IntWrapperExclusiveRangeFrom0To100(WrappedIntExclusiveRangeFrom0To100(100))),
      Error(property = "wrapped.number", violatedConstraint = "ExclusiveRange", message = "100 is not within the exclusive range of 0 to 100"))
  }

  "Some_Int_ExclusiveRange" in {
    assertSuccess(Try(ConstrainedSomeIntExclusiveRangeFrom0To100(None)))
    assertSuccess(Try(ConstrainedSomeIntExclusiveRangeFrom0To100(Some(1))))
    assertSuccess(Try(ConstrainedSomeIntExclusiveRangeFrom0To100(Some(50))))
    assertSuccess(Try(ConstrainedSomeIntExclusiveRangeFrom0To100(Some(99))))
    assertFailure(Try(ConstrainedSomeIntExclusiveRangeFrom0To100(Some(0))),
      Error(property = "number", violatedConstraint = "ExclusiveRange", message = "0 is not within the exclusive range of 0 to 100"))
    assertFailure(Try(ConstrainedSomeIntExclusiveRangeFrom0To100(Some(100))),
      Error(property = "number", violatedConstraint = "ExclusiveRange", message = "100 is not within the exclusive range of 0 to 100"))
  }

  "Wrapped_Some_Int_ExclusiveRange" in {
    assertSuccess(Try(SomeIntWrapperExclusiveRangeFrom0To100(WrappedSomeIntExclusiveRangeFrom0To100(None))))
    assertSuccess(Try(SomeIntWrapperExclusiveRangeFrom0To100(WrappedSomeIntExclusiveRangeFrom0To100(Some(1)))))
    assertSuccess(Try(SomeIntWrapperExclusiveRangeFrom0To100(WrappedSomeIntExclusiveRangeFrom0To100(Some(50)))))
    assertSuccess(Try(SomeIntWrapperExclusiveRangeFrom0To100(WrappedSomeIntExclusiveRangeFrom0To100(Some(99)))))
    assertFailure(Try(SomeIntWrapperExclusiveRangeFrom0To100(WrappedSomeIntExclusiveRangeFrom0To100(Some(0)))),
      Error(property = "wrapped.number", violatedConstraint = "ExclusiveRange", message = "0 is not within the exclusive range of 0 to 100"))
    assertFailure(Try(SomeIntWrapperExclusiveRangeFrom0To100(WrappedSomeIntExclusiveRangeFrom0To100(Some(100)))),
      Error(property = "wrapped.number", violatedConstraint = "ExclusiveRange", message = "100 is not within the exclusive range of 0 to 100"))
  }

  //EqualsTo for String and Maybe String

  "String_EqualsTo" in {
    assertSuccess(Try(ConstrainedStringEqualsToSebastian("Sebastian")))
    assertFailure(Try(ConstrainedStringEqualsToSebastian("sebastian")),
      Error(property = "string", violatedConstraint = "EqualsTo", message = "sebastian is not equal to Sebastian"))
  }

  "Wrapped_String_EqualsTo" in {
    assertSuccess(Try(StringWrapperEqualsToSebastian(WrappedStringEqualsToSebastian("Sebastian"))))
    assertFailure(Try(StringWrapperEqualsToSebastian(WrappedStringEqualsToSebastian("sebastian"))),
      Error(property = "wrapped.string", violatedConstraint = "EqualsTo", message = "sebastian is not equal to Sebastian"))
  }

  "Some_String_EqualsTo" in {
    assertSuccess(Try(ConstrainedSomeStringEqualsToSebastian(None)))
    assertSuccess(Try(ConstrainedSomeStringEqualsToSebastian(Some("Sebastian"))))
    assertFailure(Try(ConstrainedSomeStringEqualsToSebastian(Some("sebastian"))),
      Error(property = "string", violatedConstraint = "EqualsTo", message = "sebastian is not equal to Sebastian"))
  }

  "Wrapped_Some_String_EqualsTo" in {
    assertSuccess(Try(SomeStringWrapperEqualsToSebastian(WrappedSomeStringEqualsToSebastian(None))))
    assertSuccess(Try(SomeStringWrapperEqualsToSebastian(WrappedSomeStringEqualsToSebastian(Some("Sebastian")))))
    assertFailure(Try(SomeStringWrapperEqualsToSebastian(WrappedSomeStringEqualsToSebastian(Some("sebastian")))),
      Error(property = "wrapped.string", violatedConstraint = "EqualsTo", message = "sebastian is not equal to Sebastian"))
  }

  //EqualsToIgnoringCase for String and Maybe String

  "String_EqualsToIgnoringCase" in {
    assertSuccess(Try(ConstrainedStringEqualsToIgnoringCaseSebastian("sebastian")))
    assertFailure(Try(ConstrainedStringEqualsToIgnoringCaseSebastian("Fulanito")),
      Error(property = "string", violatedConstraint = "EqualsToIgnoringCase", message = "Fulanito is not equal to Sebastian"))
  }

  "Wrapped_String_EqualsToIgnoringCase" in {
    assertSuccess(Try(StringWrapperEqualsToIgnoringCaseSebastian(WrappedStringEqualsToIgnoringCaseSebastian("sebastian"))))
    assertFailure(Try(StringWrapperEqualsToIgnoringCaseSebastian(WrappedStringEqualsToIgnoringCaseSebastian("Fulanito"))),
      Error(property = "wrapped.string", violatedConstraint = "EqualsToIgnoringCase", message = "Fulanito is not equal to Sebastian"))
  }

  "Some_String_EqualsToIgnoringCase" in {
    assertSuccess(Try(ConstrainedSomeStringEqualsToIgnoringCaseSebastian(None)))
    assertSuccess(Try(ConstrainedSomeStringEqualsToIgnoringCaseSebastian(Some("sebastian"))))
    assertFailure(Try(ConstrainedSomeStringEqualsToIgnoringCaseSebastian(Some("Fulanito"))),
      Error(property = "string", violatedConstraint = "EqualsToIgnoringCase", message = "Fulanito is not equal to Sebastian"))
  }

  "Wrapped_Some_String_EqualsToIgnoringCase" in {
    assertSuccess(Try(SomeStringWrapperEqualsToIgnoringCaseSebastian(WrappedSomeStringEqualsToIgnoringCaseSebastian(None))))
    assertSuccess(Try(SomeStringWrapperEqualsToIgnoringCaseSebastian(WrappedSomeStringEqualsToIgnoringCaseSebastian(Some("sebastian")))))
    assertFailure(Try(SomeStringWrapperEqualsToIgnoringCaseSebastian(WrappedSomeStringEqualsToIgnoringCaseSebastian(Some("Fulanito")))),
      Error(property = "wrapped.string", violatedConstraint = "EqualsToIgnoringCase", message = "Fulanito is not equal to Sebastian"))
  }

  "ConstrainedString must report the first violated constraint" when {
    "String_EndsWithStartsWith" in {
      assertFailure(Try(ConstrainedStringEndsWithStartsWithCucho("fulanito")),
        Error(property = "string", violatedConstraint = "EndsWith", message = "fulanito does not end with suffix cucho"))
    }

    "Wrapped_String_EndsWithStartsWith" in {
      assertFailure(Try(StringWrapperEndsWithStartsWithCucho(WrappedStringEndsWithStartsWithCucho("fulanito"))),
        Error(property = "wrapped.string", violatedConstraint = "EndsWith", message = "fulanito does not end with suffix cucho"))
    }

    "Some_String_EndsWithStartsWith" in {
      assertSuccess(Try(ConstrainedSomeStringEndsWithStartsWithCucho(None)))
      assertFailure(Try(ConstrainedSomeStringEndsWithStartsWithCucho(Some("fulanito"))),
        Error(property = "string", violatedConstraint = "EndsWith", message = "fulanito does not end with suffix cucho"))
    }

    "Wrapped_Some_String_StartsWith" in {
      assertSuccess(Try(SomeStringWrapperEndsWithStartsWithCucho(WrappedSomeStringEndsWithStartsWithCucho(None))))
      assertFailure(Try(SomeStringWrapperEndsWithStartsWithCucho(WrappedSomeStringEndsWithStartsWithCucho(Some("fulanito")))),
        Error(property = "wrapped.string", violatedConstraint = "EndsWith", message = "fulanito does not end with suffix cucho"))
    }
  }

  //StartsWith for String and Maybe String

  "String_StartsWith" in {
    assertSuccess(Try(ConstrainedStringStartsWithSebas("Sebas")))
    assertFailure(Try(ConstrainedStringStartsWithSebas("sebastian")),
      Error(property = "string", violatedConstraint = "StartsWith", message = "sebastian does not start with prefix Sebas"))
  }

  "Wrapped_String_StartsWith" in {
    assertSuccess(Try(StringWrapperStartsWithSebas(WrappedStringStartsWithSebas("Sebastian"))))
    assertFailure(Try(StringWrapperStartsWithSebas(WrappedStringStartsWithSebas("sebastian"))),
      Error(property = "wrapped.string", violatedConstraint = "StartsWith", message = "sebastian does not start with prefix Sebas"))
  }

  "Some_String_StartsWith" in {
    assertSuccess(Try(ConstrainedSomeStringStartsWithSebas(None)))
    assertSuccess(Try(ConstrainedSomeStringStartsWithSebas(Some("Sebastian"))))
    assertFailure(Try(ConstrainedSomeStringStartsWithSebas(Some("sebastian"))),
      Error(property = "string", violatedConstraint = "StartsWith", message = "sebastian does not start with prefix Sebas"))
  }

  "Wrapped_Some_String_StartsWith" in {
    assertSuccess(Try(SomeStringWrapperStartsWithSebas(WrappedSomeStringStartsWithSebas(None))))
    assertSuccess(Try(SomeStringWrapperStartsWithSebas(WrappedSomeStringStartsWithSebas(Some("Sebastian")))))
    assertFailure(Try(SomeStringWrapperStartsWithSebas(WrappedSomeStringStartsWithSebas(Some("sebastian")))),
      Error(property = "wrapped.string", violatedConstraint = "StartsWith", message = "sebastian does not start with prefix Sebas"))
  }

  //StartsWithIgnoringCase for String and Maybe String

  "String_StartsWithIgnoringCase" in {
    assertSuccess(Try(ConstrainedStringStartsWithIgnoringCaseSebas("sebastian")))
    assertFailure(Try(ConstrainedStringStartsWithIgnoringCaseSebas("Fulanito")),
      Error(property = "string", violatedConstraint = "StartsWithIgnoringCase", message = "Fulanito does not start with prefix Sebas"))
  }

  "Wrapped_String_StartsWithIgnoringCase" in {
    assertSuccess(Try(StringWrapperStartsWithIgnoringCaseSebas(WrappedStringStartsWithIgnoringCaseSebas("sebastian"))))
    assertFailure(Try(StringWrapperStartsWithIgnoringCaseSebas(WrappedStringStartsWithIgnoringCaseSebas("Fulanito"))),
      Error(property = "wrapped.string", violatedConstraint = "StartsWithIgnoringCase", message = "Fulanito does not start with prefix Sebas"))
  }

  "Some_String_StartsWithIgnoringCase" in {
    assertSuccess(Try(ConstrainedSomeStringStartsWithIgnoringCaseSebas(None)))
    assertSuccess(Try(ConstrainedSomeStringStartsWithIgnoringCaseSebas(Some("sebastian"))))
    assertFailure(Try(ConstrainedSomeStringStartsWithIgnoringCaseSebas(Some("Fulanito"))),
      Error(property = "string", violatedConstraint = "StartsWithIgnoringCase", message = "Fulanito does not start with prefix Sebas"))
  }

  "Wrapped_Some_String_StartsWithIgnoringCase" in {
    assertSuccess(Try(SomeStringWrapperStartsWithIgnoringCaseSebas(WrappedSomeStringStartsWithIgnoringCaseSebas(None))))
    assertSuccess(Try(SomeStringWrapperStartsWithIgnoringCaseSebas(WrappedSomeStringStartsWithIgnoringCaseSebas(Some("sebastian")))))
    assertFailure(Try(SomeStringWrapperStartsWithIgnoringCaseSebas(WrappedSomeStringStartsWithIgnoringCaseSebas(Some("Fulanito")))),
      Error(property = "wrapped.string", violatedConstraint = "StartsWithIgnoringCase", message = "Fulanito does not start with prefix Sebas"))
  }

  //Contains for String and Maybe String

  "String_Contains" in {
    assertSuccess(Try(ConstrainedStringContainsTian("sebastian")))
    assertFailure(Try(ConstrainedStringContainsTian("sebastIAN")),
      Error(property = "string", violatedConstraint = "Contains", message = "sebastIAN does not contain tian"))
  }

  "Wrapped_String_Contains" in {
    assertSuccess(Try(StringWrapperContainsTian(WrappedStringContainsTian("sebastian"))))
    assertFailure(Try(StringWrapperContainsTian(WrappedStringContainsTian("sebastIAN"))),
      Error(property = "wrapped.string", violatedConstraint = "Contains", message = "sebastIAN does not contain tian"))
  }

  "Some_String_Contains" in {
    assertSuccess(Try(ConstrainedSomeStringContainsTian(None)))
    assertSuccess(Try(ConstrainedSomeStringContainsTian(Some("sebastian"))))
    assertFailure(Try(ConstrainedSomeStringContainsTian(Some("sebastIAN"))),
      Error(property = "string", violatedConstraint = "Contains", message = "sebastIAN does not contain tian"))
  }

  "Wrapped_Some_String_Contains" in {
    assertSuccess(Try(SomeStringWrapperContainsTian(WrappedSomeStringContainsTian(None))))
    assertSuccess(Try(SomeStringWrapperContainsTian(WrappedSomeStringContainsTian(Some("sebastian")))))
    assertFailure(Try(SomeStringWrapperContainsTian(WrappedSomeStringContainsTian(Some("sebastIAN")))),
      Error(property = "wrapped.string", violatedConstraint = "Contains", message = "sebastIAN does not contain tian"))
  }

  //ContainsIgnoringCase for String and Maybe String

  "String_ContainsIgnoringCase" in {
    assertSuccess(Try(ConstrainedStringContainsIgnoringCaseTian("sebastIAN")))
    assertFailure(Try(ConstrainedStringContainsIgnoringCaseTian("sebastain")),
      Error(property = "string", violatedConstraint = "ContainsIgnoringCase", message = "sebastain does not contain tian"))
  }

  "Wrapped_String_ContainsIgnoringCase" in {
    assertSuccess(Try(StringWrapperContainsIgnoringCaseTian(WrappedStringContainsIgnoringCaseTian("sebastIAN"))))
    assertFailure(Try(StringWrapperContainsIgnoringCaseTian(WrappedStringContainsIgnoringCaseTian("sebastain"))),
      Error(property = "wrapped.string", violatedConstraint = "ContainsIgnoringCase", message = "sebastain does not contain tian"))
  }

  "Some_String_ContainsIgnoringCase" in {
    assertSuccess(Try(ConstrainedSomeStringContainsIgnoringCaseTian(None)))
    assertSuccess(Try(ConstrainedSomeStringContainsIgnoringCaseTian(Some("sebastIAN"))))
    assertFailure(Try(ConstrainedSomeStringContainsIgnoringCaseTian(Some("sebastain"))),
      Error(property = "string", violatedConstraint = "ContainsIgnoringCase", message = "sebastain does not contain tian"))
  }

  "Wrapped_Some_String_ContainsIgnoringCase" in {
    assertSuccess(Try(SomeStringWrapperContainsIgnoringCaseTian(WrappedSomeStringContainsIgnoringCaseTian(None))))
    assertSuccess(Try(SomeStringWrapperContainsIgnoringCaseTian(WrappedSomeStringContainsIgnoringCaseTian(Some("sebastIAN")))))
    assertFailure(Try(SomeStringWrapperContainsIgnoringCaseTian(WrappedSomeStringContainsIgnoringCaseTian(Some("sebastain")))),
      Error(property = "wrapped.string", violatedConstraint = "ContainsIgnoringCase", message = "sebastain does not contain tian"))
  }

  //Match for String and Maybe String

  "String_Matches" in {
    assertSuccess(Try(ConstrainedStringMatchesNumber("123")))
    assertFailure(Try(ConstrainedStringMatchesNumber("12a")),
      Error(property = "string", violatedConstraint = "Matches", message = "12a does not match ^[0-9]+$"))
  }

  "Wrapped_String_Matches" in {
    assertSuccess(Try(StringWrapperMatchesNumber(WrappedStringMatchesNumber("123"))))
    assertFailure(Try(StringWrapperMatchesNumber(WrappedStringMatchesNumber("12a"))),
      Error(property = "wrapped.string", violatedConstraint = "Matches", message = "12a does not match ^[0-9]+$"))
  }

  "Some_String_Matches" in {
    assertSuccess(Try(ConstrainedSomeStringMatchesNumber(None)))
    assertSuccess(Try(ConstrainedSomeStringMatchesNumber(Some("123"))))
    assertFailure(Try(ConstrainedSomeStringMatchesNumber(Some("12a"))),
      Error(property = "string", violatedConstraint = "Matches", message = "12a does not match ^[0-9]+$"))
  }

  "Wrapped_Some_String_Matches" in {
    assertSuccess(Try(SomeStringWrapperMatchesNumber(WrappedSomeStringMatchesNumber(None))))
    assertSuccess(Try(SomeStringWrapperMatchesNumber(WrappedSomeStringMatchesNumber(Some("123")))))
    assertFailure(Try(SomeStringWrapperMatchesNumber(WrappedSomeStringMatchesNumber(Some("12a")))),
      Error(property = "wrapped.string", violatedConstraint = "Matches", message = "12a does not match ^[0-9]+$"))
  }

  //Email for String and Maybe String

  "String_Email" in {
    assertSuccess(Try(ConstrainedStringEmail("sebatian@gmail.com")))
    assertFailure(Try(ConstrainedStringEmail("notAnEmail")),
      Error(property = "string", violatedConstraint = "Email", message = "notAnEmail is not a valid email"))
  }

  "Wrapped_String_Email" in {
    assertSuccess(Try(StringWrapperEmail(WrappedStringEmail("sebatian@gmail.com"))))
    assertFailure(Try(StringWrapperEmail(WrappedStringEmail("notAnEmail@"))),
      Error(property = "wrapped.string", violatedConstraint = "Email", message = "notAnEmail@ is not a valid email"))
  }

  "Some_String_Email" in {
    assertSuccess(Try(ConstrainedSomeStringEmail(None)))
    assertSuccess(Try(ConstrainedSomeStringEmail(Some("sebatian@gmail.com"))))
    assertFailure(Try(ConstrainedSomeStringEmail(Some("gmail.com"))),
      Error(property = "string", violatedConstraint = "Email", message = "gmail.com is not a valid email"))
  }

  "Wrapped_Some_String_Email" in {
    assertSuccess(Try(SomeStringWrapperEmail(WrappedSomeStringEmail(None))))
    assertSuccess(Try(SomeStringWrapperEmail(WrappedSomeStringEmail(Some("sebatian@gmail.com")))))
    assertFailure(Try(SomeStringWrapperEmail(WrappedSomeStringEmail(Some("12a")))),
      Error(property = "wrapped.string", violatedConstraint = "Email", message = "12a is not a valid email"))
  }

  //Uri for String and Maybe String

  "String_Uri" in {
    assertSuccess(Try(ConstrainedStringUri("http://google.com")))
    assertFailure(Try(ConstrainedStringUri("sebastian@gmail.com")),
      Error(property = "string", violatedConstraint = "Uri", message = "sebastian@gmail.com is not a valid URI"))
  }

  "Wrapped_String_Uri" in {
    assertSuccess(Try(StringWrapperUri(WrappedStringUri("http://google.com"))))
    assertFailure(Try(StringWrapperUri(WrappedStringUri("notAnUri"))),
      Error(property = "wrapped.string", violatedConstraint = "Uri", message = "notAnUri is not a valid URI"))
  }

  "Some_String_Uri" in {
    assertSuccess(Try(ConstrainedSomeStringUri(None)))
    assertSuccess(Try(ConstrainedSomeStringUri(Some("http://google.com"))))
    assertFailure(Try(ConstrainedSomeStringUri(Some("notAnUri"))),
      Error(property = "string", violatedConstraint = "Uri", message = "notAnUri is not a valid URI"))
  }

  "Wrapped_Some_String_Uri" in {
    assertSuccess(Try(SomeStringWrapperUri(WrappedSomeStringUri(None))))
    assertSuccess(Try(SomeStringWrapperUri(WrappedSomeStringUri(Some("http://google.com")))))
    assertFailure(Try(SomeStringWrapperUri(WrappedSomeStringUri(Some("notAnUri")))),
      Error(property = "wrapped.string", violatedConstraint = "Uri", message = "notAnUri is not a valid URI"))
  }

  //Alphanumeric for String and Maybe String

  "String_Alphanumeric" in {
    assertSuccess(Try(ConstrainedStringAlphanumeric("123 abc")))
    assertFailure(Try(ConstrainedStringAlphanumeric("123 abc $")),
      Error(property = "string", violatedConstraint = "Alphanumeric", message = "123 abc $ is not alphanumeric"))
  }

  "Wrapped_String_Alphanumeric" in {
    assertSuccess(Try(StringWrapperAlphanumeric(WrappedStringAlphanumeric("123 abc"))))
    assertFailure(Try(StringWrapperAlphanumeric(WrappedStringAlphanumeric("123 abc $"))),
      Error(property = "wrapped.string", violatedConstraint = "Alphanumeric", message = "123 abc $ is not alphanumeric"))
  }

  "Some_String_Alphanumeric" in {
    assertSuccess(Try(ConstrainedSomeStringAlphanumeric(None)))
    assertSuccess(Try(ConstrainedSomeStringAlphanumeric(Some("123 abc"))))
    assertFailure(Try(ConstrainedSomeStringAlphanumeric(Some("123 abc $"))),
      Error(property = "string", violatedConstraint = "Alphanumeric", message = "123 abc $ is not alphanumeric"))
  }

  "Wrapped_Some_String_Alphanumeric" in {
    assertSuccess(Try(SomeStringWrapperAlphanumeric(WrappedSomeStringAlphanumeric(None))))
    assertSuccess(Try(SomeStringWrapperAlphanumeric(WrappedSomeStringAlphanumeric(Some("123 abc")))))
    assertFailure(Try(SomeStringWrapperAlphanumeric(WrappedSomeStringAlphanumeric(Some("123 abc $")))),
      Error(property = "wrapped.string", violatedConstraint = "Alphanumeric", message = "123 abc $ is not alphanumeric"))
  }

  //Alphabetic for String and Maybe String

  "String_Alphabetic" in {
    assertSuccess(Try(ConstrainedStringAlphabetic("abc")))
    assertFailure(Try(ConstrainedStringAlphabetic("123 abc")),
      Error(property = "string", violatedConstraint = "Alphabetic", message = "123 abc is not alphabetic"))
  }

  "Wrapped_String_Alphabetic" in {
    assertSuccess(Try(StringWrapperAlphabetic(WrappedStringAlphabetic("abc"))))
    assertFailure(Try(StringWrapperAlphabetic(WrappedStringAlphabetic("123 abc"))),
      Error(property = "wrapped.string", violatedConstraint = "Alphabetic", message = "123 abc is not alphabetic"))
  }

  "Some_String_Alphabetic" in {
    assertSuccess(Try(ConstrainedSomeStringAlphabetic(None)))
    assertSuccess(Try(ConstrainedSomeStringAlphabetic(Some("abc"))))
    assertFailure(Try(ConstrainedSomeStringAlphabetic(Some("123 abc"))),
      Error(property = "string", violatedConstraint = "Alphabetic", message = "123 abc is not alphabetic"))
  }

  "Wrapped_Some_String_Alphabetic" in {
    assertSuccess(Try(SomeStringWrapperAlphabetic(WrappedSomeStringAlphabetic(None))))
    assertSuccess(Try(SomeStringWrapperAlphabetic(WrappedSomeStringAlphabetic(Some("abc")))))
    assertFailure(Try(SomeStringWrapperAlphabetic(WrappedSomeStringAlphabetic(Some("123 abc")))),
      Error(property = "wrapped.string", violatedConstraint = "Alphabetic", message = "123 abc is not alphabetic"))
  }

  //Number for String and Maybe String

  "String_Number" in {
    assertSuccess(Try(ConstrainedStringNumber("123")))
    assertFailure(Try(ConstrainedStringNumber("abc")),
      Error(property = "string", violatedConstraint = "Number", message = "abc is not a number"))
  }

  "Wrapped_String_Number" in {
    assertSuccess(Try(StringWrapperNumber(WrappedStringNumber("123"))))
    assertFailure(Try(StringWrapperNumber(WrappedStringNumber("abc"))),
      Error(property = "wrapped.string", violatedConstraint = "Number", message = "abc is not a number"))
  }

  "Some_String_Number" in {
    assertSuccess(Try(ConstrainedSomeStringNumber(None)))
    assertSuccess(Try(ConstrainedSomeStringNumber(Some("123"))))
    assertFailure(Try(ConstrainedSomeStringNumber(Some("abc"))),
      Error(property = "string", violatedConstraint = "Number", message = "abc is not a number"))
  }

  "Wrapped_Some_String_Number" in {
    assertSuccess(Try(SomeStringWrapperNumber(WrappedSomeStringNumber(None))))
    assertSuccess(Try(SomeStringWrapperNumber(WrappedSomeStringNumber(Some("123")))))
    assertFailure(Try(SomeStringWrapperNumber(WrappedSomeStringNumber(Some("abc")))),
      Error(property = "wrapped.string", violatedConstraint = "Number", message = "abc is not a number"))
  }

  //LongerThan for String and Maybe String

  "String_LongerThan" in {
    assertSuccess(Try(ConstrainedStringLongerThanFive("123456")))
    assertFailure(Try(ConstrainedStringLongerThanFive("12345")),
      Error(property = "string", violatedConstraint = "LongerThan", message = "12345 length is not longer than 5"))
  }

  "Wrapped_String_LongerThan" in {
    assertSuccess(Try(StringWrapperLongerThanFive(WrappedStringLongerThanFive("123456"))))
    assertFailure(Try(StringWrapperLongerThanFive(WrappedStringLongerThanFive("12345"))),
      Error(property = "wrapped.string", violatedConstraint = "LongerThan", message = "12345 length is not longer than 5"))
  }

  "Some_String_LongerThan" in {
    assertSuccess(Try(ConstrainedSomeStringLongerThanFive(None)))
    assertSuccess(Try(ConstrainedSomeStringLongerThanFive(Some("123456"))))
    assertFailure(Try(ConstrainedSomeStringLongerThanFive(Some("12345"))),
      Error(property = "string", violatedConstraint = "LongerThan", message = "12345 length is not longer than 5"))
  }

  "Wrapped_Some_String_LongerThan" in {
    assertSuccess(Try(SomeStringWrapperLongerThanFive(WrappedSomeStringLongerThanFive(None))))
    assertSuccess(Try(SomeStringWrapperLongerThanFive(WrappedSomeStringLongerThanFive(Some("123456")))))
    assertFailure(Try(SomeStringWrapperLongerThanFive(WrappedSomeStringLongerThanFive(Some("12345")))),
      Error(property = "wrapped.string", violatedConstraint = "LongerThan", message = "12345 length is not longer than 5"))
  }

  //LongerThanOrEqualTo for String and Maybe String

  "String_LongerThanOrEqualTo" in {
    assertSuccess(Try(ConstrainedStringLongerThanOrEqualToFive("12345")))
    assertFailure(Try(ConstrainedStringLongerThanOrEqualToFive("1234")),
      Error(property = "string", violatedConstraint = "LongerThanOrEqualTo", message = "1234 length is not longer than or equal to 5"))
  }

  "Wrapped_String_LongerThanOrEqualTo" in {
    assertSuccess(Try(StringWrapperLongerThanOrEqualToFive(WrappedStringLongerThanOrEqualToFive("12345"))))
    assertFailure(Try(StringWrapperLongerThanOrEqualToFive(WrappedStringLongerThanOrEqualToFive("1234"))),
      Error(property = "wrapped.string", violatedConstraint = "LongerThanOrEqualTo", message = "1234 length is not longer than or equal to 5"))
  }

  "Some_String_LongerThanOrEqualTo" in {
    assertSuccess(Try(ConstrainedSomeStringLongerThanOrEqualToFive(None)))
    assertSuccess(Try(ConstrainedSomeStringLongerThanOrEqualToFive(Some("12345"))))
    assertFailure(Try(ConstrainedSomeStringLongerThanOrEqualToFive(Some("1234"))),
      Error(property = "string", violatedConstraint = "LongerThanOrEqualTo", message = "1234 length is not longer than or equal to 5"))
  }

  "Wrapped_Some_String_LongerThanOrEqualTo" in {
    assertSuccess(Try(SomeStringWrapperLongerThanOrEqualToFive(WrappedSomeStringLongerThanOrEqualToFive(None))))
    assertSuccess(Try(SomeStringWrapperLongerThanOrEqualToFive(WrappedSomeStringLongerThanOrEqualToFive(Some("12345")))))
    assertFailure(Try(SomeStringWrapperLongerThanOrEqualToFive(WrappedSomeStringLongerThanOrEqualToFive(Some("1234")))),
      Error(property = "wrapped.string", violatedConstraint = "LongerThanOrEqualTo", message = "1234 length is not longer than or equal to 5"))
  }

  //ShorterThan for String and Maybe String

  "String_ShorterThan" in {
    assertSuccess(Try(ConstrainedStringShorterThanFive("1234")))
    assertFailure(Try(ConstrainedStringShorterThanFive("12345")),
      Error(property = "string", violatedConstraint = "ShorterThan", message = "12345 length is not shorter than 5"))
  }

  "Wrapped_String_ShorterThan" in {
    assertSuccess(Try(StringWrapperShorterThanFive(WrappedStringShorterThanFive("1234"))))
    assertFailure(Try(StringWrapperShorterThanFive(WrappedStringShorterThanFive("12345"))),
      Error(property = "wrapped.string", violatedConstraint = "ShorterThan", message = "12345 length is not shorter than 5"))
  }

  "Some_String_ShorterThan" in {
    assertSuccess(Try(ConstrainedSomeStringShorterThanFive(None)))
    assertSuccess(Try(ConstrainedSomeStringShorterThanFive(Some("1234"))))
    assertFailure(Try(ConstrainedSomeStringShorterThanFive(Some("12345"))),
      Error(property = "string", violatedConstraint = "ShorterThan", message = "12345 length is not shorter than 5"))
  }

  "Wrapped_Some_String_ShorterThan" in {
    assertSuccess(Try(SomeStringWrapperShorterThanFive(WrappedSomeStringShorterThanFive(None))))
    assertSuccess(Try(SomeStringWrapperShorterThanFive(WrappedSomeStringShorterThanFive(Some("1234")))))
    assertFailure(Try(SomeStringWrapperShorterThanFive(WrappedSomeStringShorterThanFive(Some("12345")))),
      Error(property = "wrapped.string", violatedConstraint = "ShorterThan", message = "12345 length is not shorter than 5"))
  }

  //ShorterThanOrEqualTo for String and Maybe String

  "String_ShorterThanOrEqualTo" in {
    assertSuccess(Try(ConstrainedStringShorterThanOrEqualToFive("12345")))
    assertFailure(Try(ConstrainedStringShorterThanOrEqualToFive("123456")),
      Error(property = "string", violatedConstraint = "ShorterThanOrEqualTo", message = "123456 length is not shorter than or equal to 5"))
  }

  "Wrapped_String_ShorterThanOrEqualTo" in {
    assertSuccess(Try(StringWrapperShorterThanOrEqualToFive(WrappedStringShorterThanOrEqualToFive("12345"))))
    assertFailure(Try(StringWrapperShorterThanOrEqualToFive(WrappedStringShorterThanOrEqualToFive("123456"))),
      Error(property = "wrapped.string", violatedConstraint = "ShorterThanOrEqualTo", message = "123456 length is not shorter than or equal to 5"))
  }

  "Some_String_ShorterThanOrEqualTo" in {
    assertSuccess(Try(ConstrainedSomeStringShorterThanOrEqualToFive(None)))
    assertSuccess(Try(ConstrainedSomeStringShorterThanOrEqualToFive(Some("12345"))))
    assertFailure(Try(ConstrainedSomeStringShorterThanOrEqualToFive(Some("123456"))),
      Error(property = "string", violatedConstraint = "ShorterThanOrEqualTo", message = "123456 length is not shorter than or equal to 5"))
  }

  "Wrapped_Some_String_ShorterThanOrEqualTo" in {
    assertSuccess(Try(SomeStringWrapperShorterThanOrEqualToFive(WrappedSomeStringShorterThanOrEqualToFive(None))))
    assertSuccess(Try(SomeStringWrapperShorterThanOrEqualToFive(WrappedSomeStringShorterThanOrEqualToFive(Some("12345")))))
    assertFailure(Try(SomeStringWrapperShorterThanOrEqualToFive(WrappedSomeStringShorterThanOrEqualToFive(Some("123456")))),
      Error(property = "wrapped.string", violatedConstraint = "ShorterThanOrEqualTo", message = "123456 length is not shorter than or equal to 5"))
  }

  //NotBlank for String and Maybe String

  "String_NotBlank" in {
    assertSuccess(Try(ConstrainedStringNotBlank("not blank")))
    assertFailure(Try(ConstrainedStringNotBlank("")),
      Error(property = "string", violatedConstraint = "NotBlank", message = "string must not be blank"))
  }

  "Wrapped_String_NotBlank" in {
    assertSuccess(Try(StringWrapperNotBlank(WrappedStringNotBlank("not blank"))))
    assertFailure(Try(StringWrapperNotBlank(WrappedStringNotBlank("     "))),
      Error(property = "wrapped.string", violatedConstraint = "NotBlank", message = "wrapped.string must not be blank"))
  }

  "Some_String_NotBlank" in {
    assertSuccess(Try(ConstrainedSomeStringNotBlank(None)))
    assertSuccess(Try(ConstrainedSomeStringNotBlank(Some("not blank"))))
    assertFailure(Try(ConstrainedSomeStringNotBlank(Some(""))),
      Error(property = "string", violatedConstraint = "NotBlank", message = "string must not be blank"))
  }

  "Wrapped_Some_String_NotBlank" in {
    assertSuccess(Try(SomeStringWrapperNotBlank(WrappedSomeStringNotBlank(None))))
    assertSuccess(Try(SomeStringWrapperNotBlank(WrappedSomeStringNotBlank(Some("not blank")))))
    assertFailure(Try(SomeStringWrapperNotBlank(WrappedSomeStringNotBlank(Some("    ")))),
      Error(property = "wrapped.string", violatedConstraint = "NotBlank", message = "wrapped.string must not be blank"))
  }

  "ConstrainedString and ConstrainedNumbers must report the first violated constraints for both anotees" when {
    "String_EndsWithStartsWith_And_Wrapped_Number_GreaterThan" in {
      assertFailure(Try(ConstrainedStringStartsWithSebasAndConstrainedNumberIsGreaterThan10("fulanito", 5)),
        Error(property = "string", violatedConstraint = "StartsWith", message = "fulanito does not start with prefix Sebas"),
        Error(property = "number", violatedConstraint = "GreaterThan", message = "5 is not greater than 10"))
    }

    "Wrapped_String_EndsWithStartsWith_And_Wrapped_Number_GreaterThan" in {
      assertFailure(Try(StringWrapperStartsWithSebasAndConstrainedNumberIsGreaterThan10(
        WrappedStringStartsWithSebasAndConstrainedSomeNumberIsGreaterThan10("fulanito", Some(5)))),
          Error(property = "wrapped.string", violatedConstraint = "StartsWith", message = "fulanito does not start with prefix Sebas"),
          Error(property = "wrapped.number", violatedConstraint = "GreaterThan", message = "5 is not greater than 10"))
    }

    "Some_String_EndsWithStartsWith_And_Number_GreaterThan" in {
      assertSuccess(Try(ConstrainedSomeStringStartsWithSebasAndConstrainedNumberIsGreaterThan10(None, 11)))
      assertFailure(Try(ConstrainedSomeStringStartsWithSebasAndConstrainedNumberIsGreaterThan10(Some("fulanito"), 5)),
        Error(property = "string", violatedConstraint = "StartsWith", message = "fulanito does not start with prefix Sebas"),
        Error(property = "number", violatedConstraint = "GreaterThan", message = "5 is not greater than 10"))
    }

    "Wrapped_Some_String_StartsWith_And_Wrapped_Number_GreaterThan" in {
      assertSuccess(Try(SomeStringWrapperStartsWithSebasAndConstrainedNumberIsGreaterThan10(
        WrappedSomeStringStartsWithSebasAndConstrainedNumberIsGreaterThan10(None, 11))))
      assertFailure(Try(SomeStringWrapperStartsWithSebasAndConstrainedNumberIsGreaterThan10(
        WrappedSomeStringStartsWithSebasAndConstrainedNumberIsGreaterThan10(Some("fulanito"), 5))),
          Error(property = "wrapped.string", violatedConstraint = "StartsWith", message = "fulanito does not start with prefix Sebas"),
          Error(property = "wrapped.number", violatedConstraint = "GreaterThan", message = "5 is not greater than 10"))
    }
  }

  "ConstrainedString and Unannotatteed must report the String violated constraint" when {
    "String_EndsWithStartsWith_And_Unannotatteed" in {
      assertFailure(Try(ConstrainedStringStartsWithSebasAndUnannotatteed("fulanito", 5)),
        Error(property = "string", violatedConstraint = "StartsWith", message = "fulanito does not start with prefix Sebas"))
    }

    "Wrapped_String_EndsWithStartsWith_And_Unannotatteed" in {
      assertFailure(Try(StringWrapperStartsWithSebasAndUnannotatteed(
        WrappedStringStartsWithSebasAndUnannotatteed("fulanito", Some(5)))),
        Error(property = "wrapped.string", violatedConstraint = "StartsWith", message = "fulanito does not start with prefix Sebas"))
    }

    "Some_String_EndsWithStartsWith_And_Unannotatteed" in {
      assertSuccess(Try(ConstrainedSomeStringStartsWithSebasAndUnannotatteed(None, 11)))
      assertFailure(Try(ConstrainedSomeStringStartsWithSebasAndUnannotatteed(Some("fulanito"), 5)),
        Error(property = "string", violatedConstraint = "StartsWith", message = "fulanito does not start with prefix Sebas"))
    }

    "Wrapped_Some_String_StartsWith_And_Unannotatteed" in {
      assertSuccess(Try(SomeStringWrapperStartsWithSebasAndUnannotatteed(
        WrappedSomeStringStartsWithSebasAndUnannotatteed(None, 11))))
      assertFailure(Try(SomeStringWrapperStartsWithSebasAndUnannotatteed(
        WrappedSomeStringStartsWithSebasAndUnannotatteed(Some("fulanito"), 5))),
        Error(property = "wrapped.string", violatedConstraint = "StartsWith", message = "fulanito does not start with prefix Sebas"))
    }
  }

  //NonEmpty for Iterable

  "StringIterable_NonEmpty" in {
    assertSuccess(Try(ConstrainedStringIterableNonEmpty(Seq("string1","string2"))))
    assertFailure(Try(ConstrainedStringIterableNonEmpty(Nil)),
      Error(property = "iterable", violatedConstraint = "NonEmpty", message = "iterable must not be empty"))
  }

  "Wrapped_StringIterable_NonEmpty" in {
    assertSuccess(Try(StringIterableWrapperNonEmpty(WrappedStringIterableNonEmpty(Seq("string1", "string2")))))
    assertFailure(Try(StringIterableWrapperNonEmpty(WrappedStringIterableNonEmpty(Nil))),
      Error(property = "wrapped.iterable", violatedConstraint = "NonEmpty", message = "wrapped.iterable must not be empty"))
  }

  "StringIterable_NonEmpty2" in {
    assertSuccess(Try(ConstrainedStringIterableNonEmpty2(Seq("string1","string2"))))
    assertFailure(Try(ConstrainedStringIterableNonEmpty2(Nil)),
      Error(property = "iterable", violatedConstraint = "NonEmpty", message = "iterable must not be empty"))
  }

  "Wrapped_StringIterable_NonEmpty2" in {
    assertSuccess(Try(StringIterableWrapperNonEmpty2(WrappedStringIterableNonEmpty2(Seq("string1", "string2")))))
    assertFailure(Try(StringIterableWrapperNonEmpty2(WrappedStringIterableNonEmpty2(Nil))),
      Error(property = "wrapped.iterable", violatedConstraint = "NonEmpty", message = "wrapped.iterable must not be empty"))
  }

  //AtMost for Iterable

  "StringIterable_AtMost" in {
    assertSuccess(Try(ConstrainedIterableAtMost(Nil)))
    assertSuccess(Try(ConstrainedIterableAtMost(Seq("string1"))))
    assertFailure(Try(ConstrainedIterableAtMost(Seq("string1", "string2"))),
      Error(property = "iterable", violatedConstraint = "AtMost", message = "iterable must contain at most 1 elements"))
  }

  "Wrapped_StringIterable_AtMost" in {
    assertSuccess(Try(IterableWrapperAtMost(WrappedIterableAtMost(Nil))))
    assertSuccess(Try(IterableWrapperAtMost(WrappedIterableAtMost(Seq("string1")))))
    assertFailure(Try(IterableWrapperAtMost(WrappedIterableAtMost(Seq("string1","string2")))),
      Error(property = "wrapped.iterable", violatedConstraint = "AtMost", message = "wrapped.iterable must contain at most 1 elements"))
  }

  "StringIterable_AtMost2" in {
    assertSuccess(Try(ConstrainedIterableAtMost2(Nil)))
    assertSuccess(Try(ConstrainedIterableAtMost2(Seq("string1"))))
    assertFailure(Try(ConstrainedIterableAtMost2(Seq("string1", "string2"))),
      Error(property = "iterable", violatedConstraint = "AtMost", message = "iterable must contain at most 1 elements"))
  }

  "Wrapped_StringIterable_AtMost2" in {
    assertSuccess(Try(IterableWrapperAtMost2(WrappedIterableAtMost2(Nil))))
    assertSuccess(Try(IterableWrapperAtMost2(WrappedIterableAtMost2(Seq("string1")))))
    assertFailure(Try(IterableWrapperAtMost2(WrappedIterableAtMost2(Seq("string1","string2")))),
      Error(property = "wrapped.iterable", violatedConstraint = "AtMost", message = "wrapped.iterable must contain at most 1 elements"))
  }

  // Deep hierarchy

  "Deep hierarchy" in {
    assertSuccess(Try(
      Order(
        code = "code_123",
        buyer = "Sebastian",
        lines = List(
          OrderLine(
            sku = "abc",
            quantity = 1,
            details = ProductDetails(description = "cool")
          )
        )
      )
    ))

    assertFailure(Try(
      Order(
        code = "code_123",
        buyer = "Sebastian",
        lines = List(
          OrderLine(
            sku = "abc",
            quantity = 1,
            details = ProductDetails(description = "cool")
          ),
          OrderLine(
            sku = "def",
            quantity = 1,
            details = ProductDetails(description = "")
          )
        ))),
      expectedErrors =
        Error(property = "lines.1.details.description", violatedConstraint = "NotBlank", message = "lines.1.details.description must not be blank"))
  }





  "defdef" in {
    Try(Trainer(name = "cucho", age = 10).modify("  ", 100)) match {
      case Success(_) => println("PUTO")
      case Failure(_@AssertionFailureException(errors)) => errors.foreach(println)
    }
  }

}
