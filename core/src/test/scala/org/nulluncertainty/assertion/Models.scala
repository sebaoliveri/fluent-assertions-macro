package org.nulluncertainty.assertion

import org.nulluncertainty.assertion.IterableConstraints.{AtLeast, AtMost, NonEmpty}
import org.nulluncertainty.assertion.NumberConstraints.{EqualsTo, ExclusiveRange, GreaterThan, GreaterThanOrEqualTo, InclusiveRange, LessThan, LessThanOrEqualTo}
import org.nulluncertainty.assertion.StringConstraints.{Alphabetic, Alphanumeric, Email, EndsWith, LongerThan, NotBlank, StartsWith, Uri}

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//GreaterThan and LessThan for Numbers and Maybe Numbers

@EnabledAssertions
case class ConstrainedIntGreaterThan10AndLessThan5(@ConstrainedNumber(GreaterThan(10), LessThan(5)) number: Int)

@EnabledAssertions
case class IntWrapperGreaterThan10AndLessThan5(@ConstrainedType wrapped: WrappedIntGreaterThan10AndLessThan5)
case class WrappedIntGreaterThan10AndLessThan5(@ConstrainedNumber(GreaterThan(10), LessThan(5)) number: Int)

@EnabledAssertions
case class ConstrainedSomeIntGreaterThan10AndLessThan5(@ConstrainedNumber(GreaterThan(10), LessThan(5)) number: Option[Int])

@EnabledAssertions
case class SomeIntWrapperGreaterThan10AndLessThan5(@ConstrainedType wrapped: WrappedSomeIntGreaterThan10AndLessThan5)
case class WrappedSomeIntGreaterThan10AndLessThan5(@ConstrainedNumber(GreaterThan(10), LessThan(5)) number: Option[Int])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//EqualsTo for Numbers and Maybe Numbers

@EnabledAssertions
case class ConstrainedIntEqualsTo10(@ConstrainedNumber(EqualsTo(10)) number: Int)

@EnabledAssertions
case class IntWrapperEqualsTo10(@ConstrainedType wrapped: WrappedIntEqualsTo10)
case class WrappedIntEqualsTo10(@ConstrainedNumber(EqualsTo(10)) number: Int)

@EnabledAssertions
case class ConstrainedSomeIntEqualsTo10(@ConstrainedNumber(EqualsTo(10)) number: Option[Int])

@EnabledAssertions
case class SomeIntWrapperEqualsTo10(@ConstrainedType wrapped: WrappedSomeIntEqualsTo10)
case class WrappedSomeIntEqualsTo10(@ConstrainedNumber(EqualsTo(10)) number: Option[Int])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//GreaterThan for Numbers and Maybe Numbers

@EnabledAssertions
case class ConstrainedIntGreaterThan10(@ConstrainedNumber(GreaterThan(10)) number: Int)

@EnabledAssertions
case class IntWrapperGreaterThan10(@ConstrainedType wrapped: WrappedIntGreaterThan10)
case class WrappedIntGreaterThan10(@ConstrainedNumber(GreaterThan(10)) number: Int)

@EnabledAssertions
case class ConstrainedSomeIntGreaterThan10(@ConstrainedNumber(GreaterThan(10)) number: Option[Int])

@EnabledAssertions
case class SomeIntWrapperGreaterThan10(@ConstrainedType wrapped: WrappedSomeIntGreaterThan10)
case class WrappedSomeIntGreaterThan10(@ConstrainedNumber(GreaterThan(10)) number: Option[Int])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//GreaterThanOrEqualTo for Numbers and Maybe Numbers

@EnabledAssertions
case class ConstrainedIntGreaterThanOrEqualTo10(@ConstrainedNumber(GreaterThanOrEqualTo(10)) number: Int)

@EnabledAssertions
case class IntWrapperGreaterThanOrEqualTo10(@ConstrainedType wrapped: WrappedIntGreaterThanOrEqualTo10)
case class WrappedIntGreaterThanOrEqualTo10(@ConstrainedNumber(GreaterThanOrEqualTo(10)) number: Int)

@EnabledAssertions
case class ConstrainedSomeIntGreaterThanOrEqualTo10(@ConstrainedNumber(GreaterThanOrEqualTo(10)) number: Option[Int])

@EnabledAssertions
case class SomeIntWrapperGreaterThanOrEqualTo10(@ConstrainedType wrapped: WrappedSomeIntGreaterThanOrEqualTo10)
case class WrappedSomeIntGreaterThanOrEqualTo10(@ConstrainedNumber(GreaterThanOrEqualTo(10)) number: Option[Int])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//LessThan for Numbers and Maybe Numbers

@EnabledAssertions
case class ConstrainedIntLessThan10(@ConstrainedNumber(LessThan(10)) number: Int)

@EnabledAssertions
case class IntWrapperLessThan10(@ConstrainedType wrapped: WrappedIntLessThan10)
case class WrappedIntLessThan10(@ConstrainedNumber(LessThan(10)) number: Int)

@EnabledAssertions
case class ConstrainedSomeIntLessThan10(@ConstrainedNumber(LessThan(10)) number: Option[Int])

@EnabledAssertions
case class SomeIntWrapperLessThan10(@ConstrainedType wrapped: WrappedSomeIntLessThan10)
case class WrappedSomeIntLessThan10(@ConstrainedNumber(LessThan(10)) number: Option[Int])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//LessThanOrEqualTo for Numbers and Maybe Numbers

@EnabledAssertions
case class ConstrainedIntLessThanOrEqualTo10(@ConstrainedNumber(LessThanOrEqualTo(10)) number: Int)

@EnabledAssertions
case class IntWrapperLessThanOrEqualTo10(@ConstrainedType wrapped: WrappedIntLessThanOrEqualTo10)
case class WrappedIntLessThanOrEqualTo10(@ConstrainedNumber(LessThanOrEqualTo(10)) number: Int)

@EnabledAssertions
case class ConstrainedSomeIntLessThanOrEqualTo10(@ConstrainedNumber(LessThanOrEqualTo(10)) number: Option[Int])

@EnabledAssertions
case class SomeIntWrapperLessThanOrEqualTo10(@ConstrainedType wrapped: WrappedSomeIntLessThanOrEqualTo10)
case class WrappedSomeIntLessThanOrEqualTo10(@ConstrainedNumber(LessThanOrEqualTo(10)) number: Option[Int])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//InclusiveRange for Numbers and Maybe Numbers

@EnabledAssertions
case class ConstrainedIntInclusiveRangeFrom0To100(@ConstrainedNumber(InclusiveRange(0,100)) number: Int)

@EnabledAssertions
case class IntWrapperInclusiveRangeFrom0To100(@ConstrainedType wrapped: WrappedIntInclusiveRangeFrom0To100)
case class WrappedIntInclusiveRangeFrom0To100(@ConstrainedNumber(InclusiveRange(0,100)) number: Int)

@EnabledAssertions
case class ConstrainedSomeIntInclusiveRangeFrom0To100(@ConstrainedNumber(InclusiveRange(0,100)) number: Option[Int])

@EnabledAssertions
case class SomeIntWrapperInclusiveRangeFrom0To100(@ConstrainedType wrapped: WrappedSomeIntInclusiveRangeFrom0To100)
case class WrappedSomeIntInclusiveRangeFrom0To100(@ConstrainedNumber(InclusiveRange(0,100)) number: Option[Int])


//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//ExclusiveRange for Numbers and Maybe Numbers

@EnabledAssertions
case class ConstrainedIntExclusiveRangeFrom0To100(@ConstrainedNumber(ExclusiveRange(0,100)) number: Int)

@EnabledAssertions
case class IntWrapperExclusiveRangeFrom0To100(@ConstrainedType wrapped: WrappedIntExclusiveRangeFrom0To100)
case class WrappedIntExclusiveRangeFrom0To100(@ConstrainedNumber(ExclusiveRange(0,100)) number: Int)

@EnabledAssertions
case class ConstrainedSomeIntExclusiveRangeFrom0To100(@ConstrainedNumber(ExclusiveRange(0,100)) number: Option[Int])

@EnabledAssertions
case class SomeIntWrapperExclusiveRangeFrom0To100(@ConstrainedType wrapped: WrappedSomeIntExclusiveRangeFrom0To100)
case class WrappedSomeIntExclusiveRangeFrom0To100(@ConstrainedNumber(ExclusiveRange(0,100)) number: Option[Int])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//EqualsTo for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringEqualsToSebastian(@ConstrainedString(StringConstraints.EqualsTo("Sebastian")) string: String)

@EnabledAssertions
case class StringWrapperEqualsToSebastian(@ConstrainedType wrapped: WrappedStringEqualsToSebastian)
case class WrappedStringEqualsToSebastian(@ConstrainedString(StringConstraints.EqualsTo("Sebastian")) string: String)

@EnabledAssertions
case class ConstrainedSomeStringEqualsToSebastian(@ConstrainedString(StringConstraints.EqualsTo("Sebastian")) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperEqualsToSebastian(@ConstrainedType wrapped: WrappedSomeStringEqualsToSebastian)
case class WrappedSomeStringEqualsToSebastian(@ConstrainedString(StringConstraints.EqualsTo("Sebastian")) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//EqualsToIgnoringCase for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringEqualsToIgnoringCaseSebastian(@ConstrainedString(StringConstraints.EqualsToIgnoringCase("Sebastian")) string: String)

@EnabledAssertions
case class StringWrapperEqualsToIgnoringCaseSebastian(@ConstrainedType wrapped: WrappedStringEqualsToIgnoringCaseSebastian)
case class WrappedStringEqualsToIgnoringCaseSebastian(@ConstrainedString(StringConstraints.EqualsToIgnoringCase("Sebastian")) string: String)

@EnabledAssertions
case class ConstrainedSomeStringEqualsToIgnoringCaseSebastian(@ConstrainedString(StringConstraints.EqualsToIgnoringCase("Sebastian")) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperEqualsToIgnoringCaseSebastian(@ConstrainedType wrapped: WrappedSomeStringEqualsToIgnoringCaseSebastian)
case class WrappedSomeStringEqualsToIgnoringCaseSebastian(@ConstrainedString(StringConstraints.EqualsToIgnoringCase("Sebastian")) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//EndsWithStartsWith for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringEndsWithStartsWithCucho(@ConstrainedString(EndsWith("cucho"), StringConstraints.StartsWith("Sebas")) string: String)

@EnabledAssertions
case class StringWrapperEndsWithStartsWithCucho(@ConstrainedType wrapped: WrappedStringEndsWithStartsWithCucho)
case class WrappedStringEndsWithStartsWithCucho(@ConstrainedString(EndsWith("cucho"), StringConstraints.StartsWith("Sebas")) string: String)

@EnabledAssertions
case class ConstrainedSomeStringEndsWithStartsWithCucho(@ConstrainedString(EndsWith("cucho"), StringConstraints.StartsWith("Sebas")) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperEndsWithStartsWithCucho(@ConstrainedType wrapped: WrappedSomeStringEndsWithStartsWithCucho)
case class WrappedSomeStringEndsWithStartsWithCucho(@ConstrainedString(EndsWith("cucho"), StringConstraints.StartsWith("Sebas")) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//StartsWith for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringStartsWithSebas(@ConstrainedString(StringConstraints.StartsWith("Sebas")) string: String)

@EnabledAssertions
case class StringWrapperStartsWithSebas(@ConstrainedType wrapped: WrappedStringStartsWithSebas)
case class WrappedStringStartsWithSebas(@ConstrainedString(StringConstraints.StartsWith("Sebas")) string: String)

@EnabledAssertions
case class ConstrainedSomeStringStartsWithSebas(@ConstrainedString(StringConstraints.StartsWith("Sebas")) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperStartsWithSebas(@ConstrainedType wrapped: WrappedSomeStringStartsWithSebas)
case class WrappedSomeStringStartsWithSebas(@ConstrainedString(StringConstraints.StartsWith("Sebas")) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//StartsWithIgnoringCase for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringStartsWithIgnoringCaseSebas(@ConstrainedString(StringConstraints.StartsWithIgnoringCase("Sebas")) string: String)

@EnabledAssertions
case class StringWrapperStartsWithIgnoringCaseSebas(@ConstrainedType wrapped: WrappedStringStartsWithIgnoringCaseSebas)
case class WrappedStringStartsWithIgnoringCaseSebas(@ConstrainedString(StringConstraints.StartsWithIgnoringCase("Sebas")) string: String)

@EnabledAssertions
case class ConstrainedSomeStringStartsWithIgnoringCaseSebas(@ConstrainedString(StringConstraints.StartsWithIgnoringCase("Sebas")) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperStartsWithIgnoringCaseSebas(@ConstrainedType wrapped: WrappedSomeStringStartsWithIgnoringCaseSebas)
case class WrappedSomeStringStartsWithIgnoringCaseSebas(@ConstrainedString(StringConstraints.StartsWithIgnoringCase("Sebas")) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//Contains for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringContainsTian(@ConstrainedString(StringConstraints.Contains("tian")) string: String)

@EnabledAssertions
case class StringWrapperContainsTian(@ConstrainedType wrapped: WrappedStringContainsTian)
case class WrappedStringContainsTian(@ConstrainedString(StringConstraints.Contains("tian")) string: String)

@EnabledAssertions
case class ConstrainedSomeStringContainsTian(@ConstrainedString(StringConstraints.Contains("tian")) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperContainsTian(@ConstrainedType wrapped: WrappedSomeStringContainsTian)
case class WrappedSomeStringContainsTian(@ConstrainedString(StringConstraints.Contains("tian")) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//ContainsIgnoringCase for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringContainsIgnoringCaseTian(@ConstrainedString(StringConstraints.ContainsIgnoringCase("tian")) string: String)

@EnabledAssertions
case class StringWrapperContainsIgnoringCaseTian(@ConstrainedType wrapped: WrappedStringContainsIgnoringCaseTian)
case class WrappedStringContainsIgnoringCaseTian(@ConstrainedString(StringConstraints.ContainsIgnoringCase("tian")) string: String)

@EnabledAssertions
case class ConstrainedSomeStringContainsIgnoringCaseTian(@ConstrainedString(StringConstraints.ContainsIgnoringCase("tian")) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperContainsIgnoringCaseTian(@ConstrainedType wrapped: WrappedSomeStringContainsIgnoringCaseTian)
case class WrappedSomeStringContainsIgnoringCaseTian(@ConstrainedString(StringConstraints.ContainsIgnoringCase("tian")) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//Matches for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringMatchesNumber(@ConstrainedString(StringConstraints.Matches("^[0-9]+$")) string: String)

@EnabledAssertions
case class StringWrapperMatchesNumber(@ConstrainedType wrapped: WrappedStringMatchesNumber)
case class WrappedStringMatchesNumber(@ConstrainedString(StringConstraints.Matches("^[0-9]+$")) string: String)

@EnabledAssertions
case class ConstrainedSomeStringMatchesNumber(@ConstrainedString(StringConstraints.Matches("^[0-9]+$")) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperMatchesNumber(@ConstrainedType wrapped: WrappedSomeStringMatchesNumber)
case class WrappedSomeStringMatchesNumber(@ConstrainedString(StringConstraints.Matches("^[0-9]+$")) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//Email for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringEmail(@ConstrainedString(Email) string: String)

@EnabledAssertions
case class StringWrapperEmail(@ConstrainedType wrapped: WrappedStringEmail)
case class WrappedStringEmail(@ConstrainedString(Email) string: String)

@EnabledAssertions
case class ConstrainedSomeStringEmail(@ConstrainedString(Email) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperEmail(@ConstrainedType wrapped: WrappedSomeStringEmail)
case class WrappedSomeStringEmail(@ConstrainedString(Email) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//Uri for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringUri(@ConstrainedString(Uri) string: String)

@EnabledAssertions
case class StringWrapperUri(@ConstrainedType wrapped: WrappedStringUri)
case class WrappedStringUri(@ConstrainedString(Uri) string: String)

@EnabledAssertions
case class ConstrainedSomeStringUri(@ConstrainedString(Uri) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperUri(@ConstrainedType wrapped: WrappedSomeStringUri)
case class WrappedSomeStringUri(@ConstrainedString(Uri) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//Alphanumeric for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringAlphanumeric(@ConstrainedString(Alphanumeric) string: String)

@EnabledAssertions
case class StringWrapperAlphanumeric(@ConstrainedType wrapped: WrappedStringAlphanumeric)
case class WrappedStringAlphanumeric(@ConstrainedString(Alphanumeric) string: String)

@EnabledAssertions
case class ConstrainedSomeStringAlphanumeric(@ConstrainedString(Alphanumeric) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperAlphanumeric(@ConstrainedType wrapped: WrappedSomeStringAlphanumeric)
case class WrappedSomeStringAlphanumeric(@ConstrainedString(Alphanumeric) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//Alphabetic for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringAlphabetic(@ConstrainedString(Alphabetic) string: String)

@EnabledAssertions
case class StringWrapperAlphabetic(@ConstrainedType wrapped: WrappedStringAlphabetic)
case class WrappedStringAlphabetic(@ConstrainedString(Alphabetic) string: String)

@EnabledAssertions
case class ConstrainedSomeStringAlphabetic(@ConstrainedString(Alphabetic) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperAlphabetic(@ConstrainedType wrapped: WrappedSomeStringAlphabetic)
case class WrappedSomeStringAlphabetic(@ConstrainedString(Alphabetic) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//Number for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringNumber(@ConstrainedString(StringConstraints.Number) string: String)

@EnabledAssertions
case class StringWrapperNumber(@ConstrainedType wrapped: WrappedStringNumber)
case class WrappedStringNumber(@ConstrainedString(StringConstraints.Number) string: String)

@EnabledAssertions
case class ConstrainedSomeStringNumber(@ConstrainedString(StringConstraints.Number) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperNumber(@ConstrainedType wrapped: WrappedSomeStringNumber)
case class WrappedSomeStringNumber(@ConstrainedString(StringConstraints.Number) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//LongerThan for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringLongerThanFive(@ConstrainedString(StringConstraints.LongerThan(5)) string: String)

@EnabledAssertions
case class StringWrapperLongerThanFive(@ConstrainedType wrapped: WrappedStringLongerThanFive)
case class WrappedStringLongerThanFive(@ConstrainedString(StringConstraints.LongerThan(5)) string: String)

@EnabledAssertions
case class ConstrainedSomeStringLongerThanFive(@ConstrainedString(StringConstraints.LongerThan(5)) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperLongerThanFive(@ConstrainedType wrapped: WrappedSomeStringLongerThanFive)
case class WrappedSomeStringLongerThanFive(@ConstrainedString(StringConstraints.LongerThan(5)) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//LongerThanOrEqualTo for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringLongerThanOrEqualToFive(@ConstrainedString(StringConstraints.LongerThanOrEqualTo(5)) string: String)

@EnabledAssertions
case class StringWrapperLongerThanOrEqualToFive(@ConstrainedType wrapped: WrappedStringLongerThanOrEqualToFive)
case class WrappedStringLongerThanOrEqualToFive(@ConstrainedString(StringConstraints.LongerThanOrEqualTo(5)) string: String)

@EnabledAssertions
case class ConstrainedSomeStringLongerThanOrEqualToFive(@ConstrainedString(StringConstraints.LongerThanOrEqualTo(5)) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperLongerThanOrEqualToFive(@ConstrainedType wrapped: WrappedSomeStringLongerThanOrEqualToFive)
case class WrappedSomeStringLongerThanOrEqualToFive(@ConstrainedString(StringConstraints.LongerThanOrEqualTo(5)) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//ShorterThan for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringShorterThanFive(@ConstrainedString(StringConstraints.ShorterThan(5)) string: String)

@EnabledAssertions
case class StringWrapperShorterThanFive(@ConstrainedType wrapped: WrappedStringShorterThanFive)
case class WrappedStringShorterThanFive(@ConstrainedString(StringConstraints.ShorterThan(5)) string: String)

@EnabledAssertions
case class ConstrainedSomeStringShorterThanFive(@ConstrainedString(StringConstraints.ShorterThan(5)) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperShorterThanFive(@ConstrainedType wrapped: WrappedSomeStringShorterThanFive)
case class WrappedSomeStringShorterThanFive(@ConstrainedString(StringConstraints.ShorterThan(5)) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//ShorterThanOrEqualTo for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringShorterThanOrEqualToFive(@ConstrainedString(StringConstraints.ShorterThanOrEqualTo(5)) string: String)

@EnabledAssertions
case class StringWrapperShorterThanOrEqualToFive(@ConstrainedType wrapped: WrappedStringShorterThanOrEqualToFive)
case class WrappedStringShorterThanOrEqualToFive(@ConstrainedString(StringConstraints.ShorterThanOrEqualTo(5)) string: String)

@EnabledAssertions
case class ConstrainedSomeStringShorterThanOrEqualToFive(@ConstrainedString(StringConstraints.ShorterThanOrEqualTo(5)) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperShorterThanOrEqualToFive(@ConstrainedType wrapped: WrappedSomeStringShorterThanOrEqualToFive)
case class WrappedSomeStringShorterThanOrEqualToFive(@ConstrainedString(StringConstraints.ShorterThanOrEqualTo(5)) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//NotBlank for Strings and Maybe Strings

@EnabledAssertions
case class ConstrainedStringNotBlank(@ConstrainedString(StringConstraints.NotBlank) string: String)

@EnabledAssertions
case class StringWrapperNotBlank(@ConstrainedType wrapped: WrappedStringNotBlank)
case class WrappedStringNotBlank(@ConstrainedString(StringConstraints.NotBlank) string: String)

@EnabledAssertions
case class ConstrainedSomeStringNotBlank(@ConstrainedString(StringConstraints.NotBlank) string: Option[String])

@EnabledAssertions
case class SomeStringWrapperNotBlank(@ConstrainedType wrapped: WrappedSomeStringNotBlank)
case class WrappedSomeStringNotBlank(@ConstrainedString(StringConstraints.NotBlank) string: Option[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//StartsWith for Strings and IsGreaterThan for Numbers

@EnabledAssertions
case class ConstrainedStringStartsWithSebasAndConstrainedNumberIsGreaterThan10(@ConstrainedString(StartsWith("Sebas")) string: String,
                                                                               @ConstrainedNumber(GreaterThan(10)) number: Int)

@EnabledAssertions
case class StringWrapperStartsWithSebasAndConstrainedNumberIsGreaterThan10(@ConstrainedType wrapped: WrappedStringStartsWithSebasAndConstrainedSomeNumberIsGreaterThan10)
case class WrappedStringStartsWithSebasAndConstrainedSomeNumberIsGreaterThan10(@ConstrainedString(StartsWith("Sebas")) string: String,
                                                                               @ConstrainedNumber(GreaterThan(10)) number: Option[Int])

@EnabledAssertions
case class ConstrainedSomeStringStartsWithSebasAndConstrainedNumberIsGreaterThan10(@ConstrainedString(StartsWith("Sebas")) string: Option[String],
                                                                                   @ConstrainedNumber(GreaterThan(10)) number: Int)

@EnabledAssertions
case class SomeStringWrapperStartsWithSebasAndConstrainedNumberIsGreaterThan10(@ConstrainedType wrapped: WrappedSomeStringStartsWithSebasAndConstrainedNumberIsGreaterThan10)
case class WrappedSomeStringStartsWithSebasAndConstrainedNumberIsGreaterThan10(@ConstrainedString(StartsWith("Sebas")) string: Option[String],
                                                                               @ConstrainedNumber(GreaterThan(10)) number: Int)

// Unanotatted models tests at compile time

@EnabledAssertions
case class UnannotatteedNumber(number: Int)

@EnabledAssertions
case class UnannotatteedString(text: String)

@EnabledAssertions
case class UnannotatteedWrapperOfUnannotatteedWrapped(wrapped: UnannotatteedWrapped)
case class UnannotatteedWrapped(number: Int, text: String)

@EnabledAssertions
case class UnannotatteedWrapperOfAnnotatteedWrapped(@ConstrainedType wrapped: AnnotatteedWrapped)
case class AnnotatteedWrapped(number: Int, text: String)

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//StartsWith for Strings and Unannottateed

@EnabledAssertions
case class ConstrainedStringStartsWithSebasAndUnannotatteed(@ConstrainedString(StartsWith("Sebas")) string: String,
                                                            number: Int)

@EnabledAssertions
case class StringWrapperStartsWithSebasAndUnannotatteed(@ConstrainedType wrapped: WrappedStringStartsWithSebasAndUnannotatteed)
case class WrappedStringStartsWithSebasAndUnannotatteed(@ConstrainedString(StartsWith("Sebas")) string: String,
                                                        number: Option[Int])

@EnabledAssertions
case class ConstrainedSomeStringStartsWithSebasAndUnannotatteed(@ConstrainedString(StartsWith("Sebas")) string: Option[String],
                                                                number: Int)

@EnabledAssertions
case class SomeStringWrapperStartsWithSebasAndUnannotatteed(@ConstrainedType wrapped: WrappedSomeStringStartsWithSebasAndUnannotatteed)
case class WrappedSomeStringStartsWithSebasAndUnannotatteed(@ConstrainedString(StartsWith("Sebas")) string: Option[String],
                                                            number: Int)

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//NonEmpty for Iterables

@EnabledAssertions
case class ConstrainedStringIterableNonEmpty(@ConstrainedIterable(NonEmpty) iterable: Seq[String])

@EnabledAssertions
case class StringIterableWrapperNonEmpty(@ConstrainedType wrapped: WrappedStringIterableNonEmpty)
case class WrappedStringIterableNonEmpty(@ConstrainedIterable(NonEmpty) iterable: Seq[String])

@EnabledAssertions
case class ConstrainedStringIterableNonEmpty2(@ConstrainedIterable(IterableConstraints.NonEmpty) iterable: Seq[String])

@EnabledAssertions
case class StringIterableWrapperNonEmpty2(@ConstrainedType wrapped: WrappedStringIterableNonEmpty2)
case class WrappedStringIterableNonEmpty2(@ConstrainedIterable(IterableConstraints.NonEmpty) iterable: Seq[String])

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
//AtMost for Iterables

@EnabledAssertions
case class ConstrainedIterableAtMost(@ConstrainedIterable(AtMost(1)) iterable: Seq[String])

@EnabledAssertions
case class IterableWrapperAtMost(@ConstrainedType wrapped: WrappedIterableAtMost)
case class WrappedIterableAtMost(@ConstrainedIterable(AtMost(1)) iterable: Seq[String])

@EnabledAssertions
case class ConstrainedIterableAtMost2(@ConstrainedIterable(IterableConstraints.AtMost(1)) iterable: Seq[String])

@EnabledAssertions
case class IterableWrapperAtMost2(@ConstrainedType wrapped: WrappedIterableAtMost2)
case class WrappedIterableAtMost2(@ConstrainedIterable(IterableConstraints.AtMost(1)) iterable: Seq[String])

@EnabledAssertions
case class Order(@ConstrainedString(StartsWith("code")) code: String,
                 @ConstrainedString(LongerThan(3)) buyer: Option[String],
                 @ConstrainedType details: OrderDetails,
                 @ConstrainedIterable(NonEmpty) lines: List[OrderLine]) {
  def change(@ConstrainedString(NotBlank) buyer: Option[String],
             @ConstrainedString(StartsWith("code")) code: String): Order =
    copy(code = code, buyer = buyer)

  def changeDetails(@ConstrainedType newDetails: OrderDetails): Order =
    copy(details = newDetails)

  def changeLines(@ConstrainedIterable(NonEmpty) newLines: List[OrderLine]): Order =
    copy(lines = newLines)
}
case class OrderDetails(@ConstrainedString(NotBlank) firstDetail: String, @ConstrainedString(NotBlank) secondDetail: Option[String])
case class OrderLine(@ConstrainedString(NotBlank) sku: String,
                     @ConstrainedNumber(GreaterThanOrEqualTo(1)) quantity: Int,
                     @ConstrainedType details: ProductDetails)
case class ProductDetails(@ConstrainedString(NotBlank) description: String)
