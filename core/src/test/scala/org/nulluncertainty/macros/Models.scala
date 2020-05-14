package org.nulluncertainty.macros

import org.nulluncertainty.macros.NumberConstraints.InclusiveRange

@EnabledAssertions
case class Discount(@ConstrainedNumber(InclusiveRange(0, 100)) percentage: Int)
