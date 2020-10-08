# Fluent assertions Macro

When doing OOP, objects must be valid since momento zero and invariants must be preserved to remain objects consistent.

This lib uses meta-programming to refine types, by simple annotating your model, to make it consistent with regards to business rules.

### Installation

This lib supports Scala 2.13
Add in your build.sbt the following lines:
```
resolvers += Resolver.bintrayRepo("fluent-assertions", "releases")
libraryDependencies += "nulluncertainty" %% "fluent-assertions" % "2.0.1"
libraryDependencies += "nulluncertainty" %% "fluent-assertions-macro" % "1.0"
```

### Usage

Given the following sample (not production) code:

```scala

case class PurchaseOrder(buyer: String, lines: List[OrderLine])

case class PurchaseOrderLine(quantity: Int, sku: String)

```

The model shouldn´t allow a developer to do stuff like:

```scala

PurchaseOrder(buyer = " ", List(OrderLine(1, "pants_sku")))

PurchaseOrder(buyer = "Arnold", Nil)

PurchaseOrder(buyer = "Arnold", List(OrderLine(-1, "pants_sku")))

PurchaseOrder(buyer = "Arnold", List(OrderLine(1, " ")))

```

So one option would be to preserve invariants by running the following conditional assertions:

```scala

case class PurchaseOrder(buyer: String, lines: List[OrderLine]) {
  if (buyer.isBlank) throw new RuntimeException("Buyer name can not be blank")
  if (lines.isEmpty) throw new RuntimeException("At least one line must be specified")
}

case class PurchaseOrderLine(quantity: Int, sku: String) {
    if (quantity <= 0) throw new RuntimeException("The product quantity must be positive")
    if (sku.isBlank) throw new RuntimeException("The sku can not be blank")
}

```

This solution is OK to guarantee consistency within a cluster of cohesive objects but: 

It might become kind of verbose.
It also could be hard to reason about when requires lots of conditional assertions. Might become a sea of IF-ELSE statements losing legibility and making it developer dependent to maintain.
And it fails fast, meaning that it fails when the first encountered assertion fails. Won't allow the client of this code to report all error messages for the failing assertions.

A second solution to preserve invariants (and solving the issues I mentioned above) would be to use [fluent assertions](https://github.com/sebaoliveri/fluent-assertions) and lighten the work of the developer when describing and running assertions.

_Now, the third solution is the one that provides this lib and the idea is to use meta-programming to enhance a domain model with fluent-assertions._

```scala

@EnabledAssertions
case class PurchaseOrder(@ConstrainedString(NotBlank) buyer: String, @ConstrainedIterable(NonEmpty) lines: List[OrderLine])

case class PurchaseOrderLine(@ConstrainedNumber(GreaterThanOrEqualTo(1)) quantity: Int, @ConstrainedString(NotBlank) sku: String)

```

This lib extends your code with fluent assertions:

```scala

assertFailure(
  PurchaseOrder(buyer = " ", Nil),
   expectedErrors =
    AssertionError(property = "buyer", violatedConstraint = "NotBlank", message = "buyer must not be blank"),
    AssertionError(property = "lines", violatedConstraint = "NonEmpty", message = "lines must not be empty"))
    
    
assertFailure(
  PurchaseOrder(buyer = "Arnold", List(OrderLine(-1, " "))),
   expectedErrors =
    AssertionError(property = "lines.0.sku", violatedConstraint = "NotBlank", message = "lines.0.sku must not be blank"),
    AssertionError(property = "lines.0.quantity", violatedConstraint = "GreaterThanOrEqualTo", message = "-1 is not greater than or equal to 1"))
    
```

Lots of other constraint types to describe assertions are supported. Take a look at the test package.



