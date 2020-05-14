package org.nulluncertainty.macros

import IterableConstraints.IterableConstraint
import NumberConstraints._
import StringConstraints.StringConstraint
import org.nulluncertainty.expression.ComposableBooleanExp

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object NumberConstraints {
  sealed trait NumberConstraint
  case class EqualsTo(value: BigDecimal) extends NumberConstraint
  case class GreaterThan(value: BigDecimal) extends NumberConstraint
  case class GreaterThanOrEqualTo(value: BigDecimal) extends NumberConstraint
  case class LessThan(value: BigDecimal) extends NumberConstraint
  case class LessThanOrEqualTo(value: BigDecimal) extends NumberConstraint
  case class InclusiveRange(min: BigDecimal, max: BigDecimal) extends NumberConstraint
  case class ExclusiveRange(min: BigDecimal, max: BigDecimal) extends NumberConstraint
}
class ConstrainedNumber(constraints: NumberConstraint*) extends StaticAnnotation

object StringConstraints {
  sealed trait StringConstraint
  case class EqualsTo(string: String) extends StringConstraint
  case class EqualsToIgnoringCase(string: String) extends StringConstraint
  case class StartsWith(prefix: String) extends StringConstraint
  case class StartsWithIgnoringCase(prefix: String) extends StringConstraint
  case class EndsWith(suffix: String) extends StringConstraint
  case class EndsWithIgnoringCase(suffix: String) extends StringConstraint
  case class Contains(string: String) extends StringConstraint
  case class ContainsIgnoringCase(string: String) extends StringConstraint
  case class Matches(regex: String) extends StringConstraint
  case object Email extends StringConstraint
  case object Uri extends StringConstraint
  case object Alphanumeric extends StringConstraint
  case object Alphabetic extends StringConstraint
  case object Number extends StringConstraint
  case class LengthIs(value: Int) extends StringConstraint
  case class LongerThan(value: Int) extends StringConstraint
  case class ShorterThan(value: Int) extends StringConstraint
  case class LongerThanOrEqualTo(value: Int) extends StringConstraint
  case class ShorterThanOrEqualTo(value: Int) extends StringConstraint
  case object NotBlank extends StringConstraint
}
class ConstrainedString(constraints: StringConstraint*) extends StaticAnnotation

class ConstrainedType extends StaticAnnotation

object IterableConstraints {
  sealed trait IterableConstraint
  case object NonEmpty extends IterableConstraint
  case class AtLeast(numberOfElements: Int) extends IterableConstraint
  case class AtMost(numberOfItems: Int) extends IterableConstraint
  case class Contains[R](anObject: R) extends IterableConstraint
  case class MustBeEqual[R,R1](func: R => R1) extends IterableConstraint
  case class ForAll[R](predicate: R => ComposableBooleanExp[Unit]) extends IterableConstraint
  case class Exists[R](predicate: R => ComposableBooleanExp[Unit]) extends IterableConstraint
}
class ConstrainedIterable(constraints: IterableConstraint*) extends StaticAnnotation

class EnabledAssertions() extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro EnabledAssertionsImpl.impl
}

object EnabledAssertionsImpl {

  case class Error(property: String, violatedConstraint: String, message: String = "")

  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    case class Path(context: ValDef, segments: Seq[PathSegment] = Nil) {
      def contextName: Ident = context.tpt.asInstanceOf[Ident]
      def addProperty(name: String): Path = copy(segments = segments :+ PropertySegment(name))
      def addIndex(name: String): Path = copy(segments = segments :+ IndexSegment(name))
      def asFunction: c.universe.Tree =
        Function(List(context), segments.foldLeft[Tree](Ident(TermName("input"))) {
          (tree, segment) => segment.asSelect(tree)})
      def asSelect: c.universe.Tree =
        segments.foldLeft[Tree](Ident(TermName("input"))) {
          (tree, segment) => segment.asSelect(tree)}
      def asString: Tree = segments.map(_.asString).reduce{ (l, r) => q""" $l+"."+$r """}
      def nextIndexName: String = s"index${segments.count(_.isInstanceOf[IndexSegment])}"
    }
    trait PathSegment {
      def asSelect(tree: Tree): c.universe.Tree
      def asString: Tree
    }
    case class PropertySegment(propertyName: String) extends PathSegment {
      override def asSelect(tree: Tree): c.universe.Tree = Select(tree, TermName(propertyName))
      override def asString: Tree = q"""$propertyName"""
      override def toString: String = propertyName
    }
    case class IndexSegment(indexName: String) extends PathSegment {
      override def asSelect(tree: Tree): c.universe.Tree =
        Apply(Select(tree, TermName("apply")), List(Ident(TermName(indexName))))
      override def asString: Tree = q"""${Ident(TermName(indexName))}.toString"""
      override def toString: String = indexName
    }

    object StringAssertionBuilder extends((String,Path,List[Tree]) => Tree) {
      def apply(constraint: String, path: Path, constraintsArgs: List[Tree] = Nil): c.universe.Tree = constraint match {
        case "EqualsTo" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isEqualTo(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "EqualsTo", ${path.asSelect} + " is not equal to " + ${constraintsArgs.head})})
          """
        case "EqualsToIgnoringCase" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isEqualToIgnoringCase(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "EqualsToIgnoringCase", ${path.asSelect} + " is not equal to " + ${constraintsArgs.head})})
          """
        case "StartsWith" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .startsWith(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "StartsWith", ${path.asSelect} + " does not start with prefix " + ${constraintsArgs.head})})
          """
        case "StartsWithIgnoringCase" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .startsWithIgnoringCase(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "StartsWithIgnoringCase", ${path.asSelect} + " does not start with prefix " + ${constraintsArgs.head})})
          """
        case "EndsWith" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .endsWith(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "EndsWith", ${path.asSelect} + " does not end with suffix " + ${constraintsArgs.head})})
          """
        case "EndsWithIgnoringCase" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .endsWithIgnoringCase(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "EndsWithIgnoringCase", ${path.asSelect} + " does not end with suffix " + ${constraintsArgs.head})})
          """
        case "Contains" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .contains(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "Contains", ${path.asSelect} + " does not contain " + ${constraintsArgs.head})})
          """
        case "ContainsIgnoringCase" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .containsIgnoringCase(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "ContainsIgnoringCase", ${path.asSelect} + " does not contain " + ${constraintsArgs.head})})
          """
        case "Matches" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .matches(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "Matches", ${path.asSelect} + " does not match " + ${constraintsArgs.head})})
          """
        case "Email" =>
          q"""
             org.nulluncertainty.assertion.AssertionBuilder
              .assertThat(${path.asFunction})
              .isEmail
              .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "Email", ${path.asSelect} + " is not a valid email")})
            """
        case "Uri" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isUri
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "Uri", ${path.asSelect} + " is not an URI")})
          """
        case "Alphanumeric" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isAlphanumeric
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "Alphanumeric", ${path.asSelect} + " is not alphanumeric")})
          """
        case "Alphabetic" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isAlphabetic
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "Alphabetic", ${path.asSelect} + " is not alphabetic")})
          """
        case "Number" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isNumber
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "Number", ${path.asSelect} + " is not a number")})
          """
        case "LengthIs" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isSameLengthAs(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "LengthIs", ${path.asSelect} + " length is not" + ${constraintsArgs.head})})
          """
        case "LongerThan" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isLongerThan(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "LongerThan", ${path.asSelect} + " length is not longer than" + ${constraintsArgs.head})})
          """
        case "ShorterThan" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isShorterThan(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "ShorterThan", ${path.asSelect} + " length is not shorter than" + ${constraintsArgs.head})})
          """
        case "LongerThanOrEqualTo" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isLongerThanOrEqualTo(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "LongerThanOrEqualTo", ${path.asSelect} + " length is not longer than or equal to" + ${constraintsArgs.head})})
          """
        case "ShorterThanOrEqualTo" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isShorterThanOrEqualTo(${constraintsArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "ShorterThanOrEqualTo", ${path.asSelect} + " length is not shorter than or equal to" + ${constraintsArgs.head})})
          """
        case "NotBlank" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isNotBlank
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "NotBlank", ${path.asSelect} + " must not be blank")})
          """
      }
    }
    object NumberAssertionBuilder extends((String,Path,List[Tree]) => Tree) {
      def apply(constraint: String, path: Path, constraintArgs: List[Tree] = Nil): c.universe.Tree = constraint match {
        case "GreaterThan" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isGreaterThan(${constraintArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "GreaterThan", ${path.asSelect} + " is not greater than " + ${constraintArgs.head})})
          """
        case "GreaterThanOrEqualTo" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isGreaterThanOrEqualTo(${constraintArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "GreaterThanOrEqualTo", ${path.asSelect} + " is not greater than or equal to " + ${constraintArgs.head})})
          """
        case "LessThan" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isLessThan(${constraintArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "LessThan", ${path.asSelect} + " is not less than " + ${constraintArgs.head})})
          """
        case "LessThanOrEqualTo" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isLessThanOrEqualTo(${constraintArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "LessThanOrEqualTo", ${path.asSelect} + " is not less than or equal to " + ${constraintArgs.head})})
          """
        case "EqualsTo" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isEqualTo(${constraintArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "EqualsTo", ${path.asSelect} + " is not equal to " + ${constraintArgs.head})})
           """
        case "InclusiveRange" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isInInclusiveRange(${constraintArgs.head}, ${constraintArgs.tail.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "InclusiveRange", ${path.asSelect} + " is not in inclusive range " + ${constraintArgs.head}+" to "+${constraintArgs.tail.head} )})
          """
        case "ExclusiveRange" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isInExclusiveRange(${constraintArgs.head}, ${constraintArgs.tail.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "ExclusiveRange", ${path.asSelect} + " is not in exclusive range " + ${constraintArgs.head}+" to "+${constraintArgs.tail.head} )})
          """
      }
    }
    case object IterableAssertionBuilder {
      def apply(constraint: String, path: Path, constraintArgs: List[Tree] = Nil): c.universe.Tree = constraint match {
        case "NonEmpty" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .isNotEmpty
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "NonEmpty", ${path.asString} + " must not be empty")})
          """
        case "AtLeast" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.addProperty("size").asFunction})
            .isGreaterThanOrEqualTo(${constraintArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "AtLeast", ${path.asString} + " must contain at least " + ${constraintArgs.head} + " elements" )})
          """
        case "AtMost" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.addProperty("size").asFunction})
            .isLessThanOrEqualTo(${constraintArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "AtMost", ${path.asString} + " must contain at most " + ${constraintArgs.head} + " elements" )})
          """
        case "Contains" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .contains(${constraintArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "Contains", ${path.asString} + " does not contain " + ${constraintArgs.head} )})
          """
        case "MustBeEqual" =>
          val propertyName = constraintArgs.head match {
            case Function(_,Select(_,TermName(propertyName))) => q"""$propertyName"""
            case _ => q"""${path.segments.last.asString}"""
          }
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .mustBeEqual(${constraintArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "MustBeEqual", $propertyName + " must be equal across all elements" )})
          """
        case "ForAll" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .forAll(${constraintArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "ForAll", "" )})
          """
        case "Exists" =>
          q"""
           org.nulluncertainty.assertion.AssertionBuilder
            .assertThat(${path.asFunction})
            .existAny(${constraintArgs.head})
            .otherwise({input: ${path.contextName} => org.nulluncertainty.macros.EnabledAssertionsImpl.Error(${path.asString}, "Exists", "" )})
          """
      }
    }
    object IterateeAssertionBuilder extends((String,Path,Tree) => Tree) {
      override def apply(iterateeTypeName: String, path: Path, collAssertions: Tree): c.universe.Tree = {
        val randomIndexName: String = path.nextIndexName
        def iterateeAssertions() = iterateeTypeName match {
          // TODO WARN--- evalute more cases where wrapped object types are Dates, etc...!!!
          case "String" | "BigDecimal" | "Int" | "Long" | "Double" | "Float" =>
            q"""org.nulluncertainty.expression.SuccessfulAssertionExp()"""
          case _ =>
            val typeInCollection = ValDef(Modifiers(), TermName("input"),
              Ident(TypeName(iterateeTypeName)), EmptyTree)
            ObjectAssertionBuilder(
              c.typecheck(typeInCollection.duplicate, c.TYPEmode).asInstanceOf[ValDef]
                .tpt.symbol.asClass.primaryConstructor.asMethod,
              path.addIndex(randomIndexName))
        }
        q"""
         $collAssertions.ifTrue(
           new org.nulluncertainty.expression.ComposableAssertionExp[${path.contextName},${path.contextName},${path.contextName}]() {
              override def evaluate(input: ${path.contextName}): org.nulluncertainty.expression.AssertionResultBehaviour[${path.contextName}] = {
                ${path.asSelect}
                  .zipWithIndex
                  .foldLeft[org.nulluncertainty.expression.ComposableAssertionExp[${path.contextName},${path.contextName},${path.contextName}]](org.nulluncertainty.expression.SuccessfulAssertionExp[${path.contextName}]()) {
                    case (assertion, (_, ${TermName(randomIndexName)})) =>
                      assertion.and(${iterateeAssertions()})
                  }
                  .evaluate(input)
              }
           }
         )
       """
      }
    }
    case object ObjectAssertionBuilder extends((MethodSymbol,Path) => Tree) {
      override def apply(constructor: MethodSymbol, path: Path): c.universe.Tree = {
        constructor.paramLists.flatten.map { param =>
          val propertyPath = path.addProperty(param.asTerm.name.decodedName.toString)
          param.annotations.map(_.tree).map {
            case Apply(Select(Tuple2(New(constrainedTypeName), _)), constraints) =>
              constrainedTypeName.symbol.asClass.name.toString match {

                case "ConstrainedNumber" =>
                  constraints.map {
                    case Select(_, TermName(constraint)) =>
                      NumberAssertionBuilder(constraint, propertyPath)
                    case Apply(Select(Tuple2(Select(_, TermName(constraint)), _)), constraintArgs) =>
                      NumberAssertionBuilder(constraint, propertyPath,
                        constraintArgs.collect { case Apply(_, (literal@Literal(_)) :: _) => literal })
                  }.reduce((left, right) => q"""$left.and($right)""")

                case "ConstrainedString" =>
                  constraints.map {
                    case Select(_, TermName(constraint)) =>
                      StringAssertionBuilder(constraint, propertyPath)
                    case Apply(Select(Tuple2(Select(_, TermName(constraint)), _)), constraintArgs) =>
                      StringAssertionBuilder(constraint, propertyPath,
                        constraintArgs.collect { case Apply(_, (literal@Literal(_)) :: _) => literal })
                  }.reduce((left, right) => q"""$left.and($right)""")

                case "ConstrainedType" =>
                  ObjectAssertionBuilder(
                    param.asTerm.typeSignature.typeSymbol.asClass.primaryConstructor.asMethod,
                    propertyPath)

                case "ConstrainedIterable" =>
                  IterateeAssertionBuilder(
                    param.typeSignature.typeArgs.head.typeSymbol.name.toString,
                    propertyPath,
                    constraints.map {
                      case Select(_, TermName(constraint)) =>
                        IterableAssertionBuilder(constraint, propertyPath, Nil)
                      case Apply(Select(Tuple2(Select(_, TermName(constraint)), _)), constraintArgs) =>
                        IterableAssertionBuilder(constraint, propertyPath, constraintArgs)
                      case Apply(TypeApply(Ident(TermName(constraint)), _), mappingFunc::_) =>
                        IterableAssertionBuilder(constraint, propertyPath, List(mappingFunc))
                      case Apply(TypeApply(Select(Select(arg1,TermName(constraint)),arg2),arg3),mappingFunc::arg4b) =>
                        IterableAssertionBuilder(constraint, propertyPath, List(mappingFunc))
                    }.reduce((left, right) => q"""$left.ifTrue($right)"""))

                case _ => q"""org.nulluncertainty.expression.SuccessfulAssertionExp()"""
              }
          }.reduceOption((left, right) => q"""$left.and($right)""")
            .getOrElse(q"""org.nulluncertainty.expression.SuccessfulAssertionExp()""")
        }.reduceOption((left, right) => q"""$left.and($right)""")
          .getOrElse(q"""org.nulluncertainty.expression.SuccessfulAssertionExp()""")
      }
    }

    annottees.map(_.tree).toList match {
      case q"case class $className(..$fields) extends ..$parents { ..$body }" :: _ =>

        val assertionContextAtInstantiation = ValDef(Modifiers(), TermName("input"),
          Ident(TypeName(className.asInstanceOf[TypeName].decodedName.toString)), EmptyTree)
        val path = Path(assertionContextAtInstantiation)

        val instantiationAssertion: Tree =
          fields.collect {

            case ValDef(Modifiers(_,_,Apply(Select(New(Ident(TypeName("ConstrainedNumber"))), _), propertyAssertions) :: _), TermName(property), Ident(TypeName(objectClassName)), _) if Seq("BigDecimal","Int","Long","Double","Float").contains(objectClassName) =>
              propertyAssertions.map {
                case Apply(Ident(TermName(constraint)), constraintArgs) =>
                  NumberAssertionBuilder(constraint, path.addProperty(property), constraintArgs)
                case Ident(TermName(constraint)) =>
                  NumberAssertionBuilder(constraint, path.addProperty(property), Nil)
              }.reduce((left, right) => q"""$left.and($right)""")

            case ValDef(Modifiers(_,_,Apply(Select(New(Ident(TypeName("ConstrainedString"))), _), propertyAssertions) :: _), TermName(property), Ident(TypeName(objectClassName)), _) if "String" == objectClassName =>
              propertyAssertions.map {
                case Apply(Ident(TermName(constraint)), constraintArgs) =>
                  StringAssertionBuilder(constraint, path.addProperty(property), constraintArgs)
                case Ident(TermName(constraint)) =>
                  StringAssertionBuilder(constraint, path.addProperty(property), Nil)
              }.reduce((left, right) => q"""$left.and($right)""")

            case valDef@ValDef(Modifiers(_,_,Apply(Select(New(Ident(TypeName("ConstrainedType"))), _),_) :: _), TermName(property), Ident(TypeName(_)), _) =>
              ObjectAssertionBuilder(
                c.typecheck(valDef.duplicate, c.TYPEmode).asInstanceOf[ValDef]
                  .tpt.symbol.asClass.primaryConstructor.asMethod,
                path.addProperty(property))

            case ValDef(Modifiers(_,_,Apply(Select(New(Ident(TypeName("ConstrainedIterable"))), _), propertyAssertions) :: _),TermName(property),
            AppliedTypeTree(_, ident),_) if ident.tail.getClass.getName.contains("collection") =>

              IterateeAssertionBuilder(
                ident.head.asInstanceOf[Ident].name.decodedName.toString,
                path.addProperty(property),
                propertyAssertions.map {
                  case Apply(Ident(TermName(constraint)), constraintArgs) =>
                    IterableAssertionBuilder(constraint, path.addProperty(property), constraintArgs)
                  case Ident(TermName(constraint)) =>
                    IterableAssertionBuilder(constraint, path.addProperty(property), Nil)
                  case Apply(TypeApply(Ident(TermName(constraint)), _), mappingFunc::_) =>
                    IterableAssertionBuilder(constraint, path.addProperty(property), List(mappingFunc))
                }.reduce((left, right) => q"""$left.ifTrue($right)"""))

          }.reduceOption((left, right) => q"""$left.and($right)""")
            .getOrElse(q"""org.nulluncertainty.expression.SuccessfulAssertionExp()""")

        c.Expr[Any](q"""
          case class $className ( ..$fields ) extends ..$parents {
            $instantiationAssertion.evaluate(this).signalIfFailed()
            ..$body
          }
        """)

      case _ => c.abort(c.enclosingPosition, "Invalid annottee")
    }
  }
}
