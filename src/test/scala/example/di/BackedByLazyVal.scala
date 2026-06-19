package example.di

import scala.annotation.{MacroAnnotation, experimental}
import scala.quoted.Quotes

/**
 * A `def` with this annotation will be rewritten through a macro to be cached in a hidden `lazy val`. Declaring a
 * `def` with this annotation instead of declaring a `lazy val` directly has the advantage that the `def` can be
 * accessed through `super` in overrides, whereas `lazy val`s can not be accessed through `super`.
 *
 * Example:
 * {{{
 *   @BackedByLazyVal def foo: String = "hello"
 * }}}
 * will be turned into
 * {{{
 *    private lazy val $macro123_foo_lazyval: String = "hello"
 *    @BackedByLazyVal def foo: String = this.$macro123_foo_lazyval
 * }}}
 */
@experimental
class BackedByLazyVal extends MacroAnnotation {

  override def transform(using q: Quotes)(definition: q.reflect.Definition, companion: Option[q.reflect.Definition]): List[q.reflect.Definition]= {
    import q.reflect.{*, given}

    val defDef = definition match
      case d: DefDef if d.paramss.isEmpty && d.rhs.isDefined => d
      case _ => report.errorAndAbort("Annotated element must be a parameterless non-abstract def.")

    val escapedParentClassName = Symbol.spliceOwner.fullName.replace('.', '_').replace("_$", "")

    val valDefSym = Symbol.newVal(Symbol.spliceOwner,
      name = Symbol.freshName(s"${escapedParentClassName}_${defDef.name}_lazyval"),
      tpe = defDef.returnTpt.tpe,
      flags = Flags.Lazy,
      privateWithin = Symbol.spliceOwner)

    val valDef = ValDef(valDefSym, defDef.rhs.map(_.changeOwner(valDefSym)))

    val redirectedDefDef = DefDef(defDef.symbol, rhsFn = { _ =>
      Some(Select(This(defDef.symbol.owner), valDefSym))
    })

    List(valDef, redirectedDefDef)
  }
}
