package example.di.`implicit`

import scala.quoted.*

object MacroUtils {

  def extractTupleTypes(using Quotes)(t: quotes.reflect.TypeRepr): List[quotes.reflect.TypeRepr] = {
    import quotes.reflect.*
    t match
      case AppliedType(tf, args) if defn.isTupleClass(tf.typeSymbol) =>
        args

      case AppliedType(tf, args) if tf.typeSymbol == Symbol.requiredClass("scala.*:") =>
        args.head :: extractTupleTypes(args.tail.head)

      case tf if tf <:< TypeRepr.of[EmptyTuple] =>
        List.empty

      case _ =>
        report.errorAndAbort(s"Type ${t.show} is not a tuple")
  }

}