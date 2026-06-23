package example.di.implicitly

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


  protected[implicitly] inline def typeName[T]: String = ${ typeNameImpl[T] }

  private def typeNameImpl[T: Type](using Quotes): Expr[String] = {
    // Type.show returns the fully qualified name including type arguments, e.g., "com.example.Foo[java.lang.String]"
    Expr(Type.show[T])
  }
}