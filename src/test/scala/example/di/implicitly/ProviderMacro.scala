package example.di.implicitly

import scala.quoted.*

private[implicitly] object ProviderMacro {

  /** Concrete class to prevent anonymous classes being generated on every macro invocation */
  private case class ProviderImpl[A](cacheKey: String, get: A) extends Provider[A]

  def ofGivenImpl[A: Type]()(using q: Quotes): Expr[ProviderLookup[A]] = {
    import q.reflect.{*, given}

    Implicits.search(TypeRepr.of[A]) match {
      case iss: ImplicitSearchSuccess =>
        val declarationName = iss.tree.symbol.fullName
        report.info(s"Resolved provider for ${Type.show[A]}: ${declarationName}")

        '{
          ProviderLookup[A](ProviderImpl[A](
            cacheKey = ${ Expr(declarationName) },
            get = ${ iss.tree.asExprOf[A] })
          )
        }

      case isf: ImplicitSearchFailure =>
        report.errorAndAbort(s"Cannot construct ProviderLookup.ofGiven: No given instance found for ${Type.show[A]}. Reason: ${isf.explanation}")
    }
  }
}
