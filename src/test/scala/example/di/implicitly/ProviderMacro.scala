package example.di.implicitly

import scala.quoted.*

private[implicitly] object ProviderMacro {

  /** Concrete class to prevent anonymous classes being generated on every macro invocation */
  private case class ProviderImpl[A](cacheKey: String, get: A) extends Provider[A]

  def ofGivenImpl[A: Type](a: Expr[A])(using q: Quotes): Expr[ProviderLookup[A]] = {
    import q.reflect.{*, given}

    // Need to use underlyingArgument here to get the argument's declaration's name instead of the parameter name.
    // `underlying` alone doesn't seem to work when the given is inside an anonymous block.
    val declarationName =  a.asTerm.underlyingArgument.symbol.fullName
    assert(declarationName != "<none>")
    report.info(s"Resolved provider for ${Type.show[A]}: ${declarationName}")

    '{
      ProviderLookup[A](ProviderImpl[A](
        cacheKey = ${ Expr(declarationName) },
        get = ${ a })
      )
    }
  }
}
