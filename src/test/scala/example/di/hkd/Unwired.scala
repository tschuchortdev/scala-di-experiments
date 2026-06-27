/*package example.di.hkd

import _root_.com.tschuchort.hkd.*
import cats.effect.IO
import cats.effect.kernel.Resource
import cats.effect.syntax.all.*
import cats.syntax.all.*

import scala.quoted.{Expr, Type}

type Unwired[Deps[_[_]]] = Deps[[A] =>> Deps[IO] => Resource[IO, Resource[IO, A]]]
object Unwired {
  transparent inline def apply[Deps[_[_]]]: Any = ${ applyImpl[Deps] }
  private def applyImpl[Deps[_[_]]: Type](using q: scala.quoted.Quotes): Expr[Any] = {
    import q.reflect.{*, given}

    val e = Select.unique(Ref(TypeRepr.of[Deps].typeSymbol.companionModule), "apply")
      .appliedToType(TypeRepr.of[[A] =>> Deps[IO] => Resource[IO, Resource[IO, A]]])
      .etaExpand(Symbol.spliceOwner)

    e.asExprOf[Any]
  }
}*/
