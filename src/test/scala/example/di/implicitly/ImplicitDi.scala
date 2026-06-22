package example.di.implicitly

import example.di.implicitly.TupleUtils.{ContainsSubtype, IndexOfSubtype}

import scala.annotation.targetName
import scala.compiletime.ops.int.S
import scala.compiletime.{constValue, summonInline}
import scala.util.NotGiven


trait Provider[A] {
  def get: A
}
object Provider {
  def of[A](a: => A): Provider[A] = new Provider[A] {
    override def get: A = a
  }
}

/** Call this method to manually summon an instance of [[A]] from the [[Provider]] dependency graph. */
def provide[A](using pl: ProviderLookup[A]): A = pl.p.get

/** The [[ProviderLookup]] type exists solely to provide a level of indirection so the implicit prioritization can be
 * controlled. We want a locally defined `given A` to have higher priority than a `given Provider[A]` defined in `A`'s
 * companion object.
 *
 *  See [[https://github.com/rssh/notes/blob/570b62218ebee01e706a74289225773bc1621665/2025_12_01_implicit_search_priority.md]] */
case class ProviderLookup[A](p: Provider[A])
trait ProviderLookupLowPriorityImplicits {
  given l: [A] => (p: Provider[A]) => ProviderLookup[A] = ProviderLookup(p)
}
object ProviderLookup extends ProviderLookupLowPriorityImplicits {
  given ofGiven: [A] => (a: A) => ProviderLookup[A] = ProviderLookup(Provider.of(a))
}

private object TupleUtils {
  type ContainsSubtype[Xs <: Tuple, T] <: Boolean = Xs match
    case EmptyTuple => false
    case head *: tail => head match
      case T => true
      case _ => ContainsSubtype[tail, T]

  type IndexOfSubtype[Xs <: Tuple, T] <: Int | Nothing = Xs match
    case EmptyTuple => Nothing
    case xsh *: xst => xsh match
      case T => 0
      case _ => S[IndexOfSubtype[xst, T]]
}

case class Inject[Xs <: NonEmptyTuple](val allProviders: Tuple.Map[Xs, Provider]) {
  inline def getProvider[T](using ContainsSubtype[Xs, T] =:= true): Provider[T] = {
    val index = constValue[IndexOfSubtype[Xs, T]]
    allProviders.productElement(index).asInstanceOf[Provider[T]]
  }
}
trait InjectLowLowPriorityImplicits {
  //given injectN: [Head, Tail <: NonEmptyTuple] =>(pl: ProviderLookup[Head], is: Inject[Tail]) => Inject[Head *: Tail] =
    //Inject(pl.p *: is.allProviders)
}
trait InjectLowPriorityImplicits extends InjectLowLowPriorityImplicits {
  // These implicits must be a lower priority than the TupleX specializations. For some reason they are similar enough
  // to cause an ambiguity error but also different enough to not be an implicit search match.

  given inject2: [T1, T2] => (pl1: ProviderLookup[T1], pl2: ProviderLookup[T2]) => Inject[T1 *: T2 *: EmptyTuple] =
    Inject((pl1.p, pl2.p))

  given inject3: [T1, T2, T3] => (pl1: ProviderLookup[T1], pl2: ProviderLookup[T2], pl3: ProviderLookup[T3]) => Inject[T1 *: T2 *: T3 *: EmptyTuple] =
    Inject((pl1.p, pl2.p, pl3.p))
}
object Inject extends InjectLowPriorityImplicits {
  given inject1: [T] => (pl: ProviderLookup[T]) => Inject[T *: EmptyTuple] =
    Inject(pl.p *: EmptyTuple)

  // These superfluous givens with fixed arity exist mainly to make the debugging output nicer.

  // Implementation note: (T1, T2) is an alias for Tuple2, which is a different type from T1 *: T2 *: EmptyTuple. We need both.
  given injectT2: [T1, T2] =>(pl1: ProviderLookup[T1], pl2: ProviderLookup[T2]) => Inject[(T1, T2)] =
    Inject((pl1.p, pl2.p))

  given injectT3: [T1, T2, T3] => (pl1: ProviderLookup[T1], pl2: ProviderLookup[T2], pl3: ProviderLookup[T3]) => Inject[(T1, T2, T3)] =
    Inject((pl1.p, pl2.p, pl3.p))


  extension [T1](p: Inject[T1 *: EmptyTuple])
    def into[R](cont: T1 ?=> R): R =
      cont(using p.allProviders._1.get)

  /*extension [T1, T2](p: Inject[Tuple2[T1, T2]])
    @targetName("intoT")
    def into[R](cont: (T1, T2) ?=> R): R = {
      val all = p.allProviders
      cont(using all._1.get, all._2.get)
    }*/

  extension [T1, T2](p: Inject[T1 *: T2 *: EmptyTuple])
    def into[R](cont: (T1, T2) ?=> R): R = {
      val all = p.allProviders
      cont(using all._1.get, all._2.get)
    }

  extension [T1, T2, T3](p: Inject[(T1, T2, T3)])
    def into[R](cont: (T1, T2, T3) ?=> R): R = {
      val all = p.allProviders
      cont(using all._1.get, all._2.get, all._3.get)
    }

  extension [T1, T2, T3, T4](p: Inject[(T1, T2, T3, T4)])
    def into[R](cont: (T1, T2, T3, T4) ?=> R): R = {
      val all = p.allProviders
      cont(using all._1.get, all._2.get, all._3.get, all._4.get)
    }
}