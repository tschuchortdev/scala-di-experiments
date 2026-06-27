package example.di.implicitly

import example.di.implicitly.TupleUtils.{ContainsSubtype, IndexOfSubtype}

import java.util.UUID
import scala.annotation.targetName
import scala.compiletime.ops.int.S
import scala.compiletime.{constValue, summonInline}
import scala.util.NotGiven





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

case class Inject[Xs <: NonEmptyTuple](allProviders: Tuple.Map[Xs, Provider]) {
  inline def getProvider[T](using ContainsSubtype[Xs, T] =:= true): Provider[T] = {
    val index = constValue[IndexOfSubtype[Xs, T]]
    allProviders.productElement(index).asInstanceOf[Provider[T]]
  }
}
trait InjectLowLowPriorityImplicits {
  given injectN: [Head, Tail <: NonEmptyTuple] =>(pl: ProviderLookup[Head], is: Inject[Tail]) => Inject[Head *: Tail] =
    Inject(pl.p *: is.allProviders)
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

  def apply[Xs <: NonEmptyTuple](using i: Inject[Xs]): Inject[Xs] = i

  given inject1: [T] => (pl: ProviderLookup[T]) => Inject[T *: EmptyTuple] =
    Inject(pl.p *: EmptyTuple)

  // These superfluous givens with fixed arity exist mainly to make the debugging output nicer.

  // Implementation note: (T1, T2) is an alias for Tuple2, which is a different type from T1 *: T2 *: EmptyTuple. We need both.
  given injectT2: [T1, T2] => (pl1: ProviderLookup[T1], pl2: ProviderLookup[T2]) => Inject[(T1, T2)] =
    Inject((pl1.p, pl2.p))

  given injectT3: [T1, T2, T3] => (pl1: ProviderLookup[T1], pl2: ProviderLookup[T2], pl3: ProviderLookup[T3]) => Inject[(T1, T2, T3)] =
    Inject((pl1.p, pl2.p, pl3.p))

  private class ProviderFromInject[T, Deps <: NonEmptyTuple](inject: Inject[Deps], create: () => T) extends Provider[T] {
    private val dependencyCacheKeys: Seq[String] =
      inject.allProviders.productIterator.map(_.asInstanceOf[Provider[?]].cacheKey).toSeq

    override def get: T = create()
    override def cacheKey: String = s"${MacroUtils.unerasedTypeName[T]}(${dependencyCacheKeys.mkString(",")})"
  }

  private class ProviderFromInjectCached[T, Deps <: NonEmptyTuple](inject: Inject[Deps], create: () => T)(using cache: ProviderCache) extends Provider[T] {
    private val dependencyCacheKeys: Seq[String] =
      inject.allProviders.productIterator.map(_.asInstanceOf[Provider[?]].cacheKey).toSeq

    override def get: T = cache.getOrCreate(cacheKey)(create())
    override def cacheKey: String = s"${MacroUtils.unerasedTypeName[T]}(${dependencyCacheKeys.mkString(",")})"
  }

  extension [T1](i: Inject[T1 *: EmptyTuple])
    def into[R](cont: T1 ?=> R): Provider[R] =
      new ProviderFromInject(i, () => cont(using i.allProviders._1.get))

  extension [T1](i: Inject[T1 *: EmptyTuple])
    def intoCached[R](cont: T1 ?=> R)(using cache: ProviderCache): Provider[R] =
      new ProviderFromInjectCached(i, () => cont(using i.allProviders._1.get))

 /* extension [T1](i: Inject[Tuple1[T1]])
    @targetName("intoT1")
    def into[R](cont: T1 ?=> R): Provider[R] =
      new ProviderFromInject(i.asInstanceOf[Inject[T1 *: EmptyTuple]], () => cont(using i.asInstanceOf[Inject[T1 *: EmptyTuple]].allProviders._1.get))
  */

  /*extension [T1, T2](p: Inject[Tuple2[T1, T2]])
    @targetName("intoT")
    def into[R](cont: (T1, T2) ?=> R): R = {
      val all = p.allProviders
      cont(using all._1.get, all._2.get)
    }*/

  extension [T1, T2](i: Inject[T1 *: T2 *: EmptyTuple])
    def into[R](cont: (T1, T2) ?=> R): Provider[R] =
      new ProviderFromInject(i, () => cont(using i.allProviders._1.get, i.allProviders._2.get))

  extension [T1, T2, T3](i: Inject[T1 *: T2 *: T3 *: EmptyTuple])
    def into[R](cont: (T1, T2, T3) ?=> R): Provider[R] =
      new ProviderFromInject(i, () => cont(using i.allProviders._1.get, i.allProviders._2.get, i.allProviders._3.get))

  extension [T1, T2, T3, T4](i: Inject[T1 *: T2 *: T3 *: T4 *: EmptyTuple])
    def into[R](cont: (T1, T2, T3, T4) ?=> R): Provider[R] =
      new ProviderFromInject(i, () => cont(using i.allProviders._1.get, i.allProviders._2.get, i.allProviders._3.get, i.allProviders._4.get))
}

class ProviderCache protected (private val parent: Option[ProviderCache]) extends AutoCloseable {
  def this() = this(None)

  private val store = scala.collection.concurrent.TrieMap[String, Any]()

  def get[T](key: String): Option[T] =
    store.get(key).orElse(parent.flatMap(_.get(key))).map(_.asInstanceOf[T])

  def getOrCreate[T](key: String)(value: => T): T = {
    store.getOrElseUpdate(key, value).asInstanceOf[T]
  }

  override def close(): Unit = {
    // TODO
  }
}
object ProviderCache {
  def inherit(parent: ProviderCache): ProviderCache = new ProviderCache(Some(parent))
}