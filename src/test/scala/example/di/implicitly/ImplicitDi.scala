package example.di.implicitly

import scala.util.NotGiven


trait Provider[A] {
  def get: A
}
object Provider {
  export DefaultProvider.given

  def of[A](a: => A): Provider[A] = new Provider[A] {
    override def get: A = a
  }

  given ofGiven: [A] => (a: A) => Provider[A] {
    override def get: A = a
  }
}

case class ProviderLookup[A](p: Provider[A])
object ProviderLookup {
  given [A] => (p: Provider[A]) => ProviderLookup[A] = ProviderLookup(p)
}

case class DefaultProvider[A](p: Provider[A])
object DefaultProvider {
  case class Delay[A](dp: DefaultProvider[A])
  //given [A] => (dp: DefaultProvider[A]) => DefaultProvider.Delay[A] = DefaultProvider.Delay(dp)
  //given [A] => (d: DefaultProvider.Delay[A]) => Provider[A] = d.dp.p
  given foo: [A] => (dp: DefaultProvider[A]) => NotGiven[Provider[A]] => Provider[A] = dp.p
}

def provide[A](using provider: Provider[A]): A = provider.get

case class Providers[Xs <: NonEmptyTuple](vec: Vector[Provider[?]]) {
  def getProvider[T, N <: Int](using i: TupleIndex.OfSubtype[Xs, T, N]): Provider[T] =
    vec(i.index).asInstanceOf[Provider[T]]
}
object Providers {
  given [T] => (pl: ProviderLookup[T]) => Providers[T *: EmptyTuple] =
    Providers(Vector(pl.p))

  given [Head, Tail <: NonEmptyTuple] => (pl: ProviderLookup[Head], ps: Providers[Tail]) => Providers[Head *: Tail] =
    Providers(pl.p +: ps.vec)

  /*extension [T1](p: Providers[T1 *: EmptyTuple])
    def use[R](cont: T1 ?=> R): R =
      cont(using p.getAll._1)

  extension [T1, T2](p: Providers[(T1, T2)])
    def use[R](cont: (T1, T2) ?=> R): R = {
      val all = p.getAll
      cont(using all._1, all._2)
    }

  extension [T1, T2, T3](p: Providers[(T1, T2, T3)])
    def use[R](cont: (T1, T2, T3) ?=> R): R = {
      val all = p.getAll
      cont(using all._1, all._2, all._3)
    }*/
}