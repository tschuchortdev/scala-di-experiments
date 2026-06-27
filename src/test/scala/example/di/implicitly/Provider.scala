package example.di.implicitly

import java.util.UUID

trait Provider[A] {
  def get: A
  def cacheKey: String
}
object Provider {
  def of[A](a: A): Provider[A] = new Provider[A] {
    override def get: A = a
    override def cacheKey: String = s"${MacroUtils.unerasedTypeName[A]}-${UUID.randomUUID()}"
  }

  /*def ofCached[A](a: A)(using cache: ProviderCache): Provider[A] = {
    val provider = Provider.of(a)
    cache.getOrCreate(provider.cacheKey)(provider)
  }*/
}
