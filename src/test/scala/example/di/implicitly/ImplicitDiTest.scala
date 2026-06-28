//noinspection TypeAnnotation
package example.di.implicitly

import munit.FunSuite

class ImplicitDiTest extends FunSuite {
  import ImplicitDiTest.*

  test("Prefers local given over companion object given Provider") {
    given DepA = new DepA("DepA:local")
    val depA = provide[DepA]
    assertEquals(depA.name, "DepA:local")
  }

  test("Prefers local given Provider over companion object given Provider") {
    given Provider[DepA] = Provider.of(new DepA("DepA:local"))
    val depA = provide[DepA]
    assertEquals(depA.name, "DepA:local")
  }

  test("Prefers local given over companion object given Provider in transitive dependency") {
    given DepA = new DepA("DepA:local")
    val depB = provide[DepB]
    assertEquals(depB.name, "DepB:companion")
    assertEquals(depB.depA.name, "DepA:local")
  }

  test("Prefers local given Provider over companion object given Provider in transitive dependency") {
    given Provider[DepA] = Provider.of(new DepA("DepA:local"))
    val depB = provide[DepB]
    assertEquals(depB.name, "DepB:companion")
    assertEquals(depB.depA.name, "DepA:local")
  }

  test("Provides dependency from cache") {
    given cache: ProviderCache = ProviderCache()
    given Provider[DepB] = Inject[DepA *: EmptyTuple].intoCached(DepB())

    val depB1 = provide[DepB]
    val depB2 = provide[DepB]
    assert(depB1 eq depB2)
  }

  test("Provides dependency from parent cache when child cache does not have it") {
    val parent = ProviderCache()
    val parentDep = new DepA("parent")
    val _ = parent.getOrCreate("myKey")(parentDep)

    val child = ProviderCache.inherit(parent)
    // Child has no own entry for "myKey" → must delegate to parent
    val result = child.get[DepA]("myKey")
    assertEquals(result, Some(parentDep))
    assert(result.get eq parentDep)
  }

  test("Provides dependency from child cache when both parent and child cache have it") {
    val parent = ProviderCache()
    val parentDep = new DepA("parent")
    val _ = parent.getOrCreate("myKey")(parentDep)

    val child = ProviderCache.inherit(parent)
    val childDep = new DepA("child")
    // Populate the child's own store with a different instance under the same key
    val _ = child.getOrCreate("myKey")(childDep)

    val result = child.get[DepA]("myKey")
    assertEquals(result, Some(childDep))
    assert(result.get eq childDep, "Child's own entry should take precedence over parent's")
  }

  test("Dependency in parent cache remains untouched when child cache is closed") {
    // TODO
  }

  test("Invalidates cached dependency when a transitive dependency's provider is replaced by local given Provider") {
    given cache: ProviderCache = ProviderCache()

    val cacheKey1 = Inject[DepA *: EmptyTuple].intoCached(DepB()).cacheKey

    // The overriding given is confined to the inner block so it cannot interfere with cacheKey1 above
    val cacheKey2 = {
      given Provider[DepA] = Provider.of(new DepA("local"))
      Inject[DepA *: EmptyTuple].intoCached(DepB()).cacheKey
    }

    assertNotEquals(cacheKey1, cacheKey2,
      "Replacing DepA's provider must change DepB's cache key, invalidating the cached DepB")
  }

  test("Invalidates cached dependency when a transitive dependency's provider is replaced by local given") {
    given cache: ProviderCache = ProviderCache()

    val cacheKey1 = Inject[DepA *: EmptyTuple].intoCached(DepB()).cacheKey

    // A direct given DepA uses its declaration name as cache key (not a UUID)
    val cacheKey2 = {
      given localDep: DepA = new DepA("local")
      Inject[DepA *: EmptyTuple].intoCached(DepB()).cacheKey
    }

    assertNotEquals(cacheKey1, cacheKey2,
      "Replacing DepA with a local given must change DepB's cache key, invalidating the cached DepB")
  }

  test("Two local givens have different cache keys") {
    // Define two distinct named givens in separate scopes to avoid ambiguity
    val key1 = {
      given dep1: DepA = new DepA("first")
      summon[ProviderLookup[DepA]].p.cacheKey
    }
    val key2 = {
      given dep2: DepA = new DepA("second")
      summon[ProviderLookup[DepA]].p.cacheKey
    }
    assertNotEquals(key1, key2,
      "Two different given declarations must produce different cache keys")
  }

  test("Local given (compiled to val) cache key remains the same across multiple calls to provide") {
    // A simple given alias is compiled to a lazy val; the macro captures the declaration name.
    given dep: DepA = new DepA("stable")
    val key1 = summon[ProviderLookup[DepA]].p.cacheKey
    val key2 = summon[ProviderLookup[DepA]].p.cacheKey
    assertEquals(key1, key2,
      "A val given must yield the same cache key on every summon")
    // The same DepA instance is returned on every access
    assert(provide[DepA] eq provide[DepA])
  }

  test("Local given (compiled to def) cache key remains the same across multiple calls to provide") {
    // A parameterized given is compiled to a def and called anew each time it is summoned,
    // producing a different DepA value on each call. Nevertheless the macro-derived cache key
    // is the declaration name, which is stable.
    given dep(using DummyImplicit): DepA = new DepA(java.util.UUID.randomUUID().toString)
    val key1 = summon[ProviderLookup[DepA]].p.cacheKey
    val key2 = summon[ProviderLookup[DepA]].p.cacheKey
    assertEquals(key1, key2,
      "A def given must still yield the same (declaration-name-based) cache key on every summon")
    // But the value itself is recreated on each summon
    assert(provide[DepA] ne provide[DepA])
  }

  test("When a local given that was cached goes out of scope, the next call to provide will not use it") {
    var inScopeKey: String = ""
    {
      given dep: DepA = new DepA("in-scope")
      inScopeKey = summon[ProviderLookup[DepA]].p.cacheKey
      assertEquals(provide[DepA].name, "in-scope")
    }
    // dep is no longer in scope; fallback is the companion-object Provider[DepA]
    assertEquals(provide[DepA].name, "DepA:companion")
    assertNotEquals(summon[ProviderLookup[DepA]].p.cacheKey, inScopeKey,
      "After the local given goes out of scope, a different provider (with a different key) must be used")
  }

  test("A local given can access the current provider to modify the dependency (like super.copy)") {
    given superr: Config(environment = "prod", dbPassword = "qwerty")

    {
      given newConfig: Config = provide[Config].copy(dbPassword = "newpassword")

      val providedConfig = provide[Config]
      assert(providedConfig eq newConfig)
      assertEquals(providedConfig.environment, "prod")
      assertEquals(providedConfig.dbPassword, "newpassword")
    }
  }


  /*test("Imports from objects can be used to override multiple dependencies at once") {
    class DbMixin(using ProviderLookup[Config]) {
      given Provider[Config] = Provider.of(provide[Config].copy(dbPassword = "newpassword"))
    }

    {
      given superr: Provider[Config] = Provider.of(Config(environment = "prod", dbPassword = "qwerty"))

      {
        val dbMixin = new DbMixin

        import dbMixin.given

        val providedConfig = provide[Config]
        assertEquals(providedConfig.environment, "prod")
        assertEquals(providedConfig.dbPassword, "newpassword")
      }
    }
  }

  test("Multiple object imports can be chained, each accessing the previous provider to modify the dependency") {
  }*/
}

object ImplicitDiTest {
  class DepA(val name: String)
  object DepA {
    given provider: Provider[DepA] = Provider.of(DepA("DepA:companion"))
  }

  class DepB(val name: String)(using val depA: DepA)
  object DepB {
    def apply()(using DepA): DepB = new DepB("DepB:companion")

    given (inject: Inject[DepA *: EmptyTuple]) => Provider[DepB] = {
      inject.into(DepB())
    }
  }

  class DepC(val name: String)(using DepA, DepB)
  object DepC {
    def apply()(using DepA, DepB): DepC = new DepC("DepC:companion")

    given (inject: Inject[(DepA, DepB)]) => Provider[DepC] = {
      inject.into(DepC())
    }
  }

  case class Config(environment: String, dbPassword: String)
}
