//noinspection TypeAnnotation
package example.di.implicitly

import example.di.implicitly.TupleUtils.ContainsSubtype
import munit.FunSuite

import scala.compiletime.summonInline

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
    assertEquals(depB.depA.name, "DepA:local")
  }

  test("Prefers local given Provider over companion object given Provider in transitive dependency") {
    given Provider[DepA] = Provider.of(new DepA("DepA:local"))
    val depB = provide[DepB]
    assertEquals(depB.depA.name, "DepA:local")
  }

  test("Provides dependency from cache") {
  }

  test("Provides dependency from parent cache when child cache does not have it") {
  }

  test("Provides dependency from child cache when both parent and child cache have it") {
  }

  test("Dependency in parent cache remains untouched when child cache is closed") {
  }

  test("Invalidates cached dependency when a transitive dependency's provider is replaced by local given Provider") {
  }

  test("Invalidates cached dependency when a transitive dependency's provider is replaced by local given") {
  }

  test("Two local givens have different cache keys") {
  }

  test("Local given (compiled to val) cache key remains the same across multiple calls to provide") {
  }

  test("Local given (compiled to def) cache key remains the same across multiple calls to provide") {
  }

  test("When a local given that was cached goes out of scope, the next call to provide will not use it") {
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

  test("Imports from objects can be used to override multiple dependencies at once") {
  }

  test("Multiple object imports can be chained, each accessing the previous provider to modify the dependency") {
  }
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
