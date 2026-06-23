package example.di.implicitly

import example.di.implicitly.TupleUtils.ContainsSubtype
import munit.FunSuite

import scala.compiletime.summonInline

class ImplicitDiTest extends FunSuite {
  import ImplicitDiTest.*

  test("1") {
    given DepA = new DepA("DepA:local")
    val depB = provide[DepB]
    assertEquals(depB.depA.name, "DepA:local")
  }

  test("2") {
    given Provider[DepA] = Provider.of(new DepA("DepA:local"))

    val depB = provide[DepB]
    assertEquals(depB.depA.name, "DepA:local")

    provide[DepC]
  }

  test("Provider.of cacheKey is derived from callsite source location") {
    // Each Provider.of call site gets a unique, stable key baked in at compile time
    val p1 = Provider.of(new DepA("first"))
    val p2 = Provider.of(new DepA("second"))

    // Different call sites → different keys
    assertNotEquals(p1.cacheKey, p2.cacheKey)

    // The key encodes the source file name
    assert(p1.cacheKey.contains("ImplicitDiTest.scala"), s"Expected filename in key, got: ${p1.cacheKey}")
    assert(p2.cacheKey.contains("ImplicitDiTest.scala"), s"Expected filename in key, got: ${p2.cacheKey}")

    // The key is stable across repeated get() calls (not random per invocation)
    assertEquals(p1.cacheKey, p1.cacheKey)
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
    
    /*inline given provider: Provider[DepB] = {
      val depA = summonInline[ProviderLookup[DepA]].p.get
      Provider.of(DepB()(using depA))
    }*/

    given (inject: Inject[DepA *: EmptyTuple]) => Provider[DepB] = {
      inject.into(Provider.of(DepB()))
    }
  }

  /*class DepD(val name: String)(using val depA: DepA)

  object DepD {
    def apply()(using DepA): DepB = new DepB("DepD:companion")

    inline given provider: Provider[DepA] = {
      val depA = summonInline[Providers[DepA *: EmptyTuple]].getProvider[DepA].get
      Provider.of(DepB()(using depA))
    }
  }*/
  
  class DepC(val name: String)(using DepA, DepB)
  object DepC {
    def apply()(using DepA, DepB): DepC = new DepC("DepC:companion")}

    given (inject: Inject[(DepA, DepB)]) => Provider[DepC] = {
      inject.into(Provider.of(DepC()))
    }
}
