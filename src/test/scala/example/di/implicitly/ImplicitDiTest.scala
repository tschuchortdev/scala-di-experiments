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
      inject.into(DepB())
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
      inject.into(DepC())
    }
}
