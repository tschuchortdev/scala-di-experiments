package example.di.implicitly

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
  }

  test("3") {
    val depB = provide[DepB]
    println("hello")
    assertEquals(depB.depA.name, "DepA:companion")
  }
}

object ImplicitDiTest {
  class DepA(val name: String)
  object DepA {
    inline given provider: DefaultProvider[DepA] =
      DefaultProvider(Provider.of(DepA("DepA:companion")))
  }
  
  class DepB(val name: String)(using val depA: DepA)
  object DepB {
    def apply()(using DepA): DepB = new DepB("DepB:companion")
    
    /*inline given provider: Provider[DepB] = {
      val depA = summonInline[Provider[DepA]].get
      Provider.of(DepB()(using depA))
    }*/

    inline given provider: (p1: Provider[DepA]) => Provider[DepB] =
      Provider.of(DepB()(using p1.get))
  }

  /*class DepD(val name: String)(using val depA: DepA)

  object DepD {
    def apply()(using DepA): DepB = new DepB("DepD:companion")

    inline given provider: Provider[DepA] = {
      val depA = summonInline[Providers[DepA *: EmptyTuple]].getProvider[DepA].get
      Provider.of(DepB()(using depA))
    }
  }*/
  
  class DepC(val name: String)(using DepA, DepB) {
    
  }
}