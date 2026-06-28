package example.di.implicitly

import munit.FunSuite

import scala.util.Using

class ExampleTest extends FunSuite {
  import ExampleTest.*

  given suiteCache: ProviderCache()

  override def afterAll(): Unit = {
    suiteCache.close()
    super.afterAll()
  }


  test("example") {
    Using.resource(ProviderCache.inherit(suiteCache)) { case given ProviderCache =>
      given DepA = new DepA("DepA:local")
      val depB = provide[DepB]

      assertEquals(depB.name, "DepB:companion")
      assertEquals(depB.depA.name, "DepA:local")
    }
  }
}
object ExampleTest {
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
}
