import cats.syntax.all.*
import cats.effect.syntax.all.*
import munit.CatsEffectSuite

class FooTest extends CatsEffectSuite {
  identity({
    test("foo test") {}

  })

}
