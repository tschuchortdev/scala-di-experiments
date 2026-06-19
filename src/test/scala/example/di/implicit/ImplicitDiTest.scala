package example.di.`implicit`

import munit.FunSuite

import scala.compiletime.summonInline

class ImplicitDiTest extends FunSuite {

}

object ImplicitDiTest {
  class DepA() {
    
  }
  
  class DepB(i: Int)(using DepA) {
    
  }
  object DepB {
    def apply(using DepA): DepB = new DepB(123)
    
    inline given provider: Provider[DepB] = {
      apply(using summonInline[DepA])
    }
  }
  
  class DepC(j: Int)(using DepA, DepB) {
    
  }
}