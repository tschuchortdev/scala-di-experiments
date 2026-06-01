package example.di

import munit.FunSuite

class BackedByLazyValTest extends FunSuite {

  test("value should be initialized only once in non-overridden base class") {
    var count = 0
    class BaseClass {
      @BackedByLazyVal
      def value: String = {
        count += 1
        "hello"
      }
    }

    val base = new BaseClass
    assert(count == 0)
    assert(base.value == "hello")
    assert(count == 1)
    assert(base.value == "hello")
    assert(count == 1)
  }

  test("value should be initialized only once when overridden") {
    var baseInitialized = 0
    class BaseClass {
      @BackedByLazyVal
      def value: String = {
        baseInitialized += 1
        "hello"
      }
    }

    var derivedInitialized = 0
    class DerivedClass extends BaseClass {
      override def value: String = {
        derivedInitialized += 1
        super.value ++ " world"
      }
    }

    val derived = new DerivedClass
    assert(baseInitialized == 0)
    assert(derived.value == "hello world")
    assert(baseInitialized == 1)
    assert(derivedInitialized == 1)
    assert(derived.value == "hello world")
    assert(baseInitialized == 1)
    assert(derivedInitialized == 2)
  }

  test("base class value should never be initialized when never accessed through super") {
    var count = 0
    class BaseClass {
      @BackedByLazyVal
      def value: String = {
        count += 1
        "hello"
      }
    }

    class DerivedClass extends BaseClass {
      override def value: String = "hello2"
    }

    val derived = new DerivedClass
    assert(derived.value == "hello2")
    assert(count == 0)
  }

  test("value is cached in a chain of derived classes") {
    var baseInitialized = 0
    class BaseClass {
      @BackedByLazyVal
      def value: String = {
        baseInitialized += 1
        "hello"
      }
    }

    var derived1Initialized = 0
    class Derived1 extends BaseClass {
      @BackedByLazyVal
      override def value: String = {
        derived1Initialized += 1
        super.value ++ " derived1"
      }
    }

    var derived2Initialized = 0
    class Derived2 extends Derived1 {
      @BackedByLazyVal
      override def value: String = {
        derived2Initialized += 1
        super.value ++ " derived2"
      }
    }

    val derived2 = new Derived2
    assert(derived2.value == "hello derived1 derived2")
    assert(baseInitialized == 1)
    assert(derived1Initialized == 1)
    assert(derived2Initialized == 1)

    derived2.value: Unit
    assert(baseInitialized == 1)
    assert(derived1Initialized == 1)
    assert(derived2Initialized == 1)
  }

  test("cache value when multiple mixins are overriding a trait") {
    trait Interface {
      def value: String
    }

    var baseInitialized = 0
    class BaseClass extends Interface {
      //@BackedByLazyVal
      override def value: String = {
        baseInitialized += 1
        "hello"
      }
    }

    /*var mixin1Initialized = 0
    trait Mixin1 extends Interface {
      @BackedByLazyVal
      abstract override def value: String = {
        mixin1Initialized += 1
        super.value ++ " mixin1"
      }
    }*/
    var mixin1Initialized = 0
    trait Mixin1 extends Interface {
      private lazy val value_lazyval: String = {
        mixin1Initialized += 1
        super.value ++ " mixin1"
      }
      abstract override def value: String = this.value_lazyval
    }

    /*var mixin2Initialized = 0
    trait Mixin2 extends Interface {
      @BackedByLazyVal
      abstract override def value: String = {
        mixin2Initialized += 1
        super.value ++ " mixin2"
      }
    }*/

    val derived = new BaseClass with Mixin1

    assert(derived.value == "hello mixin1")
    //assert(baseInitialized == 1)
    assert(mixin1Initialized == 1)
    //assert(mixin2Initialized == 1)

    derived.value: Unit
    //assert(clue(baseInitialized) == 1)
    assert(clue(mixin1Initialized) == 1)
    //assert(mixin2Initialized == 1)
  }

  test("ajfhdkhakjskjhvds") {
    trait Interface {
      def value: String
    }

    var baseInitialized = 0
    class BaseClass extends Interface {
      //@BackedByLazyVal
      override def value: String = {
        baseInitialized += 1
        "hello"
      }
    }
    var mixin1Initialized = 0
    trait Mixin1 extends Interface {
      private lazy val value_lazyval: String = {
        mixin1Initialized += 1
        super.value ++ " mixin1"
      }

      abstract override def value: String = this.value_lazyval
    }

    val derived = new BaseClass with Mixin1

    println(derived.value)
    println(mixin1Initialized == 1)

    derived.value: Unit
    println(mixin1Initialized == 1)
  }
}
