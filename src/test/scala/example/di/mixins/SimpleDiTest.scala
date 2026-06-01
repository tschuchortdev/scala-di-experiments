//noinspection TypeAnnotation
package example.di.mixins

import example.di.*

import cats.effect.syntax.all.*
import cats.effect.{IO, Resource}
import cats.mtl.syntax.all.*
import cats.syntax.all.*
import munit.CatsEffectSuite

import scala.annotation.experimental




trait NestedModule {
  def strings: List[String]
  def usesStrings: String
}

trait Dependencies {
  def config: MyConfig
  def nestedModule: NestedModule
  def db: Database
  def kafkaClient: KafkaClient
  def httpClient: HttpClient
  def fooService: FooService
  def barService: BarService
}

class DependenciesProd extends Dependencies {
  @BackedByLazyVal override def config = new MyConfig(host = "0.0.0.0", xyz = "xyz", declaredBy = "DependenciesProd")
  @BackedByLazyVal override def nestedModule = new NestedModule {
    @BackedByLazyVal override def strings = List("s1")
    override def usesStrings: String = this.strings.mkString
  }
  @BackedByLazyVal override def db = new Database("DependenciesProd")
  @BackedByLazyVal override def kafkaClient = new KafkaClient("DependenciesProd")
  @BackedByLazyVal override def httpClient = new HttpClient("DependenciesProd")
  @BackedByLazyVal override def fooService = new FooService(kafkaClient, db, "DependenciesProd")
  @BackedByLazyVal override def barService = new BarService(httpClient, db, "DependenciesProd")
}

class DependenciesTest extends DependenciesProd {
  @BackedByLazyVal override def config = super.config.copy(host = "127.0.0.1", declaredBy = "DependenciesTest")
  @BackedByLazyVal override def db = new Database("DependenciesTest")
  @BackedByLazyVal override def kafkaClient = new KafkaClient("DependenciesTest")
}

trait TestWithDatabase {}

trait OverwritesNestedModule extends Dependencies {
  @BackedByLazyVal abstract override def nestedModule: NestedModule = new NestedModule {
    private val sup = OverwritesNestedModule.super.nestedModule
    export sup.{strings => _, *}
    override def strings = OverwritesNestedModule.super.nestedModule.strings.s :+ "s2"
  }
}

def testWithHttpClient: Resource[IO, Unit] = ???

class MyTestSuite extends CatsEffectSuite {

  test("regular test")(for {
    _ <- IO(())
  } yield ())

  test("with local fixture")(for {
    _ <- IO(())
  } yield ())
}
