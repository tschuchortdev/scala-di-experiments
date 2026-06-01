package example.di.hkd

import _root_.com.tschuchort.hkd.*
import cats.effect.{Concurrent, IO, Ref}
import cats.effect.kernel.{Async, Resource}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import example.di.HttpClient

import scala.collection.mutable
import scala.quoted.{Expr, Type}

@main
def main(): Unit =
  println("hello world")

/*
case class Config(username: String, password: String)

case class Request(requestId: String)

case class NestedModule[H[_]](
    httpClient: H[HttpClient]
                             )

case class MainDependencies[H[_]](
    config: H[Config],
    nestedModule: H[NestedModule[H]],
    requestScoped: H[Request => RequestScopedDependencies[H]]
)

case class RequestScopedDependencies[H[_]]()

val mainDependencies = new MainDependencies[ConstructWith[MainDependencies, IO]](
  config = Singleton { Resource.pure(Config("user", "password")) },
  nestedModule = Singleton {
    new NestedModule[ConstructWith[NestedModule, IO]](
      httpClient = Singleton {
        Resource.pure(HttpClient("mainDependencies.nestedModule"))
      }
    )
  },
  requestScoped = Factory {
    Resource.pure((request: Request) => new RequestScopedDependencies[ConstructWith[RequestScopedDependencies, IO]]())
  },
)

class HasSelf[A](val self: A) extends AnyVal
def self[A](using hs: HasSelf[A]): A = hs.self
class HasSuper[A](val _super: A) extends AnyVal
def _super[A](using hs: HasSuper[A]): A = hs._super

sealed trait Constructor[A]
class Singleton[A](a: A) extends Constructor[A]
class EagerSingleton[A](a: A) extends Constructor[A]
class Factory[A](a: A) extends Constructor[A]

type ConstructWith[Deps[_[_]], F[_]] = [A] =>> Constructor[HasSelf[Deps[F]] ?=> Resource[F, A]]
type ConstructWithSuper[Deps[_[_]], F[_]] = [A] =>> Constructor[(HasSelf[Deps[F]], HasSuper[Deps[F]]) ?=> Resource[F, A]]


def overrideDeps[Deps[_[_]]: {FunctorK, ApplyK}, F[_]](base: Deps[ConstructWith[Deps, F]])(
  overrides: Deps[ConstructWithSuper[Deps, F]]
): Deps[ConstructWith[Deps, F]] = {
  /*base.zipWithK(overrides)([A] => (baseDef: ConstructWith[Deps, F][A], overrideDef: ConstructWithSuper[Deps, F][A]) =>
      (hasSelf: HasSelf[Deps[F]]) ?=>
        overrideDef(using hasSelf, new HasSuper[Deps[F]] { def _super = baseDef(using hasSelf) } )
  )*/

  overrides.mapK([A] => (overrideDef: ConstructWithSuper[Deps, F][A]) =>
    (hasSelf: HasSelf[Deps[F]]) ?=> {
      val baseWired: Deps[F] = base.mapK([B] => (baseDef: ConstructWith[Deps, F][B]) =>
        baseDef(using hasSelf)
      )

      overrideDef(using hasSelf, new HasSuper(baseWired))
    }
  )

  /*val overridesArg: Deps[ConstructWithSuper[Deps, F]] =
    base.mapK([A] => (baseDef: ConstructWith[Deps, F][A]) =>
      (hasSelf: HasSelf[Deps[F]], hasSuper: HasSuper[Deps[F]]) ?=> baseDef(using hasSelf)
    )*/

  ???
}

def wireDeps[Deps[_[_]]: TraverseK, F[_]: Concurrent](depsUnwired: Deps[ConstructWith[Deps, F]]): Resource[F, Deps[F]] =
  Resource[F, Deps[F]](for {
    finalizers <- Ref[F].of(new mutable.Stack[IO[Unit]]())
    depsKnotDeferred <- Concurrent[F].deferred[Deps[F]]
    // mapK wires the resource fields by tying the knot
    depsWired: Deps[[A] =>> Resource[F, Resource[F, A]]] =
      depsUnwired.mapK([A] => (fieldDef: ConstructWith[Deps, F][A]) =>
          depsKnotDeferred.get.toResource.flatMap { depsKnot => fieldDef(using new HasSelf(depsKnot))  }
      )
    // traverseK pulls out the field memoization effect
    depsMemoized <- depsWired.sequenceK
    _ <- depsKnotDeferred.complete(depsMemoized)
    finalizeAll = finalizers.get.flatMap(_.toSeq.sequence_)
  } yield (deps, finalizeAll))


def foo() = {
  overrideDeps(mainDependencies)(_.copy(
    config = Resource.pure(_super.config.map(_.copy(username = "user2")))
  ))
}
*/
