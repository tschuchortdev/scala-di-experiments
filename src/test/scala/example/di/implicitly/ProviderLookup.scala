package example.di.implicitly

/** Call this method to manually summon an instance of [[A]] from the [[Provider]] dependency graph. */
def provide[A](using pl: ProviderLookup[A]): A = pl.p.get

/** The [[ProviderLookup]] type exists solely to provide a level of indirection so the implicit prioritization can be
 * controlled. We want a locally defined `given A` to have higher priority than a `given Provider[A]` defined in `A`'s
 * companion object.
 *
 *  See [[https://github.com/rssh/notes/blob/570b62218ebee01e706a74289225773bc1621665/2025_12_01_implicit_search_priority.md]] */
case class ProviderLookup[A](p: Provider[A])
trait ProviderLookupLowPriorityImplicits {
  given l: [A] => (p: Provider[A]) => ProviderLookup[A] = ProviderLookup(p)
}
object ProviderLookup extends ProviderLookupLowPriorityImplicits {
  inline given ofGiven: [A] => (a: A) => ProviderLookup[A] = ${ ProviderMacro.ofGivenImpl[A]() }
}