package example.di.`implicit`

trait Provider[A] {
  def get: A
}
object Provider {
  given givenToProvider: [A] => (a: A) => Provider[A] {
    override def get: A = a
  }
}



trait Providers[Ts <: NonEmptyTuple]