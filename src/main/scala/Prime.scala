import scala.concurrent.Future

class Prime {
  def isPrime(x: Int) = {
    Future.successful(
      Iterator.from(2).takeWhile(v => v*v < x).forall(y => x % y != 0)
    )
  }
}

trait PrimeEnvironment {
  def prime: Prime
}

object PrimeRule extends Predicate[Int, PrimeEnvironment] {
  override def function: PrimeRule.PredicateFunction = environment => subject => {
    environment.prime.isPrime(subject)
  }

  override def named(name: String): Predicate[Int, PrimeEnvironment] with Name = NamedPredicate(function, name)
}
