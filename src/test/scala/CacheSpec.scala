import FutureBool.all
import PredicateChain.finish
import org.mockito.Mockito
import org.mockito.Mockito.{spy, verify}
import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.prop.{TableDrivenPropertyChecks, Tables}
import org.slf4j.Logger

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class CacheSpec extends FunSuite with ScalaFutures {
  val fakeLogger = Mockito.spy(classOf[Logger])
  val engine = new RulesEngine(fakeLogger)
  import Predicate._

  test("Rules can be constructed for fizzbuzz as a table") {
    val scenarios = Tables.Table[Int, String](
      ("param", "fizz or buzz or fizzbuzz or param"),
      (1, "1"),
      (2, "2"),
      (3, "fizz"),
      (4, "4"),
      (5, "buzz"),
      (6, "fizz"),
      (7, "7"),
      (8, "8"),
      (9, "fizz"),
      (10, "buzz"),
      (11, "11"),
      (12, "fizz"),
      (13, "13"),
      (14, "14"),
      (15, "fizzbuzz"),
      (16, "16")
    )

    val fizzBuzz = new FizzBuzz()

    val rules = PredicateChain.of[Int, String, Any](
      all((fizzBuzz.divisibleByThree _), (fizzBuzz.divisibleByFive _)).named("fizzbuzz-rule") -> "fizzbuzz",
      (fizzBuzz.divisibleByThree _).named("fizz") -> "fizz",
      (fizzBuzz.divisibleByFive _).named("bazz") -> "buzz",
      finish[Int, Any] -> Result(n => n.toString())
    )

    TableDrivenPropertyChecks.forAll(scenarios) { (n, expected) =>
      val result = Await.result(engine.loggingAssess(rules)(null)(n), 1.seconds).get
      assert(expected == result)
    }
  }

  test("fizzbuzz rules assessed for 15 call divisibleByThree once and divisibleByFive once") {
    val fizzBuzz = spy(new FizzBuzz())

    val rules = PredicateChain.of[Int, String, Any](
      all((fizzBuzz.divisibleByThree _), (fizzBuzz.divisibleByFive _)).named("fizzbuzz-rule") -> "fizzbuzz",
      (fizzBuzz.divisibleByThree _).named("fizz") -> "fizz",
      (fizzBuzz.divisibleByFive _).named("bazz") -> "buzz",
      finish[Int, Any]  -> Result(n => n.toString())
    )

    val result = Await.result(engine.loggingAssess(rules)(null)(15), 5.seconds).get
    assert(result == "fizzbuzz")
    verify(fizzBuzz).divisibleByThree(15)
    verify(fizzBuzz).divisibleByFive(15)
  }

  test("fizzbuzz rules assessed for 1 call divisibleByThree once and divisibleByFive once to test caching") {
    val fizzBuzz = spy(new FizzBuzz())

    val rules = PredicateChain.of[Int, String, Any](
      all((fizzBuzz.mDivisibleByThree _), (fizzBuzz.mDivisibleByFive _)).named("fizzbuzz-rule") -> "fizzbuzz",
      (fizzBuzz.mDivisibleByThree _).named("fizz") -> "fizz",
      (fizzBuzz.mDivisibleByFive _).named("bazz") -> "buzz",
      finish[Int, Any]  -> Result(n => n.toString())
    )

    val result = Await.result(engine.loggingAssess(rules)(null)(1), 5.seconds).get
    assert(result == "1")
    verify(fizzBuzz).divisibleByThree(1)
    verify(fizzBuzz).divisibleByFive(1)
  }

  test("caching works with multiple params") {
    val fizzBuzz = spy(new FizzBuzz())

    val rules = PredicateChain.of[Int, String, Any](
      all((fizzBuzz.mDivisibleByThreeP _), (fizzBuzz.mDivisibleByFiveP _)).named("fizzbuzz-rule") -> "fizzbuzz",
      (fizzBuzz.mDivisibleByThreeP _).named("fizz") -> "fizz",
      (fizzBuzz.mDivisibleByFiveP _).named("bazz") -> "buzz",
      finish[Int, Any]  -> Result(n => n.toString())
    )

    val result = Await.result(engine.loggingAssess(rules)(null)(1), 30.seconds).get
    assert(result == "1")
    verify(fizzBuzz).divisibleBy(1, 3)
    verify(fizzBuzz).divisibleBy(1, 5)
  }

  test("caching works with less time") {

    val fizzBuzz = spy(new FizzBuzz())

    val rules = PredicateChain.of[Int, String, Any](
      all((fizzBuzz.mDivisibleByThreeP _), (fizzBuzz.mDivisibleByFiveP _)).named("fizzbuzz-rule") -> "fizzbuzz",
      (fizzBuzz.mDivisibleByThreeP _).named("fizz") -> "fizz",
      (fizzBuzz.mDivisibleByFiveP _).named("bazz") -> "buzz",
      finish[Int, Any]  -> Result(n => n.toString())
    )

    def time[R](block: => R): (R, Double) = {
      val start = System.nanoTime()
      val result = block
      val end = System.nanoTime()
      (result, (end - start) / 1e9)
    }

    val (result, secs) = time(Await.result(engine.loggingAssess(rules)(null)(1), 30.seconds).get)
    assert(result == "1")
    assert(secs > 1)

    val (result2, secs2) = time(Await.result(engine.loggingAssess(rules)(null)(1), 30.seconds).get)
    assert(result2 == "1")
    assert(secs2 < 1)
  }

  test("caching re-evaluates with new values") {
    class MemoizedFizzBuzz() {
      val fizzBuzz = spy(new FizzBuzz())

      val rules = PredicateChain.of[Int, String, Any](
        all((fizzBuzz.mDivisibleByThreeP _), (fizzBuzz.mDivisibleByFiveP _)).named("fizzbuzz-rule") -> "fizzbuzz",
        (fizzBuzz.mDivisibleByThreeP _).named("fizz") -> "fizz",
        (fizzBuzz.mDivisibleByFiveP _).named("bazz") -> "buzz",
        finish[Int, Any]  -> Result(n => n.toString())
      )

      def time[R](block: => R): (R, Double) = {
        val start = System.nanoTime()
        val result = block
        val end = System.nanoTime()
        (result, (end - start) / 1e9)
      }

      val (result, secs) = time(Await.result(engine.loggingAssess(rules)(null)(1), 30.seconds).get)
      assert(result == "1")
      assert(secs > 1)

      val (result2, secs2) = time(Await.result(engine.loggingAssess(rules)(null)(2), 30.seconds).get)
      assert(result2 == "2")
      assert(secs2 > 1)

      val (result3, secs3) = time(Await.result(engine.loggingAssess(rules)(null)(1), 30.seconds).get)
      assert(result3 == "1")
      assert(secs3 < 1)
    }
  }



  test("fizzbuzz rules are logged witlh the rule name and value") {
    import Predicate._
    val fakeLogger = Mockito.spy(classOf[Logger])
    val engine = new RulesEngine(fakeLogger)
    val fizzBuzz = spy(new FizzBuzz())
    val world = new FizzBazzEnvirionment {
      override val fizzbazz: FizzBuzz = fizzBuzz
    }
    val rules = PredicateChain.of[Int, String, FizzBazzEnvirionment](
      all(FizzRule, BazzRule).named("fizzbuzz-rule") -> "fizzbuzz",
      FizzRule.named("fizz") -> "fizz",
      BazzRule.named("bazz") -> "buzz",
      finish[Int, FizzBazzEnvirionment]  -> Result(n => n.toString())
    )
    val result = Await.result(engine.envirionmentalAssess(rules)(world)(15), 5.seconds).get
    assert(result == "fizzbuzz")
    verify(fizzBuzz).divisibleByThree(15)
    verify(fizzBuzz).divisibleByFive(15)
    verify(fakeLogger).info("Evaluting predicate fizzbuzz-rule")
    verify(fakeLogger).info("Predicate fizzbuzz-rule has value true")
  }


  test("Use two envirionemts in chain") {
    import Predicate._
    val fakeLogger = Mockito.spy(classOf[Logger])
    val engine = new RulesEngine(fakeLogger)
    val fizzBuzz = spy(new FizzBuzz())
    val primeService = new Prime()
    val world = new FizzBazzEnvirionment with PrimeEnvironment {
      override val fizzbazz: FizzBuzz = fizzBuzz

      override val prime: Prime = primeService
    }
    val rules = PredicateChain.of[Int, String, FizzBazzEnvirionment with PrimeEnvironment](
      all(FizzRule, BazzRule).named("fizzbuzz-rule") -> "fizzbuzz",
      FizzRule.named("fizz") -> "fizz",
      BazzRule.named("bazz") -> "buzz",
      PrimeRule.named("prime") -> "prime",
      finish[Int, FizzBazzEnvirionment with PrimeEnvironment]  -> Result(n => n.toString())
    )
    val result = Await.result(engine.envirionmentalAssess(rules)(world)(7), 5.seconds).get
    assert(result == "prime")
  }
}