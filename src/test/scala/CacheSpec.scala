import FutureBool.all
import org.mockito.Mockito
import org.mockito.Mockito.{spy, verify}
import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures
import org.slf4j.Logger

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class CacheSpec extends FunSuite with ScalaFutures {
  val fakeLogger = Mockito.spy(classOf[Logger])
  val engine = new RulesEngine(fakeLogger)

//  test("Rules can be constructed for fizzbuzz as a table") {
//    val scenarios = Tables.Table[Int, String](
//      ("param", "fizz or buzz or fizzbuzz or param"),
//      (1, "1"),
//      (2, "2"),
//      (3, "fizz"),
//      (4, "4"),
//      (5, "buzz"),
//      (6, "fizz"),
//      (7, "7"),
//      (8, "8"),
//      (9, "fizz"),
//      (10, "buzz"),
//      (11, "11"),
//      (12, "fizz"),
//      (13, "13"),
//      (14, "14"),
//      (15, "fizzbuzz"),
//      (16, "16")
//    )
//
//    val fizzBuzz = new FizzBuzz()
//
//    def rules(n: Int) = Seq(
//      (() => all(fizzBuzz.divisibleByThree(n), fizzBuzz.divisibleByFive(n))) -> "fizzbuzz",
//      (() => fizzBuzz.divisibleByThree(n)) -> "fizz",
//      (() => fizzBuzz.divisibleByFive(n)) -> "buzz",
//      (() => Future(true)) -> n.toString()
//    )
//
//    TableDrivenPropertyChecks.forAll(scenarios) { (n, expected) =>
//      val result = Await.result(engine.assess(rules(n)), 5.seconds).get
//      assert(expected == result)
//    }
//  }
//
//  test("fizzbuzz rules assessed for 15 call divisibleByThree once and divisibleByFive once") {
//    val fizzBuzz = spy(new FizzBuzz())
//
//    def rules(n: Int) = Seq(
//      (() => all(fizzBuzz.divisibleByThree(n), fizzBuzz.divisibleByFive(n))) -> "fizzbuzz",
//      (() => fizzBuzz.divisibleByThree(n)) -> "fizz",
//      (() => fizzBuzz.divisibleByFive(n)) -> "buzz",
//      (() => Future(true)) -> n.toString()
//    )
//
//    val result = Await.result(engine.assess(rules(15)), 5.seconds).get
//    assert(result == "fizzbuzz")
//    verify(fizzBuzz).divisibleByThree(15)
//    verify(fizzBuzz).divisibleByFive(15)
//  }
//
//  test("fizzbuzz rules assessed for 1 call divisibleByThree once and divisibleByFive once to test caching") {
//    val fizzBuzz = spy(new FizzBuzz())
//
//    def rules(n: Int) = Seq(
//      (() => all(fizzBuzz.mDivisibleByThree(n), fizzBuzz.mDivisibleByFive(n))) -> "fizzbuzz",
//      (() => fizzBuzz.mDivisibleByThree(n)) -> "fizz",
//      (() => fizzBuzz.mDivisibleByFive(n)) -> "buzz",
//      (() => Future(true)) -> n.toString()
//    )
//
//    val result = Await.result(engine.assess(rules(1)), 5.seconds).get
//    assert(result == "1")
//    verify(fizzBuzz).divisibleByThree(1)
//    verify(fizzBuzz).divisibleByFive(1)
//  }
//
//  test("caching works with multiple params") {
//    val fizzBuzz = spy(new FizzBuzz())
//
//    def rules(n: Int) = Seq(
//      (() => all(fizzBuzz.mDivisibleByThreeP(n), fizzBuzz.mDivisibleByFiveP(n))) -> "fizzbuzz",
//      (() => fizzBuzz.mDivisibleByThreeP(n)) -> "fizz",
//      (() => fizzBuzz.mDivisibleByFiveP(n)) -> "buzz",
//      (() => Future(true)) -> n.toString()
//    )
//
//    val result = Await.result(engine.assess(rules(1)), 30.seconds).get
//    assert(result == "1")
//    verify(fizzBuzz).divisibleBy(1, 3)
//    verify(fizzBuzz).divisibleBy(1, 5)
//  }
//
//  test("caching works with less time") {
//
//    val fizzBuzz = spy(new FizzBuzz())
//
//    def rules(n: Int) = Seq(
//      (() => all(fizzBuzz.mDivisibleByThreeP(n), fizzBuzz.mDivisibleByFiveP(n))) -> "fizzbuzz",
//      (() => fizzBuzz.mDivisibleByThreeP(n)) -> "fizz",
//      (() => fizzBuzz.mDivisibleByFiveP(n)) -> "buzz",
//      (() => Future(true)) -> n.toString()
//    )
//
//    def time[R](block: => R): (R, Double) = {
//      val start = System.nanoTime()
//      val result = block
//      val end = System.nanoTime()
//      (result, (end - start) / 1e9)
//    }
//
//    val (result, secs) = time(Await.result(engine.assess(rules(1)), 30.seconds).get)
//    assert(result == "1")
//    assert(secs > 1)
//
//    val (result2, secs2) = time(Await.result(engine.assess(rules(1)), 30.seconds).get)
//    assert(result2 == "1")
//    assert(secs2 < 1)
//  }
//
//  test("caching re-evaluates with new values") {
//    class MemoizedFizzBuzz() {
//      val fizzBuzz = spy(new FizzBuzz())
//
//      def rules(n: Int) = Seq(
//        (() => all(fizzBuzz.mDivisibleByThreeP(n), fizzBuzz.mDivisibleByFiveP(n))) -> "fizzbuzz",
//        (() => fizzBuzz.mDivisibleByThreeP(n)) -> "fizz",
//        (() => fizzBuzz.mDivisibleByFiveP(n)) -> "buzz",
//        (() => Future(true)) -> n.toString()
//      )
//
//      def time[R](block: => R): (R, Double) = {
//        val start = System.nanoTime()
//        val result = block
//        val end = System.nanoTime()
//        (result, (end - start) / 1e9)
//      }
//
//      val (result, secs) = time(Await.result(engine.assess(rules(1)), 30.seconds).get)
//      assert(result == "1")
//      assert(secs > 1)
//
//      val (result2, secs2) = time(Await.result(engine.assess(rules(2)), 30.seconds).get)
//      assert(result2 == "2")
//      assert(secs2 > 1)
//
//      val (result3, secs3) = time(Await.result(engine.assess(rules(1)), 30.seconds).get)
//      assert(result3 == "1")
//      assert(secs3 < 1)
//    }
//  }



  test("fizzbuzz rules are logged witlh the rule name and value") {
    val fizzBuzz = spy(new FizzBuzz())
    import Predicate._

    val rules = PredicateChain.of[Int, String](
      all(fizzBuzz.divisibleByThree _, fizzBuzz.divisibleByFive _).named("fizzbuzz-predicate") -> "fizzbuzz",
      (fizzBuzz.divisibleByThree _).named("fizz-predicate")-> "fizz",
      (fizzBuzz.divisibleByFive _).named("buzz-predicate") -> "buzz",
      PredicateChain.finish[Int].named("finish") -> "number"
    )
    val result = Await.result(engine.loggingAssess(rules)(15), 5.seconds).get
    assert(result == "fizzbuzz")
    verify(fizzBuzz).divisibleByThree(15)
    verify(fizzBuzz).divisibleByFive(15)
    verify(fakeLogger).info("Evaluting predicate fizzbuzz-predicate")
    verify(fakeLogger).info("Predicate fizzbuzz-predicate has value true")
  }
}