import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps
import org.scalatest.prop.{TableDrivenPropertyChecks, Tables}
import org.mockito.Mockito.{spy, verify, when}
import FutureBool._
import com.softwaremill.macmemo.memoize
import scala.reflect.macros.Context
import scala.language.experimental.macros

class RulesEngineSpec extends FunSuite with ScalaFutures {
  val engine = new RulesEngine

  test("An empty sequence of rules returns a None as a consequence when asseses") {
    val result = Await.result(engine.assess(Seq()), 5 seconds)
    assert(result == None)
  }

  test("A sequence of rules returns the value of the leftmost rule where the precedent evaluates to Future.successful(true) when assessed") {
    val rules = Seq((() => Future(true), "FIRST-TRUE-RULE"), (() => Future(false), "FALSE-RULE"))
    val result = Await.result(engine.assess(rules), 5 seconds)
    assert(result == Some("FIRST-TRUE-RULE"))
  }

  test("A sequence of rules with some precedents evaluating to false at the beginning, skips over them to the first true rule") {
    val rules = Seq(
      (() => Future(false)) -> "FIRST-FALSE-RULE",
      (() => Future(true)) -> "FIRST-TRUE-RULE",
      (() => Future(false)) -> "SECOND-FALSE-RULE"
    )
    val result = Await.result(engine.assess(rules), 5 seconds)
    assert(result == Some("FIRST-TRUE-RULE"))
  }

  test("A sequence of rules with some precedents evaluating to false at the beginning, skips over them to the first true rule, subsequent true rules are ignored") {
    val rules = Seq(
      (() => Future(false)) -> "FIRST-FALSE-RULE",
      (() => Future(true)) -> "FIRST-TRUE-RULE",
      (() => Future(false)) -> "SECOND-FALSE-RULE",
      (() => Future(true)) -> "SECOND-TRUE-RULE"
    )
    val result = Await.result(engine.assess(rules), 5 seconds)
    assert(result == Some("FIRST-TRUE-RULE"))
  }

  test("A sequence of rules with all precedents evaluating to false returns None") {
    val rules = Seq(
      (() => Future(false)) -> "FIRST-FALSE-RULE",
      (() => Future(false)) -> "SECOND-FALSE-RULE",
      (() => Future(false)) -> "THIRD-FALSE-RULE",
      (() => Future(false)) -> "FOURTH-FALSE-RULE"
    )
    val result = Await.result(engine.assess(rules), 5 seconds)
    assert(result == None)
  }

  test("A sequence of rules with an early true followed by a failure returns the value associated with the true") {
    val rules = Seq(
      (() => Future(false)) -> "FIRST-FALSE-RULE",
      (() => Future(true)) -> "FIRST-TRUE-RULE",
      (() => Future.failed(new RuntimeException("fail"))) -> "FAIL",
      (() => Future(false)) -> "THIRD-FALSE-RULE"
    )
    val result = Await.result(engine.assess(rules), 5 seconds)
    assert(result == Some("FIRST-TRUE-RULE"))
  }

  test("A sequence of rules with an early fail followed by a true returns the failure") {
    val rules = Seq(
      (() => Future(false)) -> "FIRST-FALSE-RULE",
      (() => Future.failed(new RuntimeException("fail"))) -> "FAIL",
      (() => Future(true)) -> "FIRST-TRUE-RULE",
      (() => Future(false)) -> "THIRD-FALSE-RULE"
    )
    val result = intercept[RuntimeException] {
      Await.result(engine.assess(rules), 5 seconds)
    }
    assert(result.getMessage == "fail")
  }

  test("A sequence of rules with an early fail followed another fail followed by a true returns the first failure") {
    val rules = Seq(
      (() => Future(false)) -> "FIRST-FALSE-RULE",
      (() => Future.failed(new RuntimeException("fail0"))) -> "FAIL0",
      (() => Future.failed(new RuntimeException("fail1"))) -> "FAIL1",
      (() => Future(true)) -> "FIRST-TRUE-RULE",
      (() => Future(false)) -> "THIRD-FALSE-RULE"
    )
    val result = intercept[RuntimeException] {
      Await.result(engine.assess(rules), 5 seconds)
    }
    assert(result.getMessage == "fail0")
  }

  test("Finding an earlier true rule prevents execution of later precedents") {}

  //Fizbuzz example

  class FizzBuzz() {
    def divisibleByThree(n: Int): Future[Boolean] = Future(n % 3 == 0)

    def divisibleByFive(n: Int): Future[Boolean] = Future(n % 5 == 0)
  }

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
    def rules(n: Int) = Seq(
      (() => all(fizzBuzz.divisibleByThree(n), fizzBuzz.divisibleByFive(n))) -> "fizzbuzz",
      (() => fizzBuzz.divisibleByThree(n)) -> "fizz",
      (() => fizzBuzz.divisibleByFive(n)) -> "buzz",
      (() => Future(true)) -> n.toString()
    )

    TableDrivenPropertyChecks.forAll(scenarios) {(n, expected) =>
      val result = Await.result(engine.assess(rules(n)), 5 seconds).get
      assert(expected == result)
    }
  }

  test("fizzbuzz rules assessed for 15 call divisibleByThree once and divisibleByFive once"){
    val fizzBuzz = spy(new FizzBuzz())
    def rules(n: Int) = Seq(
      (() => all(fizzBuzz.divisibleByThree(n), fizzBuzz.divisibleByFive(n))) -> "fizzbuzz",
      (() => fizzBuzz.divisibleByThree(n)) -> "fizz",
      (() => fizzBuzz.divisibleByFive(n)) -> "buzz",
      (() => Future(true)) -> n.toString()
    )
    val result = Await.result(engine.assess(rules(15)), 5 seconds).get
    assert(result == "fizzbuzz")
    verify(fizzBuzz).divisibleByThree(15)
    verify(fizzBuzz).divisibleByFive(15)
  }

  test("fizzbuzz rules assessed for 1 call divisibleByThree once and divisibleByFive once to test caching"){
    class MemoizedFizzBuzz() {

      def divisibleByThree(n: Int): Future[Boolean] = Future(n % 3 == 0)

      def divisibleByFive(n: Int): Future[Boolean] = Future(n % 5 == 0)

      @memoize(2000, 2 hours)
      def mDivisibleByThree(n: Int): Future[Boolean] = divisibleByThree(n)

      @memoize(maxSize = 2000, expiresAfter = 2 hours)
      def mDivisibleByFive(n: Int): Future[Boolean] = divisibleByFive(n)
    }

    val fizzBuzz = spy(new MemoizedFizzBuzz())

    def rules(n: Int) = Seq(
      (() => all(fizzBuzz.mDivisibleByThree(n), fizzBuzz.mDivisibleByFive(n))) -> "fizzbuzz",
      (() => fizzBuzz.mDivisibleByThree(n)) -> "fizz",
      (() => fizzBuzz.mDivisibleByFive(n)) -> "buzz",
      (() => Future(true)) -> n.toString()
    )

    val result = Await.result(engine.assess(rules(1)), 5 seconds).get
    assert(result == "1")
    verify(fizzBuzz).divisibleByThree(1)
    verify(fizzBuzz).divisibleByFive(1)
  }

  test("caching works with multiple params"){
    class MemoizedFizzBuzz() {

      def divisibleBy(n: Int, d: Int): Future[Boolean] = {Thread.sleep(5 * 1000); Future(n % d == 0)}

      @memoize(2000, 100 second)
      def mDivisibleByThree(n: Int): Future[Boolean] = divisibleBy(n, 3)

      @memoize(maxSize = 2000, expiresAfter = 100 second)
      def mDivisibleByFive(n: Int): Future[Boolean] = divisibleBy(n, 5)
    }

    val fizzBuzz = spy(new MemoizedFizzBuzz())

    def rules(n: Int) = Seq(
      (() => all(fizzBuzz.mDivisibleByThree(n), fizzBuzz.mDivisibleByFive(n))) -> "fizzbuzz",
      (() => fizzBuzz.mDivisibleByThree(n)) -> "fizz",
      (() => fizzBuzz.mDivisibleByFive(n)) -> "buzz",
      (() => Future(true)) -> n.toString()
    )

    val result = Await.result(engine.assess(rules(1)), 30 seconds).get
    assert(result == "1")
    verify(fizzBuzz).divisibleBy(1, 3)
    verify(fizzBuzz).divisibleBy(1, 5)
  }

}
