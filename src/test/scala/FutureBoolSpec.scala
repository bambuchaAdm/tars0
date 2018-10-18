import org.scalatest.FunSuite
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

class FutureBoolSpec extends FunSuite with ScalaFutures {
  def time[R](block: => R):(R, Double) = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    (result, (end - start) / 1e9)
  }

  //All

  test("all returns Future.successful(true) when all arguments are Future.successful(true)") {
    val result = Await.result(FutureBool.all(Future.successful(true), Future.successful(true)), 5 seconds)
    assert(result)
  }

  test("all returns Future.successful(false) when some arguments are Future.successful(false)") {
    val result = Await.result(FutureBool.all(Future.successful(true), Future.successful(true), Future.successful(false)), 5 seconds)
    assert(!result)
  }

  test("all returns Future.successful(false) when all arguments are Future.successful(false)") {
    val result = Await.result(FutureBool.all(Future.successful(false), Future.successful(true), Future.successful(false)), 5 seconds)
    assert(!result)
  }

  test("Failed futures are cause a fail when calculating a true result using all") {
    pending
    val result = intercept[RuntimeException]{
      Await.result(FutureBool.all(Future.failed(new RuntimeException("Failed")), Future.successful(true), Future.successful(true)), 5 seconds)
    }
    assert(result.getMessage == "Fail detected in all when all others were true")
  }

  test("Failed futures are ignored when calculating a false result using all") {
    val result = Await.result(FutureBool.all(Future.failed(new RuntimeException("Failed")), Future.successful(false), Future.successful(true)), 5 seconds)
    assert(!result)
  }

  test ("all returns false fast if there are trues and falses, but trues are delayed") {
    val (result, secs) = time(Await.result(FutureBool.all(Future {Thread.sleep(2000); true}, Future.successful(false)), 5 seconds))
    println("Time was " + secs)
    assert(!result)
    assert(secs < 2)
  }

  test ("all returns true at the rate of the slowest true if they are all trues.") {
    val (result, secs) = time(Await.result(FutureBool.all(Future {Thread.sleep(2000); true}, Future.successful(true)), 5 seconds))
    println("Time was " + secs)
    assert(result)
    assert(secs > 2)
  }

  test ("all returns true at the rate of the slowest true/fail if they are all trues/fails.") {
    val (result, secs) = time(Await.result(FutureBool.all(Future {Thread.sleep(4000); true}, Future.successful(true)), 5 seconds))
    println("Time was " + secs)
    assert(result)
    assert(secs > 3)
  }

  // Any

  test("any returns Future.successful(true) when all arguments are Future.successful(true)") {
    val result = Await.result(FutureBool.any(Future.successful(true), Future.successful(true), Future.successful(true)), 5 seconds)
    assert(result)
  }

  test("any returns Future.successful(true) when some arguments are Future.successful(false)") {
    val result = Await.result(FutureBool.any(Future.successful(true), Future.successful(false), Future.successful(false)), 5 seconds)
    assert(result)
  }

  test("any returns Future.successful(true) when all arguments are Future.successful(false)") {
    val result = Await.result(FutureBool.any(Future.successful(false), Future.successful(false), Future.successful(false)), 5 seconds)
    assert(!result)
  }

  test("Failed futures are ignored when calculating a true result using any") {
    val result = Await.result(FutureBool.any(Future.failed(new RuntimeException("Failed")), Future.successful(true), Future.successful(false)), 5 seconds)
    assert(result)
  }

  test("Failed futures are ignored when calculating a false result using any") {
    val result = Await.result(FutureBool.any(Future.failed(new RuntimeException("Failed")), Future.successful(false), Future.successful(false)), 5 seconds)
    assert(!result)
  }

  test ("any returns true fast if there are trues and falses, but falses are delayed") {
    val (result, secs) = time(Await.result(FutureBool.any(Future {Thread.sleep(2000); false}, Future.successful(true)), 5 seconds))
    println("Time was " + secs)
    assert(result)
    assert(secs < 2)
  }
}
