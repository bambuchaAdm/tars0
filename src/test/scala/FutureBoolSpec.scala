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

  //Found Fails
  test("A mutable.Seq of Future.successful(true) returns false because it has no failures") {
    val buffer = Seq(Future.successful(true), Future.successful(true), Future.successful(true)).toBuffer
    val result = FutureBool.hasFailures(buffer)
    assert(!result)
  }

  test("A mutable.Seq of Future.successful(false) returns false because it has no failures") {
    val buffer = Seq(Future.successful(false), Future.successful(false), Future.successful(false)).toBuffer
    val result = FutureBool.hasFailures(buffer)
    assert(!result)
  }

  test("A mutable.Seq of a mix of Future.successful(true) and Future.successful(false) returns false because it has no failures") {
    val buffer = Seq(Future.successful(false), Future.successful(false), Future.successful(false)).toBuffer
    val result = FutureBool.hasFailures(buffer)
    assert(!result)
  }

  test("An mutable.Seq returns false because it has no failures") {
    val buffer = Seq[Future[Boolean]]().toBuffer
    val result = FutureBool.hasFailures(buffer)
    assert(!result)
  }

  test("A mutable Seq with Future.sucessful true, falses, and fails returns a true because it fails in it.") {
    val buffer = Seq(Future.successful(false), Future.successful(false), Future.failed(new RuntimeException)).toBuffer
    val result = FutureBool.hasFailures(buffer)
    assert(result)
  }

  //All

  test("all returns Future.successful(true) when all arguments are Future.successful(true)") {
    val allFunction = FutureBool.all(Future.successful(true), Future.successful(true))
    val result = Await.result(allFunction(null), 5 seconds)
    assert(result)
  }

  test("all returns Future.successful(false) when some arguments are Future.successful(false)") {
    val allFunction = FutureBool.all(Future.successful(true), Future.successful(true), Future.successful(false))
    val result = Await.result(allFunction(null), 5 seconds)
    assert(!result)
  }

  test("all returns Future.successful(false) when all arguments are Future.successful(false)") {
    val allFunction = FutureBool.all(Future.successful(false), Future.successful(true), Future.successful(false))
    val result = Await.result(allFunction(null), 5 seconds)
    assert(!result)
  }

  test("Failed futures are cause a fail when calculating a true result using all") {
    val result = intercept[RuntimeException]{
      val allFunction = FutureBool.all(Future.failed(new RuntimeException("Failed")), Future.successful(true), Future.successful(true))
      Await.result(allFunction(null), 5 seconds)
    }
    assert(result.getMessage == "Fail detected in all when all others were true")
  }

  test("Failed futures are ignored when calculating a false result using all") {
    val allFunction = FutureBool.all(Future.failed(new RuntimeException("Failed")), Future.successful(false), Future.successful(true))
    val result = Await.result(allFunction(null), 5 seconds)
    assert(!result)
  }

  test ("all returns false fast if there are trues and falses, but trues are delayed") {
    val allFunction = FutureBool.all(Future {
      Thread.sleep(2000); true
    }, Future.successful(false))
    val (result, secs) = time(Await.result(allFunction(null), 5 seconds))
    println("Time was " + secs)
    assert(!result)
    assert(secs < 2)
  }

  test ("all returns true at the rate of the slowest true if they are all trues.") {
    val allFunction = FutureBool.all(Future {
      Thread.sleep(2000); true
    }, Future.successful(true))
    val (result, secs) = time(Await.result(allFunction(null), 5 seconds))
    println("Time was " + secs)
    assert(result)
    assert(secs > 2)
  }

  test ("all returns true at the rate of the slowest true/fail if they are all trues/fails.") {
    val allFunction = FutureBool.all(Future {
      Thread.sleep(4000); true
    }, Future.successful(true))
    val (result, secs) = time(Await.result(allFunction(null), 5 seconds))
    println("Time was " + secs)
    assert(result)
    assert(secs > 3)
  }

  // Any

  test("any returns Future.successful(true) when all arguments are Future.successful(true)") {
    val anyFunction = FutureBool.any(Future.successful(true), Future.successful(true), Future.successful(true))
    val result = Await.result(anyFunction(null), 5 seconds)
    assert(result)
  }

  test("any returns Future.successful(true) when some arguments are Future.successful(false)") {
    val anyFunction = FutureBool.any(Future.successful(true), Future.successful(false), Future.successful(false))
    val result = Await.result(anyFunction(null), 5 seconds)
    assert(result)
  }

  test("any returns Future.successful(true) when all arguments are Future.successful(false)") {
    val anyFunction = FutureBool.any(Future.successful(false), Future.successful(false), Future.successful(false))
    val result = Await.result(anyFunction(null), 5 seconds)
    assert(!result)
  }

  test("Failed futures are ignored when calculating a true result using any") {
    val anyFunction = FutureBool.any(Future.failed(new RuntimeException("Failed")), Future.successful(true), Future.successful(false))
    val result = Await.result(anyFunction(null), 5 seconds)
    assert(result)
  }

  test("Failed futures are cause a fail when calculating a false result using any") {
    val result = intercept[RuntimeException]{
      val anyFunction = FutureBool.any(Future.failed(new RuntimeException("Failed")), Future.successful(false), Future.successful(false))
      Await.result(anyFunction(null), 5 seconds)
    }
    assert(result.getMessage == "Fail detected in any when all others were false")
  }

  test ("any returns true fast if there are trues and falses, but falses are delayed") {
    val anyFunction = FutureBool.any(Future {
      Thread.sleep(2000); false
    }, Future.successful(true))
    val (result, secs) = time(Await.result(anyFunction(null), 5 seconds))
    println("Time was " + secs)
    assert(result)
    assert(secs < 2)
  }

  // Not

  test ("not true is false") {
    val notFunction = FutureBool.not(Future.successful(true))
    val result = Await.result(notFunction(null), 5 seconds)
    assert(!result)
  }

  test ("not false is true") {
    val notFunction = FutureBool.not(Future.successful(false))
    val result = Await.result(notFunction(null), 5 seconds)
    assert(result)
  }

  test("not Future failed is Future failed") {
    val result = intercept[RuntimeException] {
      val notFunction = FutureBool.not(Future.failed(new RuntimeException("Failed")))
      Await.result(notFunction(null), 5 seconds)
    }
    assert(result.getMessage == "Failed")
  }

  //Combos

  test("Combinations of all/any/not work as expected 0") {
    val function = FutureBool.not(FutureBool.all(Future(true), Future(true), FutureBool.any(Future(false), Future(true))))
    val result = Await.result(function(null), 5 seconds)
    assert(!result)
  }

  test("Combinations of all/any/not work as expected 1") {
    val function = FutureBool.not(FutureBool.all(Future(true), Future(true), FutureBool.any(Future(false), Future(false))))
    val result = Await.result(function(null), 5 seconds)
    assert(result)
  }

  test("Combinations of all/any/not work as expected 2") {
    val result = intercept[RuntimeException] {
      val function = FutureBool.not(FutureBool.all(Future(true), Future(true), FutureBool.any(Future(false), Future(false), Future.failed(new RuntimeException))))
      Await.result(function(null), 5 seconds)
    }
    assert(result.getMessage == "Fail detected in all when all others were true")
  }

  test("Combinations of all/any/not work as expected 3") {
    val function = FutureBool.all(Future(true), Future(false), FutureBool.any(Future.failed(new RuntimeException), Future(false)))
    val result = Await.result(function(null), 5 seconds)
    assert(!result)
  }
}
