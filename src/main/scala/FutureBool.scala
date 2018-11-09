import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import scala.collection.mutable
import scala.concurrent.Future.successful
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object FutureBool extends App {
  def hasFailures[T](futuresBuffer: Traversable[Future[T]]): Boolean =
    futuresBuffer.exists(_.value.fold(false) {xxx: Try[T] => xxx.isFailure})

  /** Asynchronously and non-blockingly returns a `Future` that will hold the optional result
    * of the first `Future` with a result that matches the predicate.
    *
    * @tparam T the type of the value in the future
    * @param futuresBuffer the `mutable sequence` of Futures to search
    * @param p       the predicate which indicates if it's a match
    * @return the `Future` holding the optional result of the search
    */
  def find[T](futuresBuffer: Traversable[Future[T]])(p: T => Boolean)(implicit executor: ExecutionContext) = {
    if (futuresBuffer.isEmpty) successful[Option[T]](None)
    else {
      val result = Promise[Option[T]]()
      val ref = new AtomicInteger(futuresBuffer.size)
      val search: Try[T] => Unit = v => try {
        v match {
          case Success(r) if p(r) => result tryComplete Success(Some(r))
          case _ =>
        }
      } finally {
        if (ref.decrementAndGet == 0) {
          result tryComplete Success(None)
        }
      }

      futuresBuffer.foreach(_ onComplete search)

      result.future
    }
  }

  def all[A, E](futures: Predicate[A, E]*)(implicit ec: ExecutionContext): Predicate[A, E] = Predicate(
    (value: A, environment: E) => {
      val buffer = futures.map(predicate => predicate.function(environment)(value))
      find(buffer) { result => !result } flatMap {_.fold(if (hasFailures(buffer)) Future.failed(new RuntimeException("Fail detected in all when all others were true")) else Future(true)) {_ => Future(false)}}
    }
  )

  def any[A, E](futures: Predicate[A, E]*)(implicit ec: ExecutionContext): Predicate[A, E] = Predicate(
    (value: A, environment: E) => {
    val buffer = futures.map(predicate => predicate.function(environment)(value))
    find(buffer) {identity} flatMap {_.fold(if (hasFailures(buffer)) Future.failed(new RuntimeException("Fail detected in any when all others were false")) else Future.successful(false)) {_ => Future(true)}}
  })

  def not[A, E](fb: Predicate[A, E])(implicit ec: ExecutionContext): Predicate[A, E] = {
    Predicate((subject: A, environment: E) => fb.function(environment)(subject).map(!_))
  }
}
