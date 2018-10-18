import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}

import scala.concurrent.Future.successful
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

object FutureBool extends App {
  /** Asynchronously and non-blockingly returns a `Future` that will hold the optional result
    *  of the first `Future` with a result that matches the predicate.
    *
    * @tparam T        the type of the value in the future
    * @param futures   the `TraversableOnce` of Futures to search
    * @param p         the predicate which indicates if it's a match
    * @return          the `Future` holding the optional result of the search
    */
  def find[T](futures: TraversableOnce[Future[T]])(p: T => Boolean)(implicit executor: ExecutionContext): Future[Option[T]] = {
    val futuresBuffer = futures.toBuffer
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

  def ffind[T](futures: TraversableOnce[Future[T]])(p: T => Boolean)(implicit executor: ExecutionContext) = {
    val futuresBuffer = futures.toBuffer
    if (futuresBuffer.isEmpty) (successful[Option[T]](None), false)
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

      val foundFails = futuresBuffer.exists(_.value match {
        case Some(Success(x)) => false
        case Some(Failure(e)) => true
        case None => false
      })

      (result.future, foundFails)
    }
  }


  def all(futures: Future[Boolean]*)(implicit ec: ExecutionContext) = {
    find(futures) { !_ } map {_.isEmpty}
//    val result: (Future[Option[Boolean]], Boolean) = ffind(futures) {!_ }
//    result._1 flatMap {
//      case Some(f) => Future.successful(false)
//      case None =>
//        if (result._2) Future.failed(new RuntimeException("Fail detected in all when all others were true")) else Future.successful(true)
//    }
  }

  def any(futures: Future[Boolean]*)(implicit ec: ExecutionContext) = {
    find(futures){ b => b} map {_.isDefined}
  }

}
