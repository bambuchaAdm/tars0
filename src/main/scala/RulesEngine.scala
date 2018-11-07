import org.slf4j.Logger
import scala.language.implicitConversions

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

case class Predicate[A](function: A => Future[Boolean], name: Option[String]) {
  def this(function: A => Future[Boolean]) = this(function, None)

  def named(name: String) = copy(name = Option(name))

  def inverse: Predicate[A] = copy(function = function.andThen(_.map(!_)))

  def apply(value: A): Future[Boolean] = function(value)
}

object Predicate {
  def apply[A](fun: A => Future[Boolean], name: String): Predicate[A] = new Predicate(fun, Some(name))

  implicit def apply[A](source: (String, A => Future[Boolean])): Predicate[A] = new Predicate[A](source._2, Some(source._1))

  implicit def apply[A](fun: A => Future[Boolean]): Predicate[A] = new Predicate[A](fun, None)

  implicit def apply(value: => Future[Boolean]): Predicate[Any] = new Predicate[Any](_ => value, None)
}

case class PredicateChain[A, R](seq: Seq[(Predicate[A], R)])

object PredicateChain {
  def of[A, R](first: (Predicate[A], R), rest: (Predicate[A], R)*) = new PredicateChain[A, R](rest.+:(first))

  def finish[A]: Predicate[A] = Predicate[A]((_: A) => Future.successful(true))
}


class RulesEngine(log: Logger) {

  def assess[V](rules: Seq[(() => Future[Boolean], V)]): Future[Option[V]] = {
    rules.foldLeft(Future(Option.empty[V])) { (accum, curr) =>
      accum flatMap {
        case v@Some(_) => Future(v)
        case None => curr._1().map {
          if (_) Some(curr._2) else None
        }
      }
    }
  }

  def loggingAssess[S, V](chain: PredicateChain[S, V])(value: S): Future[Option[V]] = {
    val rules = chain.seq
    rules.foldLeft(Future(Option.empty[V])) { (accum, curr) =>
      val (predicate, outcome) = curr
      accum flatMap {
        case v@Some(_) => Future(v)
        case None =>
          val predicateName = predicate.name.getOrElse("'unamed'")
          log.info(s"Evaluting predicate $predicateName")
          predicate.function(value).map { predicateResult =>
            log.info(s"Predicate $predicateName has value $predicateResult")
            if (predicateResult) {
              Some(outcome)
            } else {
              None
            }
          }
      }
    }
  }
}
