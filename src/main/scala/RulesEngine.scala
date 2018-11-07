import org.slf4j.Logger
import scala.language.implicitConversions

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

case class Predicate[A](function: A => Future[Boolean], name: String)

object Predicate {
  def apply[A](fun: A => Future[Boolean], name: String): Predicate[A] = new Predicate(fun, name)

  implicit def apply[A](source: (String, A => Future[Boolean])): Predicate[A] = new Predicate[A](source._2, source._1)
}

case class PredicateChain[A, R](seq: Seq[(Predicate[A], R)])

object PredicateChain {
  def of[A, R](first: (Predicate[A], R), rest: (Predicate[A], R)*) = new PredicateChain[A, R](rest.+:(first))
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

  def loggingAssess[S, V](rules: Seq[(Predicate[S], V)])(value: S): Future[Option[V]] = {
    rules.foldLeft(Future(Option.empty[V])) { (accum, curr) =>
      val (predicate, outcome) = curr
      accum flatMap {
        case v@Some(_) => Future(v)
        case None =>
          log.info(s"Evaluting predicate ${predicate.name}")
          predicate.function(value).map { predicateResult =>
            log.info(s"Predicate ${predicate.name} has value $predicateResult")
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
