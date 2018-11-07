import org.slf4j.Logger

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

case class Predicate[A](function: A => Future[Boolean], name: String)

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
