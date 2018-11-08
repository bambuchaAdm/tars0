import org.slf4j.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.{implicitConversions, postfixOps}

trait Name {
  def name: String
}

trait Predicate[A] {
  def function: A => Future[Boolean]
  def named(name: String): Predicate[A] with Name
  def apply(value: A): Future[Boolean] = function(value)
}

case class NamedPredicate[A](function: A => Future[Boolean], name: String) extends Predicate[A] with Name {
  override def named(name: String): Predicate[A] with Name = copy(name = name)
}

case class AnonymousPredicate[A](function: A => Future[Boolean]) extends Predicate[A] {
  override def named(name: String): Predicate[A] with Name = NamedPredicate(function, name)
}

object Predicate {
  def apply[A](fun: A => Future[Boolean], name: String): Predicate[A] = new NamedPredicate[A](fun, name)

  implicit def apply[A](fun: A => Future[Boolean]): Predicate[A] = new AnonymousPredicate[A](fun)

  implicit def apply(value: => Future[Boolean]): Predicate[Any] = new AnonymousPredicate[Any](_ => value)
}

case class Result[S, R](fun: S => R)

object Result {
  implicit def apply[S, R](value: R): Result[S, R] = new Result((_: S) => value)
  implicit def apply[S, R](fun: S => R): Result[S, R] = new Result(fun)
}

case class ChainEntry[A, R](predicate: Predicate[A] with Name, result: Result[A, R])

object ChainEntry {
  def lift[A, R](entry: (Predicate[A] with Name, Result[A, R])): ChainEntry[A, R] = new ChainEntry[A, R](entry._1, entry._2)
}

case class PredicateChain[A, R](seq: Seq[ChainEntry[A, R]])

object PredicateChain {
//  def of[A, R](first: ChainEntry[A, R], rest: ChainEntry[A, R]*) = new PredicateChain[A, R](rest.+:(first))
  def of[A, R](first: (Predicate[A] with Name, Result[A, R]), rest: (Predicate[A] with Name, Result[A, R])*) = {
    val firstLifeted = ChainEntry.lift(first)
    val restLifted = rest.map(ChainEntry.lift)
    new PredicateChain[A, R](restLifted.+:(firstLifeted))
  }

  def finish[A]: Predicate[A] with Name = NamedPredicate[A]((_: A) => Future.successful(true), "finish")
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
      val predicate = curr.predicate
      val outcome = curr.result
      accum flatMap {
        case v@Some(_) => Future(v)
        case None =>
          val predicateName = predicate.name
          log.info(s"Evaluting predicate $predicateName")
          predicate.function(value).map { predicateResult =>
            log.info(s"Predicate $predicateName has value $predicateResult")
            if (predicateResult) {
              Some(outcome.fun(value))
            } else {
              None
            }
          }
      }
    }
  }
}

