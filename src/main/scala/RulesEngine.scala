import org.slf4j.Logger

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.{implicitConversions, postfixOps}

trait Name {
  def name: String
}

trait Predicate[Subject, Environment] {
  type PredicateFunction = Environment => Subject => Future[Boolean]
  def function: PredicateFunction
  def named(name: String): Predicate[Subject, Environment] with Name
  def apply(value: Subject, environment: Environment): Future[Boolean] = function(environment)(value)
}

case class NamedPredicate[Subject, Environment](function: Environment => Subject => Future[Boolean], name: String)
  extends Predicate[Subject, Environment] with Name {

  override def named(name: String): Predicate[Subject, Environment] with Name = copy(name = name)
}

case class AnonymousPredicate[Subject, Environment](function: Environment => Subject => Future[Boolean]) extends Predicate[Subject, Environment] {
  override def named(name: String): Predicate[Subject, Environment] with Name = NamedPredicate(function, name)
}

object Predicate {
  def apply[A, E](fun: E => A => Future[Boolean], name: String): Predicate[A, E] = NamedPredicate(fun, name)

  implicit def apply[A, E](fun: (A, E) => Future[Boolean]): Predicate[A, E] =
    new AnonymousPredicate[A, E]((env: E) => (subject: A) => fun(subject, env))

  implicit def apply[A](fun: A => Future[Boolean]): Predicate[A, Any] = new AnonymousPredicate[A, Any](_ => fun)

  implicit def apply(value: Future[Boolean]): Predicate[Any, Any] = new AnonymousPredicate[Any, Any](_ => _ => value)
}

case class Result[S, R](fun: S => R)

object Result {
  implicit def apply[S, R](value: R): Result[S, R] = new Result((_: S) => value)
  implicit def apply[S, R](fun: S => R): Result[S, R] = new Result(fun)
}

case class ChainEntry[A, R, E](predicate: Predicate[A, _ >: E] with Name, result: Result[A, R])

object ChainEntry {
  def lift[A, R, E](entry: (Predicate[A, _ >: E] with Name, Result[A, R])): ChainEntry[A, R, E] = new ChainEntry[A, R, E](entry._1, entry._2)
}

case class PredicateChain[A, R, E](seq: Seq[ChainEntry[A, R, E]])

object PredicateChain {
//  def of[A, R](first: ChainEntry[A, R], rest: ChainEntry[A, R]*) = new PredicateChain[A, R](rest.+:(first))
  def of[A, R, E](first: (Predicate[A, _ >: E] with Name, Result[A, R]), rest: (Predicate[A, _ >: E] with Name, Result[A, R])*) = {
    val firstLifeted = ChainEntry.lift[A, R, E](first)
    val restLifted = rest.map(ChainEntry.lift[A, R, E])
    new PredicateChain[A, R, E](restLifted.+:(firstLifeted))
  }

  def finish[A, E]: Predicate[A, E] with Name = NamedPredicate[A, E](_ => _ => Future.successful(true), "finish")
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

  def loggingAssess[S, V, E](chain: PredicateChain[S, V, E])(env: E)(value: S): Future[Option[V]] = {
    val rules = chain.seq
    rules.foldLeft(Future(Option.empty[V])) { (accum, curr) =>
      val predicate = curr.predicate
      val outcome = curr.result
      accum flatMap {
        case v@Some(_) => Future(v)
        case None =>
          val predicateName = predicate.name
          log.info(s"Evaluting predicate $predicateName")
          predicate.function(env)(value).map { predicateResult =>
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

  def envirionmentalAssess[S, V, E](chain: PredicateChain[S, V, E])(env: E): S => Future[Option[V]] = {
    val rules = chain.seq
    val application = chain.seq.map(entry => (entry.predicate.function(env), entry.predicate.name, entry.result))
    subject => application.foldLeft(Future(Option.empty[V])) { (accum, curr) =>
      val (fun, name, outcome) = curr
      accum flatMap {
        case v@Some(_) => Future(v)
        case None =>
          log.info(s"Evaluting predicate $name")
          fun(subject).map { predicateResult =>
            log.info(s"Predicate $name has value $predicateResult")
            if (predicateResult) {
              Some(outcome.fun(subject))
            } else {
              None
            }
          }
      }
    }
  }
}

