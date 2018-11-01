import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

class RulesEngine {
  def assess[V](rules: Seq[(() => Future[Boolean], V)]): Future[Option[V]] = {
    rules.foldLeft(Future(Option.empty[V])){(accum, curr) =>
      accum flatMap {
        case v@Some(_) => Future(v)
        case None => curr._1().map {if (_) Some(curr._2) else None}
      }
    }
  }
}
