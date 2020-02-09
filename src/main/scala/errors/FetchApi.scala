package errors

import java.util.concurrent.Executors

import cats.{ Monad, MonadError }

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

object FetchApi {
  case class URL(value: String)
  case class Value(value: String)
  def fetchTry(url: URL): Try[Value] = {
    val success = true
    val result = Value("result")
    if (success) Success(result)
    else Failure(new Exception("fetch error"))
  }
  def fetchEither(url: URL): Either[Throwable, Value] = {
    val success = true
    val result = Value("result")
    if (success) Right(result)
    else Left(new Exception("fetch error"))
  }
  def fetchFuture(url: URL)(implicit ex: ExecutionContext): Future[Value] = Future {
    val success = true
    val result = Value("result")
    if (success) result
    else throw new Exception("fetch error")
  }

  /**
   * def fetch[F[_]](url: String)(implicit M: Monad[F]): F[Value] = {
   * val success = true
   * val result = Value("result")
   * if(success) M.pure(result)
   * else ???
   * }
   */

  def fetch[F[_]](url: URL)(implicit ME: MonadError[F, Throwable]): F[Value] = {
    val success = true
    val result = Value("result")
    if (success) ME.pure(result)
    else ME.raiseError(new Exception("fetch error"))
  }

  def fetchAdvanced[F[_]](url: URL)(implicit ME: MonadError[F, Throwable]): F[Value] = {
    val success = true
    val result = Value("result")
    ME.ensure(ME.pure(result))(new Exception("fetch error"))(_ => success)
  }

  def main(args: Array[String]): Unit = {
    import cats.instances.either._
    val url = URL("http://example.com")
    type EitherThrowable[A] = Either[Throwable, A]
    fetch[EitherThrowable](url)

    implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
    import cats.instances.future._
    fetch[Future](url)
  }
}