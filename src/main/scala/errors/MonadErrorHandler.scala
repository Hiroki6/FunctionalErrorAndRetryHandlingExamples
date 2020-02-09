package errors

import cats.{ Monad, MonadError }

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

trait MonadHandler {
  import cats.implicits.toFlatMapOps
  import cats.implicits.toFunctorOps
  def program[F[_], E](value: F[Int], e: => E)(implicit M: Monad[F]): F[Int] = for {
    n <- value
    result <- if (n < 0) ???
    else M.pure(n * 2)
  } yield result

}

object EitherErrorHandler {
  def program(value: Either[String, Int], e: => String): Either[String, Int] = for {
    n <- value
    result <- if (n < 0) Left(e)
    else Right(n * 2)
  } yield result

  def main(args: Array[String]): Unit = {
    import cats.syntax.either._
    val resEither = program(43.asRight, "Error")

    println(resEither)
  }
}

object FutureErrorHandler {
  def program(value: Future[Int], e: => String)(implicit ec: ExecutionContext): Future[Int] = for {
    n <- value
    result <- if (n < 0) Future.failed(new RuntimeException(e))
    else Future.successful(n * 2)
  } yield result

  def main(args: Array[String]): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val result = program(Future(43), "failure")

    result onComplete {
      case Success(i) => println(i)
      case Failure(e) => println(e.getMessage)
    }
    Thread.sleep(3000)
  }
}

object TryErrorHandler {
  def program(value: Try[Int], e: => String): Try[Int] = for {
    n <- value
    result <- if (n < 0) Failure(new RuntimeException(e))
    else Success(n * 2)
  } yield result
  def main(args: Array[String]): Unit = {
    val resTry = program(Success(43), "failure")

    println(resTry)
  }
}

object OptionErrorHandler {
  def program(value: Option[Int], e: => String): Option[Int] = for {
    n <- value
    result <- if (n < 0) None
    else Some(n * 2)
  } yield result

  def main(args: Array[String]): Unit = {
    import cats.syntax.option._
    val resOption = program(-86.some, "Error")

    println(resOption)
  }
}

object MonadErrorHandler {
  import cats.implicits.toFlatMapOps
  import cats.implicits.toFunctorOps
  def program[F[_], E](value: F[Int], e: => E)(implicit ME: MonadError[F, E]): F[Int] = for {
    n <- value
    result <- if (n < 0) ME.raiseError(e)
    else ME.pure(n * 2)
  } yield result

  def programAdvanced[F[_], E](value: F[Int], e: => E)(implicit ME: MonadError[F, E]): F[Int] =
    ME.ensure(value)(e)(_ >= 0).map(_ * 2)

  def main(args: Array[String]): Unit = {
    import cats.syntax.either._
    import cats.instances.either._
    println(catsStdInstancesForEither.ensure(Right(42))("Error")(_ > 0)) // Right(42)
    //MonadError[({type E[A] = Either[String, A]})#E, String].ensure(Right(42))("Error")(_ > 0)
    println(catsStdInstancesForEither.ensure(Right(-42))("Error")(_ > 0)) // Left(42)

    type StringEither[A] = Either[String, A]
    val resEitherSuccess = program[({ type E[A] = Either[String, A] })#E, String](43.asRight[String], "Error") // Right(86)
    val resEitherFailure = program[({ type E[A] = Either[String, A] })#E, String](-43.asRight[String], "Error") // Left("Error")
    val resEitherSuccess1 = program[StringEither, String](43.asRight[String], "Error") // Right(86)
    val resEitherFailure1 = program[StringEither, String](-43.asRight[String], "Error") // Left("Error")

    println(resEitherSuccess)
    println(resEitherFailure)

    import scala.concurrent.ExecutionContext.Implicits.global
    import cats.instances.future._
    val resFutureSuccess = program[Future, Throwable](Future(43), new RuntimeException("Error")) // onComplete => Success(43)
    val resFutureFailure = program[Future, Throwable](Future(-43), new RuntimeException("Error")) // onComplete => Failure(RuntimeException("Error"))

    resFutureSuccess onComplete {
      case Success(i) => println(i)
      case Failure(e) => println(e.getMessage)
    }

    resFutureFailure onComplete {
      case Success(i) => println(i)
      case Failure(e) => println(e.getMessage)
    }

    Thread.sleep(3000)

    import cats.instances.try_._
    val resTrySuccess = program[Try, Throwable](Success(43), new RuntimeException("Error")) // Success(43)
    val resTryFailure = program[Try, Throwable](Success(-43), new RuntimeException("Error")) // Failure(RuntimeException("Error"))

    println(resTrySuccess)
    println(resTryFailure)
  }
}

