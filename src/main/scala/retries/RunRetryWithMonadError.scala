package retries

import cats.effect.IO
import cats.MonadError
import cats.syntax.applicativeError._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import Common.execute

object RunRetryWithMonadError {
  val logger = new Logger()

  def retryFunc[F[_], A](action: String)(retryPolicy: RetryPolicy)(execution: => F[A])(implicit monadError: MonadError[F, Throwable]): F[A] = {
    def retry(execution: => F[A], cumulativeDelay: FiniteDuration = 0.second, counter: Int = 0): F[A] = {
      execution.handleErrorWith {
        t =>
          if (counter >= retryPolicy.maxRetry) {
            // logging
            logger.info(s"Giving up on $action because of ${t.getMessage}, after ${retryPolicy.maxRetry} retries")
            monadError.raiseError(t)
          } else {
            val delay = retryPolicy.calcDelay(counter)
            // logging
            logger.info(s"Error has occurred on $action because of ${t.getMessage}, retry[counter=$counter] after $delay [ms] sleeping...")
            // sleep for a few times before retry
            Thread.sleep(delay.toMillis)
            retry(execution, cumulativeDelay + delay, counter + 1)
          }
      }
    }
    retry(execution)
  }

  sealed trait RetryPolicy {
    val maxRetry: Int
    def calcDelay(retryCountSoFar: Int): FiniteDuration
  }

  case class ConstantDelay(maxRetry: Int, delay: FiniteDuration) extends RetryPolicy {
    def calcDelay(retryCountSoFar: Int): FiniteDuration = delay
  }

  case class ExponentialBackOff(maxRetry: Int, baseDelay: FiniteDuration) extends RetryPolicy {
    private var lastDelay = baseDelay
    def calcDelay(retryCountSoFar: Int): FiniteDuration = {
      val delay = lastDelay * 2
      lastDelay = delay
      delay
    }
  }

  def programWithConstantDelay(input: Input): IO[Output] =
    retryFunc[IO, Output]("programWithConstantDelay")(ConstantDelay(3, 1.second))(execute(input))

  def programWithExponentialBackOff(input: Input): IO[Output] =
    retryFunc[IO, Output]("programWithExponentialBackOff")(ExponentialBackOff(3, 1.second))(execute(input))

  def main(args: Array[String]): Unit = {
    import Common.{ sampleInput, printOutput }
    printOutput(programWithConstantDelay(sampleInput))
    printOutput(programWithExponentialBackOff(sampleInput))
  }
}
