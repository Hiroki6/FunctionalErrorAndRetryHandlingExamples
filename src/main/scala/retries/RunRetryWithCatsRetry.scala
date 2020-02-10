package retries

import java.time.{ DayOfWeek, LocalDate }

import cats.effect.{ IO, Timer }
import retry.RetryDetails.{ GivingUp, WillDelayAndRetry }
import retry._
import retry.RetryPolicies._

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.global
import retry.CatsEffect._
import Common.execute
import cats.syntax.semigroup._

object RunRetryWithCatsRetry {
  implicit val timer: Timer[IO] = IO.timer(global)

  val logger = new Logger()
  val logMessages = collection.mutable.ArrayBuffer.empty[String]

  def logError(action: String)(err: Throwable, details: RetryDetails): IO[Unit] = details match {
    case WillDelayAndRetry(nextDelay: FiniteDuration, retriesSoFar: Int, cumulativeDelay: FiniteDuration) =>
      IO {
        logger.info(
          s"Error has occurred on $action because of ${err.getMessage}, retry[counter=$retriesSoFar] after ${nextDelay.toMillis} [ms] sleeping..., total delay was ${cumulativeDelay.toMillis} [ms] so far"
        )
      }

    case GivingUp(totalRetries: Int, totalDelay: FiniteDuration) =>
      IO {
        logger.info(s"Giving up on $action because of ${err.getMessage} after $totalRetries retries, finally total delay was ${totalDelay.toMillis} [ms]")
      }
  }

  val retry3times1seconds = limitRetries[IO](3) |+| exponentialBackoff[IO](1.seconds)

  /**
   * The program retry with a delay exponentially 3 times
   */
  def programWithExponentialBackOff(input: Input): IO[Output] = {
    retryingOnAllErrors[Output](
      policy = retry3times1seconds,
      onError = logError("programWithExponentialBackOff")
    )(execute(input))
  }

  val retry5times100millis = limitRetries[IO](5) |+| constantDelay[IO](100.millis)
  val retry1mins = constantDelay[IO](1.minute)

  /**
   * The program retry with a 100ms delay 5 times and then retry every minutes.
   */
  def programWithAdvancedPolicy(input: Input): IO[Output] = {
    retryingOnAllErrors[Output](
      policy = retry5times100millis.followedBy(retry1mins),
      onError = logError("programWithAdvancedPolicy")
    )(execute(input))
  }

  def isWorthRetrying(err: Throwable): Boolean = {
    err.getMessage.contains("timeout")
  }

  /**
   * The program retry only when the connection timeout occurs.
   */
  def programWithRetryWhenTimeout(input: Input): IO[Output] = {
    retryingOnSomeErrors[Output](
      policy = retry3times1seconds,
      isWorthRetrying = isWorthRetrying,
      onError = logError("programWithRetryWhenTimeout")
    )(execute(input))
  }

  /**
   * This retry policy is applied for only Tuesday.
   * [https://cb372.github.io/cats-retry/docs/policies.html]
   */
  val onlyRetryOnTuesdays = RetryPolicy.lift[IO] { _ =>
    if (LocalDate.now().getDayOfWeek() == DayOfWeek.TUESDAY) {
      PolicyDecision.DelayAndRetry(delay = 100.milliseconds)
    } else {
      PolicyDecision.GiveUp
    }
  }

  def programWithRetryOnTuesday(input: Input): IO[Output] = {
    retryingOnSomeErrors[Output](
      policy = onlyRetryOnTuesdays.join(limitRetries[IO](5) |+| exponentialBackoff(1.millis)),
      isWorthRetrying = isWorthRetrying,
      onError = logError("programWithRetryOnTuesday")
    )(execute(input))
  }

  def main(args: Array[String]): Unit = {
    import Common.{ sampleInput, printOutput }
    printOutput(programWithExponentialBackOff(sampleInput))
    printOutput(programWithAdvancedPolicy(sampleInput))
    printOutput(programWithRetryWhenTimeout(sampleInput))
    printOutput(programWithRetryOnTuesday(sampleInput))
  }
}