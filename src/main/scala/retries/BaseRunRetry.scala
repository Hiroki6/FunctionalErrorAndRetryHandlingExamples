package retries

import cats.effect.IO

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.util.Random
import Common.execute

object BaseRunRetry {
  val logger = new Logger()
  val randomGenerator = new Random(10)

  def program(input: Input): IO[Output] = {
    def runRetry(maxRetryCount: Int, cumulativeDelay: FiniteDuration = 0.second, counter: Int = 0): IO[Output] =
      execute(input).handleErrorWith {
        t =>
          if (counter >= maxRetryCount) {
            // logging
            logger.info(s"Giving up on program function because of ${t.getMessage}, after $maxRetryCount retries, finally total delay was $cumulativeDelay [ms]")
            IO.raiseError(t)
          } else {
            val delay = 10.milliseconds
            // logging
            logger.info(s"Error has occurred on program function because of ${t.getMessage}, retry[counter=$counter] after $delay [ms] sleeping...")
            // sleep for a few times before retry
            Thread.sleep(delay.toMillis)
            runRetry(maxRetryCount, cumulativeDelay + delay, counter + 1)
          }
      }
    runRetry(maxRetryCount = 3)
  }

  def main(args: Array[String]): Unit = {
    import Common.{sampleInput, printOutput}
    printOutput(program(sampleInput))
  }
}
