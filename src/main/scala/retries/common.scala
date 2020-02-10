package retries

import cats.effect.IO

class Logger() {
  def info(s: String) = println(s)
}

object Common {
  def execute(input: Input): IO[Output] = IO {
    val r = scala.util.Random
    val number = r.nextInt()
    if (number % 3 == 0) {
      throw new RuntimeException("Unexpected error occurred.")
    } else if (number % 3 == 1) {
      throw new RuntimeException("Connection timeout.")
    } else {
      println("Success!")
      Output("test")
    }
  }

  val sampleInput = Input("value")

  def printOutput(result: IO[Output]): Unit = {
    result.attempt.map {
      case Right(Output(v)) => println(v)
      case Left(e)          => println(e.getMessage)
    }.unsafeRunSync()
  }
}

case class Input(value: String)
case class Output(value: String)
