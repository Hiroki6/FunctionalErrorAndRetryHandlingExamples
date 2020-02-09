package errors.tagless_final

import cats.effect.IO
import errors.tagless_final.Common.{ EitherThrowable, Input, Output }

object Common {
  case class Input(value: String)
  case class Output(value: String)
  case class PreconditionException(message: String) extends RuntimeException {
    override def getMessage: String = message
  }
  type EitherThrowable[A] = Either[Throwable, A]
}

trait Repository[F[_]] {
  def get(id: Input): F[Option[Output]]
  def update(value: Output): F[Output]
}

object IORepository extends Repository[IO] {
  def get(id: Input): IO[Option[Output]] = IO { None }
  def update(value: Output): IO[Output] = IO { value }
}

object EitherRepository extends Repository[EitherThrowable] {
  def get(id: Input): EitherThrowable[Option[Output]] = Right(None)
  def update(value: Output): EitherThrowable[Output] = Right(value)
}
