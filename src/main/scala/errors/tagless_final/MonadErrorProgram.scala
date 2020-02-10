package errors.tagless_final

import cats.MonadError
import cats.effect.IO
import errors.tagless_final.Common.{ EitherThrowable, Input, PreconditionException, Output }

object MonadErrorProgram extends App {
  class UseCase[F[_]](implicit ME: MonadError[F, Throwable], repository: Repository[F]) {
    def update(in: Input): F[Output] = {
      ME.flatMap(repository.get(in)) {
        case Some(value) => repository.update(value)
        case None        => ME.raiseError(PreconditionException("Not Found"))
      }
    }
  }

  val in = Input("sample_id")
  implicit val ioRepository = IORepository
  val ioUseCase = new UseCase[IO]
  ioUseCase.update(in).attempt.map {
    case Left(e)      => println(e.getMessage)
    case Right(value) => println(value.value)
  }.unsafeRunSync()

  import cats.instances.either._
  implicit val eitherRepository = EitherRepository
  val eitherUseCase = new UseCase[EitherThrowable]
  eitherUseCase.update(in) match {
    case Left(e)      => println(e.getMessage)
    case Right(value) => println(value.value)
  }
}