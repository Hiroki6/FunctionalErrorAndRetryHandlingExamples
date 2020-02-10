package errors.tagless_final

import cats.Monad
import cats.effect.IO
import errors.tagless_final.Common.{ EitherThrowable, Input, PreconditionException, Output }

object MonadProgram extends App {
  class UseCase[F[_]](implicit M: Monad[F], repository: Repository[F]) {
    def update(in: Input): F[Output] = {
      M.flatMap(repository.get(in)) {
        case Some(value) => repository.update(value)
        case None        => throw PreconditionException("Not Found")
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

  // Error occurred
  import cats.instances.either._
  implicit val eitherRepository = EitherRepository
  val eitherUseCase = new UseCase[EitherThrowable]
  eitherUseCase.update(in) match {
    case Left(e)      => println(e.getMessage)
    case Right(value) => println(value.value)
  }
}
