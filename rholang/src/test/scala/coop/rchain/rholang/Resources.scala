package coop.rchain.rholang
import java.io.File
import java.nio.file.{Files, Path}

import cats._
import cats.implicits._
import cats.Applicative
import cats.effect.ExitCase.Error
import cats.effect.{Concurrent, ContextShift, Resource}
import cats.effect.concurrent.Semaphore
import com.typesafe.scalalogging.Logger
import coop.rchain.metrics.Metrics
import coop.rchain.models._
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.Runtime
import coop.rchain.rholang.interpreter.Runtime.{RhoContext, RhoISpace, SystemProcess}
import coop.rchain.rholang.interpreter.errors.InterpreterError
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.{Context, RSpace}
import coop.rchain.shared.{Log, StoreType}
import monix.eval.Task
import monix.execution.Scheduler

import scala.reflect.io.Directory

object Resources {
  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  def mkTempDir[F[_]: Applicative](prefix: String): Resource[F, Path] =
    Resource.makeCase(Applicative[F].pure(Files.createTempDirectory(prefix)))(
      (path, exitCase) =>
        Applicative[F].pure(exitCase match {
          case Error(ex) =>
            logger
              .error(
                s"Exception thrown while using the tempDir '$path'. Temporary dir NOT deleted.",
                ex
              )
          case _ => new Directory(new File(path.toString)).deleteRecursively()
        })
    )

  def mkRhoISpace[F[_]: Concurrent: ContextShift: Log: Metrics](
      prefix: String = "",
      branch: String = "test",
      mapSize: Long = 1024L * 1024L * 4
  ): Resource[F, RhoISpace[F]] = {
    import coop.rchain.rholang.interpreter.storage.implicits._

    import scala.concurrent.ExecutionContext.Implicits.global

    def mkRspace(dbDir: Path): F[RhoISpace[F]] = {
      val context: RhoContext[F] = Context.create(dbDir, mapSize)

      RSpace.create[
        F,
        Par,
        BindPattern,
        ListParWithRandom,
        ListParWithRandom,
        TaggedContinuation
      ](context, Branch(branch))
    }

    mkTempDir(prefix)(implicitly[Concurrent[F]])
      .flatMap(tmpDir => Resource.make(mkRspace(tmpDir))(_.close()))
  }

  def mkRuntime[F[_]: ContextShift: Concurrent: Log: Metrics, M[_]: Parallel[F, ?[_]]](
      prefix: String,
      storageSize: Long = 1024 * 1024,
      additionalSystemProcesses: Seq[SystemProcess.Definition[F]] = Seq.empty
  )(implicit scheduler: Scheduler): Resource[F, Runtime[F]] =
    mkTempDir[F](prefix)
      .flatMap { tmpDir =>
        Resource.make[F, Runtime[F]] {
          for {
            cost <- CostAccounting.emptyCost[F]
            runtime <- {
              implicit val c = cost
              Runtime.create[F, M](tmpDir, storageSize, StoreType.LMDB, additionalSystemProcesses)
            }
          } yield (runtime)
        }(
          rt => rt.close()
        )
      }
}
