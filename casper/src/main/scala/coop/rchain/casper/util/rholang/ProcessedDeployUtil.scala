package coop.rchain.casper.util.rholang

import coop.rchain.casper.util.EventConverter
import coop.rchain.casper.protocol._
import coop.rchain.models.PCost
import coop.rchain.rspace.trace
import scodec.Codec

case class InternalProcessedDeploy(
    deploy: Deploy,
    cost: PCost,
    log: Seq[trace.Event],
    status: DeployStatus
)

object ProcessedDeployUtil {
  implicit val byteArrOrdering = coop.rchain.rspace.util.ordArrayByte

  def toInternal(pd: ProcessedDeploy): Option[InternalProcessedDeploy] =
    for {
      d <- pd.deploy
      c <- pd.cost
      l = pd.log
        .map(EventConverter.toRspaceEvent)
        .sortBy(
          e =>
            Codec.encode(e).getOrElse(throw new Exception("RSpace log is corrupted.")).toByteArray
        )
      s = if (pd.errored) UnknownFailure else Succeeded
    } yield InternalProcessedDeploy(d, c, l, s)

  def fromInternal(ipd: InternalProcessedDeploy): ProcessedDeploy = ipd match {
    case InternalProcessedDeploy(deploy, cost, log, status) =>
      ProcessedDeploy(
        deploy = Some(deploy),
        cost = Some(cost),
        log = log.map(EventConverter.toCasperEvent),
        errored = status.isFailed
      )
  }
}
