package coop.rchain.node.configuration

import java.nio.file.Path

import coop.rchain.casper.util.comm.ListenAtName.Name
import coop.rchain.comm.PeerNode
import coop.rchain.shared.StoreType

final case class Server(
    host: Option[String],
    port: Int,
    httpPort: Int,
    kademliaPort: Int,
    dynamicHostAddress: Boolean,
    noUpnp: Boolean,
    defaultTimeout: Int,
    bootstrap: PeerNode,
    standalone: Boolean,
    genesisValidator: Boolean,
    dataDir: Path,
    mapSize: Long,
    storeType: StoreType,
    maxNumOfConnections: Int,
    maxMessageSize: Int
)

final case class GrpcServer(
    host: String,
    portExternal: Int,
    portInternal: Int
)

final case class Tls(
    certificate: Path,
    key: Path,
    customCertificateLocation: Boolean,
    customKeyLocation: Boolean,
    secureRandomNonBlocking: Boolean
)

final case class Kamon(
    prometheus: Boolean,
    influxDb: Option[InfluxDb],
    zipkin: Boolean,
    sigar: Boolean
)

final case class InfluxDb(
    hostname: String,
    port: Int,
    database: String,
    protocol: String,
    authentication: Option[InfluxDBAuthentication]
)

final case class InfluxDBAuthentication(
    user: String,
    password: String
)

sealed trait Command
final case class Eval(files: List[String]) extends Command
final case object Repl                     extends Command
final case object Diagnostics              extends Command
final case class Deploy(
    address: String,
    phloLimit: Long,
    phloPrice: Long,
    nonce: Int,
    location: String
) extends Command
final case object DeployDemo                                               extends Command
final case object Propose                                                  extends Command
final case class ShowBlock(hash: String)                                   extends Command
final case class ShowBlocks(depth: Int)                                    extends Command
final case class VisualizeDag(depth: Int, showJustificationLines: Boolean) extends Command
final case object Run                                                      extends Command
final case object Help                                                     extends Command
final case class DataAtName(name: Name)                                    extends Command
final case class ContAtName(names: List[Name])                             extends Command
final case class BondingDeployGen(
    bondKey: String,
    ethAddress: String,
    amount: Long,
    secKey: String,
    pubKey: String
) extends Command
final case class FaucetBondingDeployGen(
    amount: Long,
    sigAlgorithm: String,
    secKey: String,
    pubKey: String
) extends Command
