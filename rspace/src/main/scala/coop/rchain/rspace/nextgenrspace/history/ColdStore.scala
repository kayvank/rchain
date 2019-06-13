package coop.rchain.rspace.nextgenrspace.history

import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash
import scodec.Codec
import scodec.bits.ByteVector
import scodec.codecs.{discriminated, uint2}
import coop.rchain.rspace.internal.codecByteVector
import coop.rchain.shared.{AttemptOpsF => G}

trait ColdStore[F[_]] {
  def put(hash: Blake2b256Hash, data: PersistedData): F[Unit]

  def get(hash: Blake2b256Hash): F[Option[PersistedData]]

  def close(): F[Unit]
}

object ColdStoreInstances {
  private[history] def codecPersistedData: Codec[PersistedData] =
    discriminated[PersistedData]
      .by(uint2)
      .subcaseP(0) {
        case n: JoinsLeaf => n
      }(codecByteVector.as[JoinsLeaf])
      .subcaseP(1) {
        case s: DataLeaf => s
      }(codecByteVector.as[DataLeaf])
      .subcaseP(2) {
        case c: ContinuationsLeaf => c
      }(codecByteVector.as[ContinuationsLeaf])

  def coldStore[F[_]: Sync](store: Store[F]): ColdStore[F] = new ColdStore[F] {
    private val codec = codecPersistedData

    import G.RichAttempt
    override def put(key: Blake2b256Hash, d: PersistedData): F[Unit] =
      for {
        encoded <- codec.encode(d).get
        data    <- store.put(key, encoded)
      } yield data

    def get(key: Blake2b256Hash): F[Option[PersistedData]] =
      for {
        maybeBytes         ← store.get(key)
        maybeDecoded       <- maybeBytes.map(bytes ⇒ codec.decode(bytes).get).sequence
        maybePersistedData = maybeDecoded.map(_.value)
      } yield maybePersistedData

    override def close(): F[Unit] = store.close()
  }
}

sealed trait PersistedData {
  def bytes: ByteVector
}

final case class JoinsLeaf(bytes: ByteVector)         extends PersistedData
final case class DataLeaf(bytes: ByteVector)          extends PersistedData
final case class ContinuationsLeaf(bytes: ByteVector) extends PersistedData
