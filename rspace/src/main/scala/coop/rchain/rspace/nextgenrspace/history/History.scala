package coop.rchain.rspace.nextgenrspace.history

import coop.rchain.rspace.Blake2b256Hash
import scodec.{Attempt, Codec}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{discriminated, provide, uint, uint2, vectorOfN}
import coop.rchain.rspace.internal.codecByteVector
import History._
import cats.{Applicative, FlatMap}
import cats.effect.Sync
import cats.implicits._
import coop.rchain.shared.AttemptOpsF._

trait History[F[_]] {
  def process(actions: List[HistoryAction]): F[History[F]]
  def root: Blake2b256Hash
  def find(key: KeyPath): F[(TriePointer, Vector[Trie])]
  def close(): F[Unit]
  def reset(root: Blake2b256Hash): History[F]
}

object History {

  val emptyRoot: Trie = EmptyTrie
  private[this] def encodeEmptyRoot[F[_]: Sync]: F[ByteVector] =
    for {
      bitVector  <- Trie.codecTrie.encode(emptyRoot).get
      byteVector = bitVector.toByteVector
    } yield (byteVector)

  def emptyRootHash[F[_]: Sync] =
    encodeEmptyRoot.map(x ⇒ Blake2b256Hash.create(x))

  // this mapping is kept explicit on purpose
  @inline
  private[history] def toInt(b: Byte): Int =
    java.lang.Byte.toUnsignedInt(b)

  // this mapping is kept explicit on purpose
  @inline
  private[history] def toByte(i: Int): Byte =
    i.toByte

  def commonPrefix(l: KeyPath, r: KeyPath): KeyPath =
    (l.view, r.view).zipped.takeWhile { case (ll, rr) => ll == rr }.map(_._1).toSeq

  def skip[F[_]: Sync](pb: PointerBlock, affix: KeyPath): F[Trie] =
    for {
      pbHashed <- pb.hash
      trie = Skip(
        ByteVector(affix),
        NodePointer(pbHashed)
      )
    } yield (trie)

  type KeyPath = Seq[Byte]
}

sealed trait Trie
sealed trait NonEmptyTrie                                   extends Trie
case object EmptyTrie                                       extends Trie
final case class Skip(affix: ByteVector, ptr: ValuePointer) extends NonEmptyTrie

object SkipOps {
  implicit class _SkipOps(val s: Skip) {
    def encoded[F[_]: Sync]: F[BitVector] = Trie.codecSkip.encode(this).get
    def hash[F[_]: Sync]: F[Blake2b256Hash] =
      encoded.map(e => Blake2b256Hash.create(e.toByteVector))
  }
}

final case class PointerBlock private (toVector: Vector[TriePointer]) extends NonEmptyTrie
object PointerBlockOps {
  implicit class _PointerBlockOps(val pb: PointerBlock) {
    def updated(tuples: List[(Int, TriePointer)]): PointerBlock =
      new PointerBlock(tuples.foldLeft(toVector) { (vec, curr) =>
        vec.updated(curr._1, curr._2)
      })

    def countNonEmpty: Int = toVector.count(_ != EmptyPointer)

    def encoded[F[_]: Sync]: F[BitVector] =
      PointerBlock.codecPointerBlock.encode(this).get

    def hash[F[_]: Sync]: F[Blake2b256Hash] =
      encoded.map(x => Blake2b256Hash.create(x.toByteVector))

    def toString[F[_]: Sync]: F[String] =
      for {
        h ← hash
        pbs = toVector.zipWithIndex
          .filter { case (v, _) => v != EmptyPointer }
          .map(_.swap)
          .mkString(";")
        s = s"PB($h: $pbs)"
      } yield s
  }

}

sealed trait TriePointer
sealed trait NonEmptyTriePointer extends TriePointer {
  def hash: Blake2b256Hash
}
case object EmptyPointer  extends TriePointer
sealed trait ValuePointer extends NonEmptyTriePointer

final case class LeafPointer(hash: Blake2b256Hash) extends ValuePointer
final case class SkipPointer(hash: Blake2b256Hash) extends NonEmptyTriePointer
final case class NodePointer(hash: Blake2b256Hash) extends ValuePointer

object Trie {
  import SkipOps._
  def hash[F[_]: Sync](trie: Trie): F[Blake2b256Hash] =
    trie match {
      case pb: PointerBlock  => pb.hash
      case s: Skip           => s.hash
      case _: EmptyTrie.type => History.emptyRootHash
    }

  val codecSkip: Codec[Skip] = (codecByteVector :: codecTrieValuePointer).as[Skip]

  def memoizingSkipCodec: Codec[Skip] = ???
//    Codec.apply((s: Skip) => Attempt.successful(s.encoded), codecSkip.decode)

  def memoizingPointerBlockCodec: Codec[PointerBlock] = ???
//    Codec.apply(
//      (s: PointerBlock) => Attempt.successful(s.encoded),
//      PointerBlock.codecPointerBlock.decode
//    )

  val codecTrie: Codec[Trie] =
    discriminated[Trie]
      .by(uint2)
      .subcaseP(0) {
        case e: EmptyTrie.type => e
      }(provide(EmptyTrie))
      .subcaseP(1) {
        case s: Skip => s
      }(memoizingSkipCodec)
      .subcaseP(2) {
        case pb: PointerBlock => pb
      }(memoizingPointerBlockCodec)

  implicit def codecTriePointer: Codec[TriePointer] =
    discriminated[TriePointer]
      .by(uint2)
      .subcaseP(0) {
        case p: EmptyPointer.type => p
      }(provide(EmptyPointer))
      .subcaseP(1) {
        case p: LeafPointer => p
      }(Blake2b256Hash.codecBlake2b256Hash.as[LeafPointer])
      .subcaseP(2) {
        case p: SkipPointer => p
      }(Blake2b256Hash.codecBlake2b256Hash.as[SkipPointer])
      .subcaseP(3) {
        case p: NodePointer => p
      }(Blake2b256Hash.codecBlake2b256Hash.as[NodePointer])

  implicit def codecTrieValuePointer: Codec[ValuePointer] =
    discriminated[ValuePointer]
      .by(uint(1))
      .subcaseP(0) {
        case p: LeafPointer => p
      }(Blake2b256Hash.codecBlake2b256Hash.as[LeafPointer])
      .subcaseP(1) {
        case p: NodePointer => p
      }(Blake2b256Hash.codecBlake2b256Hash.as[NodePointer])

}

object PointerBlock {

  val length = 256

  def create(): PointerBlock = new PointerBlock(Vector.fill(length)(EmptyPointer))

  def create(first: (Int, TriePointer)): PointerBlock =
    PointerBlock.create().updated(List(first))

  def create(first: (Int, TriePointer), second: (Int, TriePointer)): PointerBlock =
    PointerBlock.create().updated(List(first, second))

  // consider using zlib
  implicit val codecPointerBlock: Codec[PointerBlock] =
    vectorOfN(
      provide(length),
      Trie.codecTriePointer
    ).as[PointerBlock]

  def unapply(arg: PointerBlock): Option[Vector[TriePointer]] = Option(arg.toVector)
}

final case class TriePath(nodes: Vector[Trie], conflicting: Option[Trie], edges: KeyPath) {
  def append(affix: KeyPath, t: Trie): TriePath =
    this.copy(nodes = this.nodes :+ t, edges = this.edges ++ affix)
}

object TriePath {
  def empty: TriePath = TriePath(Vector(), None, Nil)
}
