package coop.rchain.rspace
package experiment
package history

import cats.implicits._
import cats.effect.Sync
import coop.rchain.shared.AttemptOpsF.RichAttempt
import scodec.bits.BitVector

trait HistoryStore[F[_]] {
  def put(tries: List[Trie]): F[Unit]

  def get(key: Blake2b256Hash): F[Trie]

  def close(): F[Unit]
}

object HistoryStoreInstances {
  type KVData = (Blake2b256Hash, BitVector)
  def historyStore[F[_]: Sync](store: Store[F]): HistoryStore[F] = new HistoryStore[F] {
    // TODO put list
    override def put(tries: List[Trie]): F[Unit] = {

      def asEncoded(t: Trie): F[KVData] =
        for {
          b <- Trie.codecTrie.encode(t).get
          k = Trie.hash(t)
        } yield (k, b)

      for {
        asKeyValue <- tries traverse asEncoded
        storeRes   <- store.put(asKeyValue)
      } yield (storeRes)
    }

    override def get(key: Blake2b256Hash): F[Trie] =
      for {
        maybeBytes <- store.get(key)
        result     <- maybeBytes.traverse(bytes => Trie.codecTrie.decode(bytes).get)
      } yield (result.map(_.value).getOrElse(EmptyTrie))

    override def close(): F[Unit] = store.close()
  }
}
