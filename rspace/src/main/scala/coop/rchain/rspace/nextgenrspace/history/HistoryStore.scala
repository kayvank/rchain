package coop.rchain.rspace.nextgenrspace.history

import cats.implicits._
import cats.effect.Sync
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.shared.AttemptOpsF.RichAttempt

trait HistoryStore[F[_]] {
  def put(tries: List[Trie]): F[Unit]

  def get(key: Blake2b256Hash): F[Trie]

  def close(): F[Unit]
}

object HistoryStoreInstances {
  def historyStore[F[_]: Sync](store: Store[F]): HistoryStore[F] = new HistoryStore[F] {
    // TODO put list
    override def put(tries: List[Trie]): F[Unit] = {
      def put(t: Trie): F[Unit] = {
        val k = Trie.hash(t)
        for {
          b <- Trie.codecTrie.encode(t).get
          _ <- store.put(k, b)
        } yield ()
      }
      for {
        _ <- tries traverse put
      } yield ()

    }

    override def get(key: Blake2b256Hash): F[Trie] =
      for {
        maybeBytes <- store.get(key)
        result <- maybeBytes
                   .map(bytes => Trie.codecTrie.decode(bytes).get)
                   .sequence
        f = result.map(_.value).getOrElse(EmptyTrie)
      } yield (f)

    override def close(): F[Unit] = store.close()
  }
}
