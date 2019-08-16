package coop.rchain.rspace.test

import scala.collection.mutable.ListBuffer
import coop.rchain.rspace._
import coop.rchain.rspace.examples.StringExamples.{Pattern, StringMatch, StringsCaptor, Wildcard}
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal.{Datum, GNAT, WaitingContinuation}
import coop.rchain.shared.GeneratorUtils._
import org.scalacheck._
import org.scalacheck.Arbitrary._
import scodec.bits.ByteVector

import scala.collection.SortedSet

object ArbitraryInstances {

  implicit val arbitraryPattern: Arbitrary[Pattern] = {
    val genWildcard: Gen[Pattern] = Gen.const(Wildcard)
    val genMatch: Gen[Pattern]    = Arbitrary.arbitrary[String].map((str: String) => StringMatch(str))
    val genPattern: Gen[Pattern]  = Gen.oneOf(genWildcard, genMatch)
    Arbitrary(genPattern)
  }

  val arbNonEmptyString =
    Arbitrary(Gen.nonEmptyListOf[Char](Arbitrary.arbChar.arbitrary).map(_.mkString))

  implicit def arbitraryDatumString(
      ): Arbitrary[Datum[String]] =
    Arbitrary(for {
      t <- Arbitrary.arbitrary[String]
      b <- Arbitrary.arbitrary[Boolean]
    } yield Datum.create(t, b))

  implicit def arbitraryDatum[T](
      implicit
      arbT: Arbitrary[T]
  ): Arbitrary[Datum[T]] =
    Arbitrary(for {
      t <- arbT.arbitrary
      b <- Arbitrary.arbitrary[Boolean]
    } yield Datum.create(t, b))

  implicit val arbitraryBlake2b256Hash: Arbitrary[Blake2b256Hash] =
    Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(bytes => Blake2b256Hash.create(bytes)))

  implicit val arbitraryByteVector: Arbitrary[ByteVector] =
    Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(bytes => ByteVector(bytes)))

  implicit val arbitraryTestKey: Arbitrary[TestKey4] =
    Arbitrary(Gen.sized { _ =>
      Gen
        .listOfN(4, Arbitrary.arbitrary[Int])
        .map(ints => TestKey4.create(ints))
    })

  implicit val arbitraryNonEmptyMapTestKeyByteVector: Arbitrary[Map[TestKey4, ByteVector]] = {
    Arbitrary(
      Gen
        .sized { size =>
          Gen.containerOfN[Seq, (TestKey4, ByteVector)](
            if (size > 2) size else 2,
            Arbitrary.arbitrary[(TestKey4, ByteVector)]
          )
        }
        .map(_.toMap)
    )
  }

  implicit val arbitraryTestKey32: Arbitrary[TestKey32] =
    Arbitrary(Gen.sized { _ =>
      Gen
        .listOfN(32, Arbitrary.arbitrary[Int])
        .map(ints => TestKey32.create(ints))
    })

  implicit val arbitraryNonEmptyMapTestKey32ByteVector: Arbitrary[Map[TestKey32, ByteVector]] = {
    Arbitrary(
      Gen
        .sized { size =>
          Gen.containerOfN[Seq, (TestKey32, ByteVector)](
            if (size > 2) size else 2,
            Arbitrary.arbitrary[(TestKey32, ByteVector)]
          )
        }
        .map(_.toMap)
    )
  }

  implicit def arbitraryNonEmptyMapStringDatumString: Arbitrary[Map[String, Datum[String]]] =
    Arbitrary(
      Gen
        .sized { size =>
          Gen.nonEmptyContainerOf[Seq, (String, Datum[String])](for {
            str <- arbNonEmptyString.arbitrary
            dat <- arbitraryDatum[String].arbitrary
          } yield (str, dat))
        }
        .map(_.toMap)
    )

  implicit def arbitraryWaitingContinuation
      : Arbitrary[WaitingContinuation[Pattern, StringsCaptor]] =
    Arbitrary(
      for {
        chans   <- Gen.nonEmptyListOf[String](Arbitrary.arbitrary[String])
        pats    <- Gen.containerOfN[List, Pattern](chans.length, Arbitrary.arbitrary[Pattern])
        boolean <- Arbitrary.arbitrary[Boolean]
      } yield WaitingContinuation.create(chans, pats, new StringsCaptor, boolean, SortedSet.empty)
    )

  def arbitraryWaitingContinuation(
      chans: List[String]
  ): Arbitrary[WaitingContinuation[Pattern, StringsCaptor]] =
    Arbitrary(
      for {
        pats    <- Gen.containerOfN[List, Pattern](chans.length, Arbitrary.arbitrary[Pattern])
        boolean <- Arbitrary.arbitrary[Boolean]
      } yield WaitingContinuation.create(chans, pats, new StringsCaptor, boolean, SortedSet.empty)
    )

  implicit def arbitraryGnat: Arbitrary[GNAT[String, Pattern, String, StringsCaptor]] =
    Arbitrary(Gen.sized { size =>
      val constrainedSize = if (size > 1) size else 1
      for {
        chans <- Gen.containerOfN[List, String](constrainedSize, Arbitrary.arbitrary[String])
        data <- Gen.nonEmptyContainerOf[List, Datum[String]](
                 arbitraryDatum[String].arbitrary
               )
        wks <- Gen.nonEmptyContainerOf[List, WaitingContinuation[Pattern, StringsCaptor]](
                arbitraryWaitingContinuation(chans).arbitrary
              )
      } yield GNAT(chans, data, wks)
    })

  implicit def arbitraryNonEmptyMapListStringWaitingContinuation
      : Arbitrary[Map[List[String], WaitingContinuation[Pattern, StringsCaptor]]] =
    Arbitrary(Gen.sized { size =>
      val constrainedSize = if (size > 1) size else 1
      Gen
        .nonEmptyContainerOf[List, (List[String], WaitingContinuation[Pattern, StringsCaptor])](
          for {
            chans <- Gen.containerOfN[List, String](constrainedSize, Arbitrary.arbitrary[String])
            wc    <- arbitraryWaitingContinuation(chans).arbitrary
          } yield (chans, wc)
        )
        .map(_.toMap)
    })

}
