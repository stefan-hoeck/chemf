package chemf

import collection.immutable.{IndexedSeq ⇒ IxSq}
import scalaz._, Scalaz._, effect.IO, iteratee._

package object io {

  type DisIO[+A] = EitherT[IO,String,A]

  type DEnum[A] = EnumeratorT[A,DisIO]

  type DStep[E,A] = StepT[E,DisIO,A]

  type DIter[E,A] = IterateeT[E,DisIO,A]

}

// vim: set ts=2 sw=2 et:
