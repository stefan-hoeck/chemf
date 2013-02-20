/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf

import collection.immutable.{IndexedSeq ⇒ IxSq}
import scalaz._, Scalaz._, std.indexedSeq._

/**
 * @author Stefan Höck
 */
package object parser {

  type FARes[A] = ValRes[(FAState[A], A)]
  type IntState[A] = State[Int,A]
  type ValIntState[A] = IntState[ValRes[A]]

  //a bit of help for the compiler
  implicit val ValSApplicative = Applicative[IntState].compose[ValRes]
  implicit def ValIntStateMonoid[A:Monoid] = Monoid.liftMonoid[ValIntState,A]

  val EOT = '\u0004'

  /**
   * Transforms a SMILES string to a molecule
   */
  def smiles(s: String): ValRes[Molecule] =
    SmilesParser.Default parse s flatMap SmilesMol.toMolecule

  /**
   * Parses a single line, prepending the line number to all error messages.
   */
  def parseLine[A] (f: String ⇒ ValRes[A]): String ⇒ ValIntState[A] =
    s ⇒ State(i ⇒ (i + 1, mapErr(f(s))(x ⇒ s"Line $i: $x")))

  def parseSmilesLine = parseLine(smiles)

  /**
   * Parses a list of lines, prepending line number to all error messages.
   * Cannot be accelerated by parallelization,
   * due to the sequencial nature of the
   * state monad. To run the calculation starting with line number x use:
   *
   *  bulkParseSmiles(ss) exec x
   */
  def bulkParseSmiles(ss: IxSq[String]): ValIntState[IxSq[Molecule]] =
    ss.reverse traverse parseSmilesLine
}

// vim: set ts=2 sw=2 et:
