/**                                                      **\
**   Copyright (c) 2012 Organic Chemistry Group          **
**                 Zurich University of Applied Sciences **
**                 Wädenswil, Switzerland                **
\**                                                      **/

package chemf

import scalaz._, Scalaz._

/**
 * @author Stefan Höck
 */
package object parser {

  type FARes[A] = ValRes[(FAState[A], A)]
  type IntState[A] = State[Int,A]
  type ValIntState[A] = IntState[ValRes[A]]

  //a bit of help for the compiler
  implicit val ValSApplicative = Comp.CompApplicative[IntState,ValRes]
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
    s ⇒ state(i ⇒ (i + 1, mapErrS (f(s), "Line %d: %s" format (i, _))))

  def parseSmilesLine = parseLine(smiles)

  /**
   * Parses a list of lines, prepending line number to all error messages.
   * Cannot be accelerated by parallelization,
   * due to the sequencial nature of the
   * state monad. To run the calculation starting with line number x use:
   *
   *  bulkParseSmiles(ss) ! x
   */
  def bulkParseSmiles(ss: Seq[String]): ValIntState[Seq[Molecule]] =
    ss.reverse traverse parseSmilesLine
}

// vim: set ts=2 sw=2 et:
