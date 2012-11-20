/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf.parser

import chemf._, Bond._, Element._
import scalaz._, Scalaz._

/**
 * A data type representing information about atoms available
 * from SMILES strings
 */
case class SmilesAtom (
  isotope: Isotope,
  charge: Int,
  hydrogens: Option[Int],
  stereo: Stereo,
  atomClass: Int
) {
  def element = isotope.element
}

object SmilesAtom {
  /**
   * Calculate implicit hydrogens for element e with adjacent bonds bs.
   * In most cases it is just summing up the valences of the bonds
   * and subtracting this number from the natural valence of the element.
   * Multiple aromatic bonds mean different things for different
   * elements and are treated on a case by case basis. 
   *
   * Valid valences for organic subset elements are sometimes a bit
   * less strict that defined by the OpenSMILES standard:
   * 1, 3, 5, 7 for Cl, Br, and I (instead of just 1)
   */
  def implicitHydrogens (bs: List[Bond], e: Element): ValRes[Int] = {
    def msg = "Invalid bond set for element %s: %s" format (e, bs mkString ",")
    def fail = msg.failNel[Int]

    def default (v: Int): ValRes[Int] =
      valences get e flatMap (_ find (v<=)) fold (_ - v success, fail)

    bs count (Aromatic ==) match {
      case 1 ⇒ default (2 + (bs foldMap (_.valence)))
      case 0 ⇒ default (bs foldMap (_.valence))
      case _ ⇒ bs.toList sortBy (_.valence) match {
        case Aromatic::Aromatic::Aromatic::Nil ⇒
          if (e == C || e == N || e == P || e == B) 0.success else fail
        case Aromatic::Aromatic::Single::Nil   ⇒ 
          if (e == C || e == N || e == P || e == B) 0.success else fail
        case Aromatic::Aromatic::Double::Nil   ⇒ 
          if (e == C || e == S) 0.success else fail
        case Aromatic::Aromatic::Nil           ⇒
          if (e == C || e == B) 1.success
          else if (e == N || e == P || e == O || e == S) 0.success
          else fail
        //other combos with 2 or more aromatic bonds don't make sense
        case _ ⇒ fail
      }
    }
  }

  private val valences: Map[Element,Seq[Int]] = Map(
    B → Seq(3),
    C → Seq(4),
    N → Seq(3,5),
    O → Seq(2),
    P → Seq(3,5),
    S → Seq(2,4,6),
    F → Seq(1),
    Cl → Seq(1, 3, 5, 7),
    Br → Seq(1, 3, 5, 7),
    I → Seq(1, 3, 5, 7)
  )
}

// vim: set ts=2 sw=2 et:
