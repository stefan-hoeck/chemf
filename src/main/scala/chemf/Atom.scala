/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf

import Element.H
import scalaz._, Scalaz._

/**
 * @author Stefan Höck
 */
case class Atom (
  isotope: Isotope,
  charge: Int,
  hydrogens: Int,
  stereo: Stereo
) {
  require(hydrogens >= 0, "Hydrogen count must be >= 0")

  def element = isotope.element

  def exactMass: Option[Double] =
    isotope.exactMass ⊛ H.exactMass apply (_ + hydrogens * _)

  def formula: Formula =
    Map(isotope → 1) ++
    ((hydrogens ≠ 0) ? Map(Isotope(H) → hydrogens) | Map.empty)

  def mass: Option[Double] = 
    isotope.mass ⊛ H.mass apply (_ + hydrogens * _)

  override def toString = {
    def formatCharge = charge match {
      case 0           ⇒ ""
      case x if(x < 0) ⇒ "(%d)" format x
      case x           ⇒ "(+%d)" format x
    }

    def formatStereo = stereo match {
      case Stereo.Undefined ⇒ ""
      case x                    ⇒ "(%s)" format x.symbol
    }

    def formatHs = hydrogens match {
      case 0 ⇒ ""
      case 1 ⇒ "H"
      case x ⇒ "H" + x
    }

    isotope.toString + formatHs + formatCharge + formatStereo
  }
}

object Atom {
  def fromElement (e: Element) = fromIsotope (Isotope(e))

  def fromIsotope (i: Isotope) = Atom (i, 0, 0, Stereo.Undefined)

  implicit val AtomEqual: Equal[Atom] =
    Equal.equalBy(a ⇒ (a.isotope, a.charge, a.hydrogens))

  implicit val AtomShow: Show[Atom] = Show.shows(_.toString)
}

// vim: set ts=2 sw=2 et:
