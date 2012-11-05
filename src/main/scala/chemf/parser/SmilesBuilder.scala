package chemf.parser

import chemf._, Stereo.Undefined
import scalaz._, Scalaz._

trait SmilesBuilder[A] {
  type STrans = A ⇒ ValRes[A]

  val empty: A

  def addAtom (
    isotope: Isotope,
    charge: Int,
    hydrogens: Option[Int],
    aromatic: Boolean,
    stereo: Stereo,
    atomClass: Int
  ): STrans

  def addElem (e: Element) = addAtom(Isotope(e), 0, None, false, Undefined, 0)

  def addAromElem (e: Element) =
    addAtom (Isotope(e), 0, None, true, Undefined, 0)

  def clear: STrans

  def closeBranch: STrans

  def openBranch: STrans

  def ring (i: Int): STrans

  def ring (a: Char, b: Char): STrans = ring(a.asDigit * 10 + b.asDigit)

  def setBond (b: Bond): STrans

  def setDbStereo (c: Char): STrans
}

// vim: set ts=2 sw=2 et:
