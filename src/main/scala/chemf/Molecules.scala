/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf

import Element.H
import chemf.graph.LGraph
import scalaz._, Scalaz._

/**
 * @author Stefan Höck
 */
trait Molecules {

  /**
   * Calculates the exact mass for a given molecule. Returns None,
   * if the exact mass for one or more isotopes was not defined.
   */
  def exactMass (m: Molecule): Option[Double] = m foldMap (_.exactMass)

  /**
   * Calculates the exact mass for a given formula. Returns None,
   * if the exact mass for one or more isotopes was not defined.
   */
  def exactMass (f: Formula): Option[Double] = 
    f.toSeq foldMap (p ⇒ p._1.exactMass map (_ * p._2))

  /**
   * Calculates the total formula of a molecule
   */
  def formula (m: Molecule): Formula = m foldMap (_.formula)

  /**
   * Calculates the molar weight of a molecule. Returns None
   * if the mass for one or more isotopes was not defined.
   */
  def mass (m: Molecule): Option[Double] = m.foldMap (_.mass)

}

object Molecules extends Molecules

// vim: set ts=2 sw=2 et:
