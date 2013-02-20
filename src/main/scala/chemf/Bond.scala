/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf

import scalaz.{Equal, Show}

/**
 * @author Stefan Höck
 */
sealed abstract class Bond(val symbol: String, val valence: Int)

object Bond {
  case object Single extends Bond("-", 1)
  case object Double extends Bond("=", 2)
  case object Triple extends Bond("#", 3)
  case object Quadruple extends Bond("$", 4)
  case object Aromatic extends Bond(":", 0)

  val values = List[Bond] (Single, Double, Triple, Quadruple, Aromatic)

  implicit val BondEqual = Equal.equalA[Bond]

  implicit val BondShow = Show.shows[Bond](_.symbol)
}


// vim: set ts=2 sw=2 et:
