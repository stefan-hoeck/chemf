/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf

import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._
import org.scalacheck._, Prop._, Arbitrary.arbitrary

/**
 * @author Stefan Höck
 */
trait Generators {
  implicit val ElementArbitrary = Arbitrary (Gen oneOf Element.values)

  implicit val BondArbitrary = Arbitrary (Gen oneOf Bond.values)

  implicit val StereoArbitrary = Arbitrary (Gen oneOf Stereo.values)

  implicit val IsotopeArbitrary = ElementArbitrary ∘ (Isotope.apply (_))

  implicit val AtomArbitrary = Arbitrary (
    arbitrary[Isotope] ⊛
    Gen.choose(-4,4) ⊛
    Gen.choose(0, 4) ⊛ 
    arbitrary[Stereo] apply Atom.apply
  )
    
}

object Generators extends Generators

// vim: set ts=2 sw=2 et:
