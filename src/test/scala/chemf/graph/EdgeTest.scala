/**                                                               **\
**   Copyright (c) 2012 Center of Organic and Medicinal Chemistry **
**                 Zurich University of Applied Sciences          **
**                 Wädenswil, Switzerland                         **
\**                                                               **/

package chemf.graph

import scalaz._, Scalaz._, scalacheck.ScalaCheckBinding._
import org.scalacheck._, Prop._

/**
 * @author Stefan Höck
 */
object EdgeTest extends Properties ("Edge") {
  val edgeGen = for {
    a ← Gen choose (0, Int.MaxValue)
    b ← Gen choose (0, Int.MaxValue)
    if (a != b)
  } yield Edge(a,b)

  implicit val edgeArbitrary = Arbitrary (edgeGen)

  property ("equal") = Prop.forAll {es: (Edge,Edge) ⇒ 
    val (ea, eb) = es
    (ea ≟ eb) ≟ ((ea.a ≟ eb.a) && (ea.b ≟ eb.b))
  }

  property ("aSmallerb") = Prop.forAll {e: Edge ⇒ 
    e.a <= e.b
  }

  property ("isNeighbor") = Prop.forAll {ei: (Edge,Int) ⇒ 
    val (e, i) = ei
    (e connects i) ≟ ((e.a ≟ i) || (e.b ≟ i))
  }

  property ("compare") = Prop.forAll {es: (Edge,Edge) ⇒ 
    val (a,b) = es
    (if (a.a > b.a) a > b else true) :| "greater by a" &&
    (if (a.a < b.a) a < b else true) :| "smaller by a" &&
    (if (a.a ≟ b.a) (a.b compare b.b) ≟ (a compare b) else true) :| "by b"
  }
    
}

// vim: set ts=2 sw=2 et:
