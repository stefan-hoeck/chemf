/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf

import graph.LGraph
import org.scalacheck._, Prop._
import scalaz._, Scalaz._

/**
 * @author Stefan Höck
 */
object MoleculesTest extends Properties("Molecules") {

  private def singleAtomProp[A] (a: Molecule ⇒ A, f: (Element,A) ⇒ Boolean)
  : Boolean = {
    def mol(e: Element) =
      LGraph.empty[Bond,Atom] addVertex (Atom fromElement e)

    Element.values ∀ (e ⇒ f(e, a(mol(e))))
  }

  property("formula") = singleAtomProp[Map[Isotope,Int]](
    Molecules.formula, (e,m) ⇒ m ≟ Map(Isotope(e) → 1))
    
}

// vim: set ts=2 sw=2 et:
