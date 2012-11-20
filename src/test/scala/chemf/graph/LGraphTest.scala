/**                                                               **\
**   Copyright (c) 2012 Center of Organic and Medicinal Chemistry **
**                 Zurich University of Applied Sciences          **
**                 Wädenswil, Switzerland                         **
\**                                                               **/

package chemf.graph

import chemf._
import org.scalacheck._, Prop._, Arbitrary.arbitrary
import scalaz._, Scalaz._

/**
 * @author Stefan Höck
 */
object LGraphTest extends Properties("LGraph") with Generators {
  val atomsG = (Gen listOf arbitrary[Atom]) map (_.toIndexedSeq)

  def bondG (order: Int) = for {
    a ← Gen choose (0, order - 1)
    b ← Gen choose (0, order - 1)
    if (a ≠ b)
    bt ← arbitrary[Bond]
  } yield (Edge(a,b), bt)

  def bondsG (order: Int) = (
    (order < 2) ?
    (Gen value Seq.empty[(Edge,Bond)]) |
    Gen.listOf(bondG(order))
  ) map (_.toMap)

  def molG = for {
    atoms ← atomsG
    bonds ← bondsG(atoms.size)
  } yield LGraph[Bond,Atom](atoms, bonds)

  implicit val MolArbitrary = Arbitrary[Molecule](molG)

  property("addVertex") = forAll {p: (Molecule,Atom) ⇒ 
    val (m, a) = p
    val mNew = m :+ a
    
    (mNew.order ≟ (m.order + 1)) :| "order" &&
    (mNew(m.order) ≟ a) :| "added atom" &&
    (mNew.graph isIsolate m.order) :| "is isolate" &&
    (mNew.edgesTo(m.order).isEmpty) :| "no new edges"
  }

  val addVertexG = for {
    m ← arbitrary[Molecule]
    if (m.order > 0)
    v ← Gen choose (0, m.order - 1)
    a ← arbitrary[Atom]
    b ← arbitrary[Bond]
  } yield (m, v, a, b)

  property("addVertex") = forAll(addVertexG) {q ⇒ 
    val (m, neighbor, a, b) = q
    val mNew = m addVertex (a, b, neighbor)
    
    (mNew.order ≟ (m.order + 1)) :| "order" &&
    (mNew(m.order) ≟ a) :| "added atom" &&
    (mNew.graph isPendant m.order) :| "is pendant" &&
    (mNew.edgesTo(m.order) ≟ List(b)) :| "one new edge" &&
    (mNew.eLabel(Edge(m.order, neighbor)) ≟ b) :| "edge set"
  }

  property("apply") = forAll(atomsG) {as ⇒ 
    val m = LGraph(as, Map.empty[Edge,Bond])
    
    (m.order ≟ as.size) &&
    (0 until as.size forall (v ⇒ m(v) ≟ as(v)))
  }

  property("edgesTo") = forAll {m: Molecule ⇒ 
    (0 until m.order forall (v ⇒ m.graph.degree(v) ≟ m.edgesTo(v).size))
  }

  property("emap") = forAll {p: (Molecule, Bond ⇒ Int) ⇒ 
    val (m, f) = p
    val mapped = m emap f

    m.edges ∀ (e ⇒ f(m eLabel e) ≟ mapped.eLabel(e))
  }

  property("map") = forAll {p: (Molecule, Atom ⇒ Int) ⇒ 
    val (m, f) = p
    val mapped = m map f

    List.range(0, m.order) ∀ (v ⇒ f(m vLabel v) ≟ mapped.vLabel(v))
  }

  property("mapI") = forAll {p: (Molecule, (Atom,Int) ⇒ Int) ⇒ 
    val (m, f) = p
    val mapped = m mapI f

    List.range(0, m.order) ∀ (v ⇒ f(m vLabel v, v) ≟ mapped.vLabel(v))
  }
}

// vim: set ts=2 sw=2 et:
