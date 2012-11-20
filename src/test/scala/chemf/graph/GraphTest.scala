/**                                                      **\
**   Copyright (c) 2012 Organic Chemistry Group          **
**                 Zurich University of Applied Sciences **
**                 Wädenswil, Switzerland                **
\**                                                      **/

package chemf.graph

import org.scalacheck._, Prop._
import scalaz._, Scalaz._

/**
 * @author Stefan Höck
 */
object GraphTest extends Properties("Graph") {
  val edgeG = for {
    a ← Gen choose (0, 1000)
    b ← Gen choose (0, 1000)
    if (a ≠ b)
  } yield Edge(a,b)

  val setG = Gen listOf edgeG map (_.toSet)

  val graphG = setG map Graph.apply

  val graphEdgeG = for {
    g ← graphG
    if (g.size > 0)
    e ← Gen oneOf g.edges.toSeq
  } yield (g, e)

  val graphVertexG = for {
    g ← graphG
    if (g.order > 0)
    v ← Gen choose (0, g.order - 1)
  } yield (g, v)

  val graphVerticesG = for {
    g  ← graphG
    if (g.order > 0)
    vs ← Gen someOf List.range(0, g.order)
    if (vs.nonEmpty)
  } yield (g, vs)

  implicit val GraphArbitrary = Arbitrary(graphG)
  implicit val EdgeArbitrary = Arbitrary(edgeG)

  property("addVertex_noArgs") =  forAll {g: Graph ⇒ 
    val ng = g.addVertex

    (ng.order ≟ (g.order + 1)) &&
    (ng isIsolate (ng.order - 1))
  }

  property("addVertex_neighbor") =  forAll(graphVertexG) {p ⇒ 
    val (g, n) = p
    val ng = g addVertex n

    (ng.order ≟ (g.order + 1)) :| "adjusted order" &&
    (ng isPendant (ng.order - 1)) :| "isPendant" &&
    (ng.neighbors(ng.order - 1) ≟ List(n)) :| "neighbors"
  }

  property("addVertex_neighbors") =  forAll(graphVerticesG) {p ⇒ 
    val (g, vs) = (p._1, p._2.toSet)
    val ng = g addVertex vs
    assert (vs.nonEmpty)

    (ng.order ≟ (g.order + 1)) :| "adjusted order" &&
    (ng.degree(ng.order - 1) ≟ vs.size) :| "isPendant" &&
    (ng.neighbors(ng.order - 1).toSet ≟ vs) :| "neighbors"
  }

  property("addVertex_neighbors_empty") =  forAll {g: Graph ⇒ 
    val ng = g addVertex Set.empty[Int]

    (ng.order ≟ g.order) && (ng.edges ≟ g.edges)
  }

  property("addEdge") =  forAll {p: (Graph,Edge) ⇒ 
    val (g,e) = p
    val ng = g addEdge e

    (ng.order ≟ (g.order max (e.b + 1))) &&
    (ng edges e)
  }

  property("addEdges") =  forAll {p: (Graph,Set[Edge]) ⇒ 
    val (g, es) = p
    val ng = g addEdges es

    es forall (e ⇒ ng.edges(e) && ng.order > e.b)
  }

  property("degree") =  forAll {g: Graph ⇒ 
    val dTot = List.range(0, g.order) foldMap g.degree

    (g.size * 2) ≟ dTot
  }

  property("neighbors") =  forAll {g: Graph ⇒ 
    List.range(0, g.order) ∀ (v ⇒ g.degree(v) ≟ g.neighbors(v).size)
  }

  property("order") =  forAll {g: Graph ⇒ 
    val max = g.edges.isEmpty ? 0 | (g.edges.map(_.b).max + 1)

    g.order ≟ max
  }

  property("removeEdge") =  forAll(graphEdgeG) {p ⇒ 
    val (g, e) = p
    val ng = g removeEdge e

    (! ng.edges(e)) && (ng.order ≟ g.order)
  }

  property("removeVertex") =  forAll(graphVertexG) {p ⇒ 
    val (g, v) = p
    val ng = g removeVertex v

    (ng.order ≟ (g.order - 1)) &&
    (ng.size ≟ (g.size - g.degree(v)))
  }
}

// vim: set ts=2 sw=2 et:
