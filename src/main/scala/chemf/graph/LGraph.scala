/**                                                      **\
**   Copyright (c) 2012 Organic Chemistry Group          **
**                 Zurich University of Applied Sciences **
**                 Wädenswil, Switzerland                **
\**                                                      **/

package chemf.graph

import annotation.unchecked.uncheckedVariance
import scalaz._, Scalaz._

/**
 * @author Stefan Höck
 */
trait LGraph[E,+V] {

  /**
   * Adds a new, isolated (= no neighbors) vertex to the graph.
   */
  def addVertex[W>:V](w: W): LGraph[E,W] 

  /**
   * Adds a new, pendant (= exactly one neighbor) vertex to the graph.
   */
  def addVertex[W>:V, F>:E](w: W, f: F, neighbor: Int): LGraph[F,W] =
    addVertex(w) addEdge (Edge(neighbor, order), f)

  /**
   * Adds a new edge to the graph. Throws an exception if
   * one of the edges vertices is >= the order of this graph.
   */
  def addEdge [F>:E](e: Edge, f: F): LGraph[F,V]

  /**
   * Returns the label for vertex v. Throws an IndexOutOfBounds exception
   * if v is not within the interval 0 <= v < order.
   */
  def apply (i: Int): V = vLabel (i)

  /**
   * Returns the set of all edges of this graph.
   */
  def edges: Set[Edge] = graph.edges

  /**
   * Returns an unsorted List of all edge labels connected
   * to vertex v.
   */
  def edgesTo (v: Int): List[E] = edgeList(v)

  /**
   * Changes all edges labels by applying function f.
   */
  def emap[B] (f: E ⇒ B): LGraph[B,V]

  /**
   * Returns the label for edge e. Throws an exception if
   * e is not an edge of this graph.
   */
  def eLabel (e: Edge): E

  /**
   * Right fold over all vertex labels. Similar to foldRight
   * found for typical container classes like List or Seq.
   */
  def foldRight[B](z: ⇒ B, f: (V, ⇒ B) ⇒ B): B

  /**
   * Left fold over all vertex labels. Similar to foldLeft
   * found for typical container classes like List or Seq.
   */
  def foldLeft[B](z: B, f: (B,V) ⇒ B): B

  /**
   * Returns the underlying graph.
   */
  def graph: Graph

  /**
   * Changes all vertex labels by applying function f.
   */
  def map[B] (f: V ⇒ B): LGraph[E,B] = mapI ((v,_) ⇒ f(v))

  /**
   * Changes all vertex labels by applying function f. The
   * second argument of function f comes from a vertex' index.
   */
  def mapI[B] (f: (V,Int) ⇒ B): LGraph[E,B]

  /**
   * The order (= number of vertices) of the graph
   */
  def order: Int = graph.order

  /**
   * Removes an edge (together with its label) from the graph
   */
  def removeEdge (e: Edge): LGraph[E,V]

  /**
   * Used for implementing type-class Traverse. See 'The Essence of
   * the Iterator Pattern' for applications.
   */
  def traverse[F[_] : Applicative,B](f: V => F[B]): F[LGraph[E,B]]

  /**
   * The label of vertex v. Throws an exception if v is not in the interval
   * 0 <= v < order.
   */
  def vLabel (v: Int): V

  def :+ [W>:V](w: W): LGraph[E,W] = addVertex(w)

  private[this] lazy val edgeList: Array[List[E]] @uncheckedVariance =
    LGraph edgeList this
}

object LGraph {
  def empty[E,V]: LGraph[E,V] = apply(IndexedSeq.empty)

  def apply[E,V] (vs: IndexedSeq[V], es: Map[Edge,E]): LGraph[E,V] =
    LgImpl (Graph(vs.length, es.keySet), vs, es)

  def apply[E,V] (vs: IndexedSeq[V], es: (Edge,E)*): LGraph[E,V] =
    apply(vs, es.toMap)

  private[LGraph] def edgeList[E,V] (lg: LGraph[E,V]): Array[List[E]] = {
    def edgesFor (i: Int) =
      lg.graph neighbors i map (n ⇒ lg eLabel Edge(n,i))

    Array.tabulate(lg.order)(edgesFor)
  }

  /** Implementing Class **/

  private case class LgImpl[E,+V] (
    graph: Graph, vertices: IndexedSeq[V], eMap: Map[Edge,E]
  ) extends LGraph[E,V] {
    def addVertex[W>:V](w: W) = LgImpl(graph.addVertex, vertices :+ w, eMap)

    def addEdge[F>:E](e: Edge, f: F) = {
      require(e.b < order, "Vertex %d is not part of this graph" format e.b)

      LgImpl(graph addEdge e, vertices, eMap + (e → f))
    }

    def eLabel (e: Edge) = eMap (e)

    override def map[B] (f: V ⇒ B) = LgImpl (graph, vertices map f, eMap)

    override def mapI[B] (f: (V,Int) ⇒ B) =
      LgImpl (graph, vertices.zipWithIndex map f.tupled, eMap)

    def emap[B] (f: E ⇒ B): LGraph[B,V] =
      new LgImpl (graph, vertices, eMap map {case (e,x) ⇒ (e, f(x))})

    def foldRight[B](z: ⇒ B, f: (V, ⇒ B) ⇒ B): B = {
      //implementation prevents stack overflow
      import scala.collection.mutable.ArrayStack
      val s = new ArrayStack[V]
      vertices.foreach(a ⇒ s += a)
      var r = z
      while (!s.isEmpty) {r = f(s.pop, r)}
      r
    }

    def foldLeft[B](z: B, f: (B,V) ⇒ B): B =
      vertices.foldLeft (z)(f)

    def removeEdge (e: Edge): LGraph[E,V] =
      LgImpl(graph - e, vertices, eMap - e)

    def traverse[F[_]:Applicative,B](f: V ⇒ F[B]): F[LGraph[E,B]] = {
      val a = implicitly[Apply[F]]
      val newVs = vertices.foldLeft(IndexedSeq.empty[B].η)(
        (ys, x) ⇒ a(f(x) ∘ ((b: B) ⇒ (bs: IndexedSeq[B]) ⇒ bs :+ b), ys)
      )

      newVs ∘ (LgImpl(graph, _, eMap))
    }

    def vLabel (v: Int) = vertices (v)
  }

  /** Type Classes **/

  implicit def LGraphFunctor[E] =
    new Functor[({type λ[α]=LGraph[E,α]})#λ] {
      def fmap[A,B] (f: LGraph[E,A], g: A ⇒ B) = f map g
    }

  implicit def LGraphFoldable[E] =
    new Foldable[({type λ[α]=LGraph[E,α]})#λ] {
      override def foldRight[A, B](t: LGraph[E,A], z: ⇒ B, f: (A, ⇒ B) ⇒ B)
      : B = t.foldRight(z, f)

      override def foldLeft[A, B](t: LGraph[E,A], z: B, f: (B,A) ⇒ B): B =
        t.foldLeft (z, f)
    }

  implicit def LGraphTraverse[E] =
    new Traverse[({type λ[α]=LGraph[E,α]})#λ] {
      def traverse[F[_] : Applicative, A, B](f: A ⇒ F[B], t: LGraph[E,A]) =
        t traverse f
    }

  implicit def LGraphShow[E:Show,V:Show] = shows[LGraph[E,V]]{g ⇒ 
    def vertice (i: Int) = "%d: %s" format (i, g vLabel i)
    def edge (e: Edge) = "%d - %d: %s" format (e.a, e.b, g eLabel e shows)

    val vertices = 0 until g.order map vertice mkString "\n"
    val edges = g.edges.toSeq.sorted map edge mkString "\n"

    "LGraph: \n%s\n\n%s" format (vertices, edges)
  }
}

// vim: set ts=2 sw=2 et:
