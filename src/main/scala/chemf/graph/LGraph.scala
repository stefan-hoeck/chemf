/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf.graph

import collection.immutable.{IndexedSeq ⇒ IxSq}
import annotation.unchecked.uncheckedVariance
import scalaz._, Scalaz._, scalaz.std.indexedSeq._

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
  def foldRight[B](z: ⇒ B)(f: (V, ⇒ B) ⇒ B): B

  /**
   * Left fold over all vertex labels. Similar to foldLeft
   * found for typical container classes like List or Seq.
   */
  def foldLeft[B](z: B)(f: (B,V) ⇒ B): B

  def foldMap[B:Monoid](f: V ⇒ B): B =
    foldLeft(∅[B])((b,v) ⇒ b ⊹ f(v))

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

  def neighbors(v: Int): List[Int] = graph neighbors v

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

  private[graph] def edgeList: Array[List[E]] @uncheckedVariance
}

object LGraph {
  def empty[E,V]: LGraph[E,V] = apply(IxSq.empty)

  def apply[E,V](vs: IxSq[V], es: Map[Edge,E]): LGraph[E,V] =
    LgImpl (Graph(vs.length, es.keySet), vs, es)

  def apply[E,V](vs: IxSq[V], es: (Edge,E)*): LGraph[E,V] =
    apply(vs, es.toMap)

  def dijkstra[E,V](g: LGraph[E,V])(start: Int, weight: E ⇒ Long)
    : (Array[Long],Array[Int]) = {

    val min = Array.fill(g.order)(Long.MaxValue)
    val visited = Array.fill(g.order)(false)
    val p = Array.fill(g.order)(-1)
    var h = Heap singleton (0L, start)
    min(start) = 0L

    while (! h.isEmpty) {
      val ((l, i), rest) = h.uncons.get
      h = rest
      if (!visited(i)) {
        visited(i) = true
        g neighbors i foreach { n ⇒ 
          val w = l + weight(g eLabel Edge(i, n))
          if (w < min(n)) { min(n) = w; p(n) = i; h = h insert (w, n) }
        }
      }
    }

    (min, p)
  }

  private[LGraph] def edgeList[E,V] (lg: LgImpl[E,V]): Array[List[E]] = {
    val res = Array.fill[List[E]](lg.order)(Nil)

    def add(p: (Edge,E)) {
      res(p._1.a) = p._2 :: res(p._1.a)
      res(p._1.b) = p._2 :: res(p._1.b)
    }

    lg.eMap foreach add

    res
  }

  /** Implementing Class **/

  private case class LgImpl[E,+V] (
    graph: Graph, vertices: IxSq[V], eMap: Map[Edge,E]
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

    def foldRight[B](z: ⇒ B)(f: (V, ⇒ B) ⇒ B): B = {
      //implementation prevents stack overflow
      import scala.collection.mutable.ArrayStack
      val s = new ArrayStack[V]
      vertices.foreach(a ⇒ s += a)
      var r = z
      while (!s.isEmpty) {r = f(s.pop, r)}
      r
    }

    def foldLeft[B](z: B)(f: (B,V) ⇒ B): B =
      vertices.foldLeft(z)(f)

    def removeEdge (e: Edge): LGraph[E,V] =
      LgImpl(graph - e, vertices, eMap - e)

    def traverse[F[_]:Applicative,B](f: V ⇒ F[B]): F[LGraph[E,B]] =
      vertices traverse f map (LgImpl(graph, _, eMap))

    def vLabel (v: Int) = vertices (v)

    private[graph] lazy val edgeList: Array[List[E]] @uncheckedVariance =
      LGraph edgeList this
  }

  /** Type Classes **/

  implicit def LGraphTraverse[E] = new Traverse[({type λ[α]=LGraph[E,α]})#λ] {
    def traverseImpl[G[_]:Applicative,A,B](fa: LGraph[E,A])(f: A => G[B]) =
      fa traverse f

    override def foldLeft[A,B](fa: LGraph[E,A], z: B)(f: (B,A) => B): B =
      fa.foldLeft(z)(f)

    override def foldMap[A,B](fa: LGraph[E,A])(f: A => B)(implicit F: Monoid[B]): B =
      fa foldMap f

    override def foldRight[A, B](fa: LGraph[E,A], z: => B)(f: (A, => B) => B) =
      fa.foldRight(z)(f)

    override def map[A, B](fa: LGraph[E,A])(f: A => B) = fa map f
  }

  implicit def LGraphShow[E:Show,V:Show] = Show.shows[LGraph[E,V]]{g ⇒ 
    def vertice (i: Int) = "%d: %s" format (i, g vLabel i)
    def edge (e: Edge) = "%d - %d: %s" format (e.a, e.b, g eLabel e shows)

    val vertices = 0 until g.order map vertice mkString "\n"
    val edges = g.edges.toSeq.sorted map edge mkString "\n"

    "LGraph: \n%s\n\n%s" format (vertices, edges)
  }
}

// vim: set ts=2 sw=2 et:
