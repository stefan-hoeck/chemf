/**                                                      **\
**   Copyright (c) 2012 Organic Chemistry Group          **
**                 Zurich University of Applied Sciences **
**                 Wädenswil, Switzerland                **
\**                                                      **/

package chemf.graph

import scalaz.{Scalaz, Equal}

/**
 * An edge in an unlabeled undirected graph
 *
 * The connected vertices are given by the integers
 * a and b.
 *
 * It is an invariant of this Edge-implementation that
 * for all edges b >= a
 *
 * @author Stefan Höck
 */
final class Edge private (val a: Int, val b: Int) extends Ordered[Edge] {
  /**
   * Returns true if this edge connects the given vertex v.
   */
  def connects (v: Int) =  v == a || v == b

  /**
   * Returns the neighbor of the given vertex v, if this
   * edge is adjacent to v. Otherwise returns None.
   */
  def neighborOf (v: Int): Option[Int] = v match {
    case `a` ⇒ Some(b)
    case `b` ⇒ Some(a)
    case _   ⇒ None
  }

  /**
   * Defines the natural ordering of edges.
   * The implementation first compares vertices a and then vertices b.
   */
  def compare (that: Edge) =
    if (a == that.a) b compare that.b else a compare that.a

  override val hashCode = a + b * 7919

  override def equals (that: Any) = that match {
    case e: Edge ⇒ e.a == a && e.b == b
    case _       ⇒ false
  }
}

object Edge {

  /**
   * Returns an instance of class Edge connecting
   * vertices a and b
   */
  def apply (a: Int, b: Int): Edge = {
    require (a >= 0 && b >= 0)

    if (a < 250 && b < 250) cache(a)(b)
    else create(a, b)
  }

  private def create (a: Int, b: Int): Edge = {
    if (a < b) new Edge(a, b) else new Edge (b, a)
  }

  implicit val EdgeEqual: Equal[Edge] = Scalaz equalBy (e ⇒ (e.a, e.b))

  private[this] val cache = Array.tabulate(250, 250)(create)

}

// vim: set ts=2 sw=2 et:
