/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf

import collection.immutable.IntMap
import scala.xml.{Node, XML}
import scalaz._, Scalaz._

/**
 * @author Stefan Höck
 */
sealed trait Isotope {
  def element: Element

  def massNr: Option[Int]

  lazy val iData = massNr >>= (IsotopeData get (element, _))

  lazy val mass = iData flatMap (_.exactMass) orElse element.mass

  lazy val exactMass = iData flatMap (_.exactMass) orElse element.exactMass

  override def toString =
    massNr cata (_ + element.toString, element.toString)

  lazy val isotopeDist: Seq[(Isotope,Double)] = iData cata (
    _.abundance cata (a ⇒ Seq(this → a), Seq.empty),
    element.isotopeDist
  )

  lazy val formulaDist: Seq[(Formula,Double)] =
    isotopeDist map {case (i,d) ⇒ (Map(i → 1), d)}
}

object Isotope {

  def apply (e: Element): Isotope = elems(e.atomicNr)

  def apply (e: Element, mn: Int): Isotope = Impl (e, Some(mn))

  def apply (e: Element, mn: Option[Int]): Isotope =
    mn cata (apply(e, _), apply(e))

  implicit val IsotopeEqual: Equal[Isotope] =
    Equal equalBy (i ⇒ (i.element, i.massNr))

  implicit val IsotopeShow: Show[Isotope] = Show.shows(_.toString)

  private case class Impl(element: Element, massNr: Option[Int])
    extends Isotope

  //Flyweight isotopes for all elements
  private[this] val elems = Element.values map (Impl(_, none)) toArray
}

// vim: set ts=2 sw=2 et:
