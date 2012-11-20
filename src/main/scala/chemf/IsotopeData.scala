/**                                                      **\
**   Copyright (c) 2012 Organic Chemistry Group          **
**                 Zurich University of Applied Sciences **
**                 Wädenswil, Switzerland                **
\**                                                      **/

package chemf

import collection.immutable.IntMap
import scala.xml.{Node, XML}
import scalaz._, Scalaz._

/**
 * @author Stefan Höck
 */
case class IsotopeData (
  massNr: Int,
  atomicNr: Int = 0,
  abundance: Option[Double] = None,
  exactMass: Option[Double] = None
)

object IsotopeData {

  def get (e: Element, massNr: Int): Option[IsotopeData] =
    isotopes(e) get massNr

  def isotopes (e: Element): Map[Int,IsotopeData] = try {
    data(e.atomicNr)
  } catch {case e: IndexOutOfBoundsException ⇒ Map.empty}

  private[this] val data: Array[Map[Int,IsotopeData]] = {
    def single (ns: Seq[Node]): IsotopeData = {
    
      def scalarToEndo (ns: Seq[Node]): Endo[IsotopeData] =
        ns \ "@dictRef" text match {
          case "bo:atomicNumber" ⇒
            EndoTo(_ copy (atomicNr = ns.text.toInt))
          case "bo:exactMass" ⇒
            EndoTo(_ copy (exactMass = ns.text.toDouble.some))
          case "bo:relativeAbundance" ⇒
            EndoTo(_ copy (abundance = (ns.text.toDouble / 100D).some))
          case _ ⇒ EndoTo(identity)
        }

      (ns \ "scalar") foldMap scalarToEndo apply
      IsotopeData((ns \ "@number" text) toInt)
    }

    def isos = XML load getClass.getResourceAsStream("isotopes.xml")

    val res = Array.fill(Element.values.size)(
      IntMap.empty[IsotopeData]: Map[Int,IsotopeData]
    )

    (isos \ "isotopeList" \ "isotope") map single foreach
      {d ⇒ res(d.atomicNr) = res(d.atomicNr) + (d.massNr → d)}

    res
  }
}

// vim: set ts=2 sw=2 et:
