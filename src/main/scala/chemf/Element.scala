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
sealed abstract class Element (val atomicNr: Int) {

  private val data = EData.dataMap get atomicNr

  val electroNegativity: Option[Double] = data >>= (_.en)

  val electronAffinity: Option[Double] = data >>= (_.ea)

  val exactMass: Option[Double] = data >>= (_.exactMass)

  lazy val isotopes: Seq[Isotope] =
    (IsotopeData isotopes this).keySet.toSeq.sorted map (Isotope(this, _))

  lazy val isotopeDist: Seq[(Isotope,Double)] = for {
    i ← isotopes
    d ← i.iData
    a ← d.abundance
  } yield i → a

  val mass: Option[Double] = data >>= (_.mass)

  val name: String = data fold (_.name, "")

  val radiusCovalent: Option[Double] = data >>= (_.rCovalent)

  val radiusVdw: Option[Double] = data >>= (_.rVdw)

  final def symbol = toString
}

object Element {
  case object Xx extends Element (0)
  case object H extends Element (1)
  case object He extends Element (2)

  case object Li extends Element (3)
  case object Be extends Element (4)
  case object B extends Element (5)
  case object C extends Element (6)
  case object N extends Element (7)
  case object O extends Element (8)
  case object F extends Element (9)
  case object Ne extends Element (10)

  case object Na extends Element (11)
  case object Mg extends Element (12)
  case object Al extends Element (13)
  case object Si extends Element (14)
  case object P extends Element (15)
  case object S extends Element (16)
  case object Cl extends Element (17)
  case object Ar extends Element (18)

  case object K extends Element (19)
  case object Ca extends Element (20)
  case object Sc extends Element (21)
  case object Ti extends Element (22)
  case object V extends Element (23)
  case object Cr extends Element (24)
  case object Mn extends Element (25)
  case object Fe extends Element (26)
  case object Co extends Element (27)
  case object Ni extends Element (28)
  case object Cu extends Element (29)
  case object Zn extends Element (30)
  case object Ga extends Element (31)
  case object Ge extends Element (32)
  case object As extends Element (33)
  case object Se extends Element (34)
  case object Br extends Element (35)
  case object Kr extends Element (36)

  case object Rb extends Element (37)
  case object Sr extends Element (38)
  case object Y extends Element (39)
  case object Zr extends Element (40)
  case object Nb extends Element (41)
  case object Mo extends Element (42)
  case object Tc extends Element (43)
  case object Ru extends Element (44)
  case object Rh extends Element (45)
  case object Pd extends Element (46)
  case object Ag extends Element (47)
  case object Cd extends Element (48)
  case object In extends Element (49)
  case object Sn extends Element (50)
  case object Sb extends Element (51)
  case object Te extends Element (52)
  case object I extends Element (53)
  case object Xe extends Element (54)
  
  case object Cs extends Element (55)
  case object Ba extends Element (56)
  case object La extends Element (57)
  case object Ce extends Element (58)
  case object Pr extends Element (59)
  case object Nd extends Element (60)
  case object Pm extends Element (61)
  case object Sm extends Element (62)
  case object Eu extends Element (63)
  case object Gd extends Element (64)
  case object Tb extends Element (65)
  case object Dy extends Element (66)
  case object Ho extends Element (67)
  case object Er extends Element (68)
  case object Tm extends Element (69)
  case object Yb extends Element (70)
  case object Lu extends Element (71)
  case object Hf extends Element (72)
  case object Ta extends Element (73)
  case object W extends Element (74)
  case object Re extends Element (75)
  case object Os extends Element (76)
  case object Ir extends Element (77)
  case object Pt extends Element (78)
  case object Au extends Element (79)
  case object Hg extends Element (80)
  case object Tl extends Element (81)
  case object Pb extends Element (82)
  case object Bi extends Element (83)
  case object Po extends Element (84)
  case object At extends Element (85)
  case object Rn extends Element (86)

  case object Fr extends Element (87)
  case object Ra extends Element (88)
  case object Ac extends Element (89)
  case object Th extends Element (90)
  case object Pa extends Element (91)
  case object U extends Element (92)
  case object Np extends Element (93)
  case object Pu extends Element (94)
  case object Am extends Element (95)
  case object Cm extends Element (96)
  case object Bk extends Element (97)
  case object Cf extends Element (98)
  case object Es extends Element (99)
  case object Fm extends Element (100)
  case object Md extends Element (101)
  case object No extends Element (102)
  case object Lr extends Element (103)
  case object Rf extends Element (104)
  case object Db extends Element (105)
  case object Sg extends Element (106)
  case object Bh extends Element (107)
  case object Hs extends Element (108)
  case object Mt extends Element (109)
  case object Ds extends Element (110)
  case object Rg extends Element (111)
  case object Cn extends Element (112)
  case object Uut extends Element (113)
  case object Fl extends Element (114)
  case object Uup extends Element (115)
  case object Lv extends Element (116)
  case object Uus extends Element (117)
  case object Uuo extends Element (118)

  val values = List[Element](
    Xx, H, He, Li, Be, B, C, N, O, F, Ne, Na, Mg, Al, Si, P, S, Cl, Ar, K, Ca,
    Sc, Ti, V, Cr, Mn, Fe, Co, Ni, Cu, Zn, Ga, Ge, As, Se, Br, Kr, Rb, Sr,
    Y, Zr, Nb, Mo, Tc, Ru, Rh, Pd, Ag, Cd, In, Sn, Sb, Te, I, Xe, Cs, Ba, La,
    Ce, Pr, Nd, Pm, Sm, Eu, Gd, Tb, Dy, Ho, Er, Tm, Yb, Lu, Hf, Ta, W, Re,
    Os, Ir, Pt, Au, Hg, Tl, Pb, Bi, Po, At, Rn, Fr, Ra, Ac, Th, Pa, U, Np,
    Pu, Am, Cm, Bk, Cf, Es, Fm, Md, No, Lr, Rf, Db, Sg, Bh, Hs, Mt, Ds, Rg,
    Cn, Uut, Fl, Uup, Lv, Uus, Uuo
  ) sortBy (_.atomicNr)

  def fromNr (i: Int) = nrMap(i)

  def fromSymbol (s: String) = symbolMap get s.toLowerCase

  def fromSymbolV (s: String): ValRes[Element] = {
    def msg = "Unknown element: " + s
    fromSymbol(s) toSuccess msg.wrapNel
  }

  private[this] val nrMap = values.toArray

  val symbolMap: Map[String,Element] =
    values map (e ⇒ (e.symbol.toLowerCase, e)) toMap

  implicit val ElementEqual: Equal[Element] = Scalaz.equalA
}

private[chemf] case class EData(
  id: Int,
  name: String = "",
  mass: Option[Double] = None,
  exactMass: Option[Double] = None,
  ionization: Option[Double] = None,
  ea: Option[Double] = None,
  en: Option[Double] = None,
  rCovalent: Option[Double] = None,
  rVdw: Option[Double] = None
)

private[chemf] object EData {

  val dataMap: Map[Int,EData] = {
    def single (ns: Seq[Node]): EData = {
    
      def scalarToEndo (ns: Seq[Node]): Endo[EData] =
        ns \ "@dictRef" text match {
          case "bo:atomicNumber" ⇒
            EndoTo(_ copy (id = ns.text.toInt))
          case "bo:mass" ⇒
            EndoTo(_ copy (mass = ns.text.toDouble.some))
          case "bo:exactMass" ⇒
            EndoTo(_ copy (exactMass = ns.text.toDouble.some))
          case "bo:ionization" ⇒
            EndoTo(_ copy (ionization = ns.text.toDouble.some))
          case "bo:electronAffinity" ⇒
            EndoTo(_ copy (ea = ns.text.toDouble.some))
          case "bo:electronegativityPauling" ⇒
            EndoTo(_ copy (en = ns.text.toDouble.some))
          case "bo:radiusCovalent" ⇒
            EndoTo(_ copy (rCovalent = ns.text.toDouble.some))
          case "bo:radiusVDW" ⇒
            EndoTo(_ copy (rVdw = ns.text.toDouble.some))
          case _                 ⇒ EndoTo(identity)
        }

      def lblToEndo (ns: Seq[Node]): Endo[EData] =
        ns \ "@dictRef" text match {
          case "bo:name" ⇒ EndoTo(_ copy (name = ns \ "@value" text))
          case _         ⇒ EndoTo(identity)
        }

      def scalars = (ns \ "scalar") foldMap scalarToEndo
      def lbls = (ns \ "label") foldMap lblToEndo

      scalars ⊹ lbls apply EData(0)
    }

    def elems = XML.load(getClass.getResourceAsStream("elements.xml"))

    def toPair (e: EData) = (e.id, e)

    (elems \ "atom") map (n ⇒ toPair(single(n))) toMap
  }
}

// vim: set ts=2 sw=2 et:
