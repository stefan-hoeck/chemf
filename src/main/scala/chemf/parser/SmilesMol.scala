package chemf.parser

import chemf.graph.{Edge, LGraph}
import chemf._, Bond._, Element._
import scalaz._, Scalaz._

case class SmilesMol (
  atoms: IndexedSeq[SmilesAtom],
  bonds: List[(Edge,Bond)],
  stack: List[SmilesMol.AtomInfo] = Nil,
  keep: Boolean = false,
  bond: Option[Bond] = None,
  dbStereo: Option[Char] = None,
  rings: SmilesMol.Rings = Map.empty
) {
  import SmilesMol.{Rings, AtomInfo}

  def apply (i: Int): SmilesAtom = atoms(i)

  def addAtom (a: SmilesAtom, aromatic: Boolean) = copy(
    atoms = atoms :+ a,
    keep = false,
    stack = (keep, stack) match {
      case (false, a::as) ⇒ (order, aromatic) :: as
      case (_, as)        ⇒ (order, aromatic) :: as
    }
  )

  def addBond (x: AtomInfo, y: AtomInfo, b: Option[Bond] = None) = {
    def bnd = bond orElse b match {
      case None if (x._2 && y._2) ⇒ Aromatic
      case None                   ⇒ Single
      case Some(a)                ⇒ a
    }

    copy (bonds = (Edge(x._1, y._1), bnd) :: bonds).noBond.success
  }

  def closeBranch: ValRes[SmilesMol] = stack match {
    case a::as ⇒ copy(stack = as).success
    case _ ⇒ "No branch opened".failNel
  }

  def modRings (f: Rings ⇒ Rings) = copy (rings = f(rings))

  def noBond = copy(bond = None)

  def openBranch = copy(keep = true)

  def order = atoms.size
}

object SmilesMol {
  /**
   * Index of atom plus boolean flag representing aromaticity
   */
  type AtomInfo = (Int, Boolean)

  /**
   * Atom info plus option type of bond to the atom in question
   */
  type RingInfo = (AtomInfo, Option[Bond])

  /**
   * Maping from ring index to ring info
   */
  type Rings = Map[Int,RingInfo]

  /**
   * Transforms a SmilesMol to a Molecule by calculating
   * the implicit hydrogens for each atom. Aromaticity and
   * stereocenters are ignored by this function.
   */
  def toMolecule (m: SmilesMol): ValRes[Molecule] = {
    val graph = LGraph(m.atoms, m.bonds: _*)
    def toAtom (a: SmilesAtom, i: Int): ValRes[Atom] = {
      def hs = SmilesAtom implicitHydrogens (graph edgesTo i, a.element)
      def toAtom (hs: Int) = Atom(a.isotope, a.charge, hs, a.stereo)

      a.hydrogens fold (toAtom(_).success, hs map toAtom)
    }
    
    graph mapI toAtom sequence
  }

  /**
   * SmilesBuilder implementation. Does not yet provide error messages
   * for all possibly invalid SMILES strings. For instance, unclosed
   * braces, several successive braces, several successive bonds, 
   * and unclosed rings are all accepted silently.
   */
  implicit val SmilesMolBuilder = new SmilesBuilder[SmilesMol] {
    val empty = SmilesMol(IndexedSeq.empty, Nil)
    val clear: STrans = m ⇒ SmilesMol(m.atoms, m.bonds, rings = m.rings).success
    val closeBranch: STrans = _.closeBranch
    val openBranch: STrans = _.openBranch.success
    def setBond (b: Bond): STrans = _.copy(bond = Some(b)).success
    def setDbStereo (c: Char): STrans = _.copy(dbStereo = c.some).success

    def addAtom (
      i: Isotope, c: Int, h: Option[Int], a: Boolean, s: Stereo, ac: Int
    ) = m ⇒ 
      m.addAtom(SmilesAtom(i, c, h, s, ac), a) |>
      (n ⇒ m.stack.headOption fold (n addBond (_, (m.order, a)), n.success))

    def ring (i: Int) = m ⇒ (m.rings get i, m.stack.headOption) match {
      case (Some((x, bo)), Some(y)) ⇒ m modRings (_ - i) addBond (x, y, bo)
      case (None, Some(y)) ⇒ m.modRings (_ + i → (y, m.bond)).noBond.success
      case (_, None) ⇒ "Atom stack empty when opening ring.".failNel
    }
  }
}

// vim: set ts=2 sw=2 et:
