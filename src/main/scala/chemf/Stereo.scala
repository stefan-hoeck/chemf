package chemf

import scalaz._, Scalaz._

/**
 * Stereo descriptors in SMILES strings
 */
sealed trait Stereo {
  val symbol = "@" + toString
}

object Stereo {

  case object Undefined extends Stereo {
    override val symbol = ""
  }

  case object CW extends Stereo {
    override val symbol = "@"
  }

  case object AW extends Stereo {
    override val symbol = "@@"
  }

  case object AL1 extends Stereo
  case object AL2 extends Stereo

  case object TH1 extends Stereo
  case object TH2 extends Stereo

  case object SP1 extends Stereo
  case object SP2 extends Stereo
  case object SP3 extends Stereo

  case object TB1 extends Stereo
  case object TB2 extends Stereo
  case object TB3 extends Stereo
  case object TB4 extends Stereo
  case object TB5 extends Stereo
  case object TB6 extends Stereo
  case object TB7 extends Stereo
  case object TB8 extends Stereo
  case object TB9 extends Stereo
  case object TB10 extends Stereo
  case object TB11 extends Stereo
  case object TB12 extends Stereo
  case object TB13 extends Stereo
  case object TB14 extends Stereo
  case object TB15 extends Stereo
  case object TB16 extends Stereo
  case object TB17 extends Stereo
  case object TB18 extends Stereo
  case object TB19 extends Stereo
  case object TB20 extends Stereo

  case object OH1 extends Stereo
  case object OH2 extends Stereo
  case object OH3 extends Stereo
  case object OH4 extends Stereo
  case object OH5 extends Stereo
  case object OH6 extends Stereo
  case object OH7 extends Stereo
  case object OH8 extends Stereo
  case object OH9 extends Stereo
  case object OH10 extends Stereo
  case object OH11 extends Stereo
  case object OH12 extends Stereo
  case object OH13 extends Stereo
  case object OH14 extends Stereo
  case object OH15 extends Stereo
  case object OH16 extends Stereo
  case object OH17 extends Stereo
  case object OH18 extends Stereo
  case object OH19 extends Stereo
  case object OH20 extends Stereo
  case object OH21 extends Stereo
  case object OH22 extends Stereo
  case object OH23 extends Stereo
  case object OH24 extends Stereo
  case object OH25 extends Stereo
  case object OH26 extends Stereo
  case object OH27 extends Stereo
  case object OH28 extends Stereo
  case object OH29 extends Stereo
  case object OH30 extends Stereo

  val values = Seq[Stereo] (Undefined, AW, CW, AL1, AL2, SP1, SP2, SP3,
    TH1, TH2, TB1, TB2, TB3, TB4, TB5, TB6, TB7, TB8, TB9, TB10,
    TB11, TB12, TB13, TB14, TB15, TB16, TB17, TB18, TB19, TB20, 
    OH1, OH2, OH3, OH4, OH5, OH6, OH7, OH8, OH9, OH10,
    OH11, OH12, OH13, OH14, OH15, OH16, OH17, OH18, OH19, OH20, 
    OH21, OH22, OH23, OH24, OH25, OH26, OH27, OH28, OH29, OH30 
  )

  private val map = (values map (v ⇒ v.symbol → v) toMap) +
                    ("@1" → AW) +
                    ("@2" → CW)

  def fromSymbol (s: String): Option[Stereo] = map get s

  def fromSymbolV (s: String): ValRes[Stereo] =
    fromSymbol(s) toSuccess ("Unknown stereo type: " + s).wrapNel

  val pattern = """(@|@@|@1|@2|@AL1|@AL2|@TH1|@TH2|@SP1|@SP2|@SP3|""" +
                """@TB[1-9]|@TB1\d|@TB20|""" +
                """@OH[1-9]|@OH[12]\d|@OH30)?"""

  val regexp = pattern.r

  implicit val StereoEqual = equalA[Stereo]

  implicit val StereoShow = shows[Stereo](_.symbol)
}

// vim: set ts=2 sw=2 et:
