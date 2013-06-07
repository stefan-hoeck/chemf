/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

import graph.{Edge, LGraph}
import scalaz._, Scalaz._

/**
 * @author Stefan Höck
 */
package object chemf {

  type Formula = Map[Isotope,Int]

  type Molecule = LGraph[Bond,Atom]

  type ValNel[+E,+A] = Validation[NonEmptyList[E],A]

  type ValRes[+A] = ValNel[String,A]

  type DisRes[+A] = String \/ A

  /**
   * Adjust all error messages (if any) in v by applying function f.
   */
  def mapErr[E,F,A](v: ValNel[E,A])(f: E ⇒ F): ValNel[F,A] =
    Bifunctor[Validation].leftMap(v)(_ map f)
}

// vim: set ts=2 sw=2 et:
