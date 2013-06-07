/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf

/**
 * @author Stefan Höck
 */
import java.io.File
import parser._
import scalaz._, Scalaz._
import graph.iso2

object Main extends testing.Benchmark {
  def run {
    def str = getClass.getResourceAsStream("zinc.txt")
    def source = scala.io.Source fromInputStream str getLines
    val lines = source.toArray.par

    def countImpHs(s: String) = smiles(s) fold (_ ⇒ (0L, s), time(s))

    def res = (lines map countImpHs).toList.sorted.reverse take 100

    println(res mkString "\n")
  }

  def time(s: String)(m: Molecule): (Long, String) = {
    val start = System.nanoTime
    val sol = iso2 solve m.graph
    val stop = System.nanoTime

    (stop - start, s)
  }
}

// vim: set ts=2 sw=2 et:
