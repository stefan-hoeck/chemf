/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf

/**
 * @author Stefan Höck
 */
import collection.parallel.ForkJoinTaskSupport
import parser._
import graph.LGraph.dijkstra
import scalaz._, Scalaz._

object Main extends testing.Benchmark {
  def run {
    val ts = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(9))
    def str = getClass.getResourceAsStream("zinc.txt")
    def source = scala.io.Source fromInputStream str getLines
    val lines = source.toArray.par

    def weight(b: Bond) = b match {
      case Bond.Aromatic ⇒ 2
      case x        ⇒ x.valence
    }

    def maxFromZero(m: Molecule) = dijkstra(m)(0, weight)._1.max
    def countImpHs(s: String) = smiles(s) fold (_ ⇒ 0L, maxFromZero)

    lines.tasksupport = ts
    def res = lines map countImpHs max

    println(res)
  }
}

// vim: set ts=2 sw=2 et:
