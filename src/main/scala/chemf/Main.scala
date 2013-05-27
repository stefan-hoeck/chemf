/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf

/**
 * @author Stefan Höck
 */
import collection.immutable.{IndexedSeq ⇒ IxSq}
import collection.parallel.ForkJoinTaskSupport
import java.io.File
import parser._
import scalaz._, Scalaz._

object Main extends testing.Benchmark {
  def run {
    val ts = new ForkJoinTaskSupport(
      new scala.concurrent.forkjoin.ForkJoinPool(4))
    def str = getClass.getResourceAsStream("zinc.txt")
    def source = scala.io.Source fromInputStream str getLines
    val lines = source.toArray.par

    def countImpHs(s: String) = smiles(s) fold (_ ⇒ 0, _ foldMap (_.hydrogens))

    lines.tasksupport = ts
    def res = lines map countImpHs sum

    println(res)
  }
}

// vim: set ts=2 sw=2 et:
