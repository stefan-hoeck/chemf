/**                                                               **\
**  Copyright (c) 2012 Center for Organic and Medicinal Chemistry  **
**                Zurich University of Applied Sciences            **
**                Wädenswil, Switzerland                           **
\**                                                               **/

package chemf

/**
 * @author Stefan Höck
 */
import chemf.io._, ChemfIO._
import collection.immutable.{IndexedSeq ⇒ IxSq}
import collection.parallel.ForkJoinTaskSupport
import graph.LGraph.dijkstra
import java.io.File
import parser._
import scalaz._, Scalaz._, effect.{IO, SafeApp}, std.indexedSeq._
import scalaz.iteratee._, Iteratee._

object Main extends SafeApp { //extends testing.Benchmark {
  private val fs = System.getProperty("file.separator")
  private val home = System.getProperty("user.home")
  private val desk = home + fs + "Desktop/"
  private val files = desk + "zincFiles/"

  private val throb = throbber[IxSq[String]](1000) mapI disTo
  private val total = throb ⊹ linesOut(s"${desk}headers.txt")
  private val head = mapper[IxSq[IxSq[String]],IxSq[String],DisIO](_ map (_.head))
  private def group[A](i: Int) = EnumerateeT.group[A,IxSq,DisIO](i)

  override def runc: IO[Unit] = {
    val enum = paths foldMap sdfLines mapE group(100) mapE head

    consoleRun(total &= enum run)
  }

  private def paths = 
    new File(files).list.toList.sorted filter (_ endsWith ".sdf") map (files + _)

//  def run {
//    val ts = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(9))
//    def str = getClass.getResourceAsStream("zinc.txt")
//    def source = scala.io.Source fromInputStream str getLines
//    val lines = source.toArray.par
//
//    def weight(b: Bond) = b match {
//      case Bond.Aromatic ⇒ 2
//      case x        ⇒ x.valence
//    }
//
//    def maxFromZero(m: Molecule) = dijkstra(m)(0, weight)._1.max
//    def countImpHs(s: String) = smiles(s) fold (_ ⇒ 0L, maxFromZero)
//
//    lines.tasksupport = ts
//    def res = lines map countImpHs max
//
//    println(res)
//  }
}

// vim: set ts=2 sw=2 et:
