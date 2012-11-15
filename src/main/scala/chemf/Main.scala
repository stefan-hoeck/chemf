package chemf

import collection.parallel.ForkJoinTasks.{defaultForkJoinPool ⇒ FJP}
import parser._
import scalaz._, Scalaz._

object Main extends testing.Benchmark {
  def run {
    FJP.setParallelism(9)
    def str = getClass.getResourceAsStream("zinc.txt")
    def source = scala.io.Source fromInputStream str getLines
    val lines = source.toArray
    def countImpHs(s: String) = smiles(s) fold (_ ⇒ 0, _ foldMap (_ hydrogens))
    def res = lines.par map countImpHs sum

    println(res)
  }
}

// vim: set ts=2 sw=2 et:
