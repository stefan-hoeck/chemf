package chemf.io

import chemf.DisRes
import collection.immutable.{IndexedSeq ⇒ IxSq}
import java.io._
import scalaz.{Reader ⇒ _, Writer ⇒ _, _}, Scalaz._, std.indexedSeq._
import scalaz.iteratee._, Iteratee._
import scalaz.effect._
import util.control.NonFatal

trait IterFunctions {

  import iter._

  def lineOut(path: String): DIter[String,Unit] = 
    lineOutF(disIO createFile path)

  def linesOut(path: String): DIter[IxSq[String],Unit] = 
    linesOutF(disIO createFile path)

  def lineOutF(f: DisIO[File]): DIter[String,Unit] = 
    disIO.resourceIter(f >>= fileWriter)((s, o) ⇒ IO(o println s))

  def linesOutF(f: DisIO[File]): DIter[IxSq[String],Unit] = 
    disIO.resourceIter(f >>= fileWriter)(
      (ls, o) ⇒ ls.foldMap( l ⇒ IO(o println l))
    )

  def throbber[E](inc: Int): IterateeT[E,IO,Unit] = {
    type ToIter = Input[E] ⇒ IterateeT[E,IO,Unit]

    def now = System.currentTimeMillis
    def cont(t: ToIter) = IO(scont(t))
    def icont(t: ToIter) = iterateeT(cont(t))
    def report(acc: Int, millis: Long) =
      IO putStrLn s"Accumulated $acc items in $millis ms"

    def step(acc: Int, cnt: Int, start: Long): ToIter = i ⇒ 
      (acc, cnt, i) match {
        case (a, 0, Input.Element(_)) ⇒ 
          iterateeT(report(a + inc, now - start) >>
          cont(step(a + inc, inc - 1, start)))
        case (a, x, Input.Element(_)) ⇒ icont(step(a, x - 1, start))
        case (a, x, Input.Empty())    ⇒ icont(step(a, x, start))
        case (a, x, Input.Eof())      ⇒ iterateeT(IO(sdone((), eofInput)))
      }

    icont(step(0, inc - 1, now))
  }

  private def fileWriter(f: File): DisIO[PrintWriter] = try {
    (new PrintWriter(new FileWriter(f))).η[DisIO]
  } catch { 
    case NonFatal(e) ⇒ 
      disIO leftIO s"Error when opening file: ${f.getPath}"
  }
}

trait IterInstances {
  implicit def IterMonoid[E,F[_]:Monad,A:Monoid]: Monoid[IterateeT[E,F,A]] =
    new Monoid[IterateeT[E,F,A]] {
      type Iter[X] = IterateeT[E,F,X]
      type Stp[X] = StepT[E,F,X]

      val zero = ∅[A].η[Iter]

      def append(a: Iter[A], b: ⇒ Iter[A]): Iter[A] = iterateeT[E,F,A](
        for {
          sta ← a.value
          stb ← b.value
        } yield stepConcat(sta, stb)
      )

      import StepT.{Cont, Done}
      def stepConcat(a: Stp[A], b: Stp[A]): Stp[A] = (a, b) match {
        case (Cont(fa), Cont(fb))     ⇒ scont(i ⇒ append(fa(i), fb(i)))
        case (Cont(fa), b@Done(_, _)) ⇒ scont(i ⇒ append(fa(i), b.pointI))
        case (a@Done(_, _), Cont(fb)) ⇒ scont(i ⇒ append(a.pointI, fb(i)))
        case (Done(a, i), Done(b, j)) ⇒ sdone(a ⊹ b, i)
      }
    }

  implicit val WriterResource: Resource[PrintWriter] = 
    new Resource[PrintWriter] {
      def close(r: PrintWriter) = IO{r.flush(); r.close()}
    }
}

object iter extends IterInstances with IterFunctions

// vim: set ts=2 sw=2 et:
