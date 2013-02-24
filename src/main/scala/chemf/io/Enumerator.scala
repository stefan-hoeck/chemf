package chemf.io

import collection.immutable.{IndexedSeq ⇒ IxSq}
import java.io._
import scalaz.{Reader ⇒ _, Writer ⇒ _, _}, Scalaz._, std.indexedSeq._
import scalaz.iteratee._, Iteratee._
import scalaz.effect._
import util.control.NonFatal

trait EnumeratorFunctions {
  import enumerator._

  def lines(path: String): DEnum[String] =
    linesF(disIO.rightIO(new File(path)))

  def linesF(f: DisIO[File]): DEnum[String] =
    linesR(f >>= fileReader)

  def linesR(br: DisIO[BufferedReader]): DEnum[String] =
    disIO.resourceEnum(br)(lineReader(_))

  def molLines(path: String): DEnum[IxSq[String]] = sdfLines(path)

  def sdfLines(path: String): DEnum[IxSq[String]] = 
    sdfLinesF(disIO.rightIO(new File(path)))

  def sdfLinesF(f: DisIO[File]): DEnum[IxSq[String]] =
    sdfLinesR(f >>= fileReader)

  def sdfLinesR(br: DisIO[BufferedReader]): DEnum[IxSq[String]] =
    disIO.resourceEnum(br)(sdfReader(_))

  def smilesLines(path: String): DEnum[String] = lines(path)

  private def fileReader(f: File): DisIO[BufferedReader] = try {
    (new BufferedReader(new FileReader(f))).η[DisIO]
  } catch { 
    case NonFatal(e) ⇒ 
      disIO leftIO s"Error when opening file: ${f.getPath}"
  }

  import StepT.{Cont, Done}

  def mapper[O,I,F[_]:Monad](f: O ⇒ I): EnumerateeT[O,I,F] =
    new EnumerateeT[O,I,F] {
      type Stp[A] = StepT[I,F,A]
      type It[A,B] = IterateeT[A,F,B]

      def apply[A]: Stp[A] ⇒ It[O,Stp[A]] = _ match {
        case s@Done(a, i) ⇒ sdone[O,F,Stp[A]](s, emptyInput).pointI
        case Cont(g)      ⇒ scont[O,F,Stp[A]](io ⇒ 
          g(io map (f(_))) >>== apply[A]
        ).pointI
      }
    }

  //def grouper[O,F[_]:Monad](cnt: Int): EnumerateeT[O,IxSq[O],F] =
  //  new EnumerateeT[O,IxSq[O],F] {
  //    type Stp[A] = StepT[IxSq[O],F,A]
  //    type It[A,B] = IterateeT[A,F,B]

  //    def apply[A]: Stp[A] ⇒ It[O,Stp[A]] = {
  //      def loop(c: Int, acc: IxSq[O]): Stp[A] ⇒ It[O,Stp[A]] = (c, _) match {
  //        case
  //      }

  //      loop(cnt, IxSq.empty) 
  //    }
  //  }

  private def lineReader(r: BufferedReader): DEnum[String] =
    new EnumeratorT[String,DisIO] {
      def apply[A] = (s: DStep[String,A]) ⇒ s mapCont { k ⇒
        try {
          r.readLine match {
            case null ⇒ s.pointI
            case line ⇒ k(elInput(line)) >>== apply[A]
          }
        } catch {
          case NonFatal(e) ⇒ iterateeT[String,DisIO,A](disIO leftIO e.toString)
        }
      }
    }

  private def sdfReader(r: BufferedReader): DEnum[IxSq[String]] =
    new EnumeratorT[IxSq[String],DisIO] {
      def apply[A] = (s: DStep[IxSq[String],A]) ⇒ s mapCont { k ⇒
        try {
          val buff = new collection.mutable.ListBuffer[String]
          var break = false
          
          while(! break) {
            r.readLine match {
              case null      ⇒ break = true
              case "$$$$"    ⇒ break = true
              case s         ⇒ buff += s
            }
          }

          val lines = buff.toIndexedSeq

          if (lines.isEmpty) s.pointI
          else k(elInput(lines)) >>== apply[A]

        } catch {
          case NonFatal(e) ⇒ iterateeT[IxSq[String],DisIO,A](disIO leftIO e.toString)
        }
      }
    }




}

trait EnumeratorInstances {
  implicit val ReaderResource: Resource[BufferedReader] =
    new Resource[BufferedReader] {
      def close(r: BufferedReader) = 
        IO.putStrLn(s"Closing $r") >>
        IO(r.close())
    }
}

object enumerator extends EnumeratorFunctions with EnumeratorInstances

// vim: set ts=2 sw=2 et:
