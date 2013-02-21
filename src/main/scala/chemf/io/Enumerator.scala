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
    linesF(disIO rightIO(new File(path)))

  def linesF(f: DisIO[File]): DEnum[String] =
    linesR(f >>= fileReader)

  def linesR(br: DisIO[BufferedReader]): DEnum[String] =
    disIO.resourceEnum(br)(lineReader(_))

  def molLines(path: String): DEnum[IxSq[String]] = sdfLines(path)

  def sdfLines(path: String): DEnum[IxSq[String]] = 
    lines(path) mapE 
    EnumerateeT.splitOn[String,IxSq,DisIO](s ⇒ !s.startsWith("$$$$"))

  def smilesLines(path: String): DEnum[String] = lines(path)

  private def fileReader(f: File): DisIO[BufferedReader] = try {
    (new BufferedReader(new FileReader(f))).η[DisIO]
  } catch { 
    case NonFatal(e) ⇒ 
      disIO leftIO s"Error when opening file: ${f.getPath}"
  }

  private def lineReader(r: ⇒ BufferedReader): DEnum[String] =
    new EnumeratorT[String,DisIO] {
      lazy val reader = r

      def apply[A] = (s: DStep[String,A]) ⇒ s mapCont { k ⇒
        try {
          Option(reader.readLine) cata (
            line ⇒ k(elInput(line)) >>== apply[A],
            s.pointI
          )
        } catch {
          case NonFatal(e) ⇒ iterateeT[String,DisIO,A](disIO leftIO e.toString)
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
