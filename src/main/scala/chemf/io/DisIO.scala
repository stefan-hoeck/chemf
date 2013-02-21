package chemf.io

import chemf.DisRes
import java.io.File
import scalaz._, Scalaz._, effect._, iteratee._, Iteratee._
import util.control.NonFatal

trait DisIOFunctions {

  val disTo = new (IO ~> DisIO) {
    def apply[A](i: IO[A]) = EitherT(i map (_.right))
  }

  def rightCont[E,A](f: Input[E] ⇒ DIter[E,A]): DisIO[DStep[E,A]] =
    rightIO(scont(f))

  def rightIO[A](a: ⇒ A): DisIO[A] = EitherT(IO(a.right))

  def leftIO[A](msg: ⇒ String): DisIO[A] = EitherT(IO(msg.left))

  def liftDis[A](io: IO[A]): DisIO[A] = EitherT(io map (_.right))

  def consoleRun(d: DisIO[Unit]): IO[Unit] =
    d fold (IO.putStrLn(_), _ ⇒ IO.ioUnit) μ

  def resourceIter[E,R:Resource]
    (create: DisIO[R])
    (out: (E,R) ⇒ IO[Unit]): DIter[E,Unit] = {
      type MStep = DisIO[DStep[E,Unit]]
      def iter(s: MStep) = iterateeT[E,DisIO,Unit](s)

      def go(r: R): MStep = rightCont { i ⇒ 
        iter(
          i.fold(
            go(r),
            e ⇒ liftDis(out(e, r)) >> go(r),
            liftDis(Resource[R] close r) >> rightIO(sdone((), eofInput))
          )
        )
      }
      
      iter(create >>= go)
    }

  def resourceEnum[E,R:Resource]
    (r: DisIO[R])
    (enum: R ⇒ DEnum[E]): DEnum[E] = new EnumeratorT[E,DisIO] {
      private def closeD(dr: DisRes[R]): IO[Unit] =
        dr fold (_ ⇒ IO.ioUnit, Resource[R].close)

      def apply[A] = (s: StepT[E,DisIO,A]) ⇒ {
        def enumR(r: R): IO[DisRes[StepT[E,DisIO,A]]] =
          enum(r).apply(s).value.run

        def run: DisIO[StepT[E,DisIO,A]] =
          EitherT(r.run.bracket(closeD)(_ fold (s ⇒ IO(s.left), enumR)))

        iterateeT(run)
      }
    }

  def createFile(p: String): DisIO[File] = EitherT(
    IO {
      try {
        val f = new File(p)
        if (! f.exists) {f.createNewFile()}
        f.right
      } catch { case NonFatal(e) ⇒ s"Error when opening file $p: $e".left }
    }
  )
}

object disIO extends DisIOFunctions

// vim: set ts=2 sw=2 et:
