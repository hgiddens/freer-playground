package freer

import cats.data._
import org.atnos.eff._
import org.atnos.eff.all._
import org.atnos.eff.syntax.all._

object Freer extends App {
  abstract class Interposer[T[_], R, A, W] {
    def member: Member[T, R]
    def pure: A => Eff[R, W]
    def impure[X](t: T[X], rest: X => Eff[R, W]): Eff[R, W]

    private[this] final def loop: Eff[R, A] => Eff[R, W] = {
      case Pure(a) =>
        pure(a)

      case Impure(union, continuation) =>
        val composedContinuation = loop.compose(continuation.apply)
        member.project(union) match {
          case Xor.Right(t) => impure(t, composedContinuation)
          case _ => Impure(union, Arrs.singleton(composedContinuation))
        }
    }
  }

  def qComp[R, R1, A, B, C](g: Arrs[R, A, B], h: (Eff[R, B] => Eff[R1, C])): A => Eff[R1, C] =
    h.compose(g.apply)

  abstract class H[F[_], G[_], R] {
    def apply[A](fa: F[A], ga: G[A]): R
  }

  abstract class InterposeHelper[T[_], R, A, W] {
    def member: Member[T, R]
    def ret: A => Eff[R, W]

    def apply[V](h: H[T, ? => Eff[R, W], Eff[R, W]]): Eff[R, A] => Eff[R, W] = {
      def loop: Eff[R, A] => Eff[R, W] = {
        case Pure(x) => ret(x)
        case Impure(u, q) =>
          def k = qComp(q, loop)
          member.project(u).toOption match {
            case Some(x) => h(x, k)
            case None => Impure(u, Arrs.singleton(k))
          }
      }
      loop
    }
  }

  def interpose[T[_], R, A, W](ret0: A => Eff[R, W])(implicit member0: Member[T, R]): InterposeHelper[T, R, A, W] =
    new InterposeHelper[T, R, A, W] {
      def member = member0
      def ret = ret0
    }

  def actuallyLocal[R, T, A](f: T => T)(m: Eff[R, A])(implicit member: Member[Reader[T, ?], R]): Eff[R, A] = {
    ReaderCreation.ask[R, T].flatMap { e0 =>
      val e: T = f(e0)
      val h = new H[Reader[T, ?], ? => Eff[R, A], Eff[R, A]] {
        def apply[X](fa: Reader[T, X], ga: X => Eff[R, A]): Eff[R, A] =
          ga(fa.run(e))
      }
      interpose[Reader[T, ?], R, A, A](Eff.pure[R, A]).apply(h)(m)
    }
  }


  def freer() = {
    type Stack = Writer[Int, ?] |: Reader[Int, ?] |: NoEffect

    val blab: Eff[Stack, Unit] = for {
      current <- ask[Stack, Int]
      _ <- tell[Stack, Int](current)
    } yield ()

    import cats.syntax.all._

    def l[A](fa: Eff[Stack, A]): Eff[Stack, A] = {
      actuallyLocal[Stack, Int, A](_ * 2)(fa)
    }

    val action = blab *> l(blab) *> blab
    val (_, written) = action.runWriter.runReader(6).run
    println(s"wrote: $written")
  }

  def freer2() = {
    type Stack = Reader[Int, ?] |: Writer[Int, ?] |: NoEffect

    val blab: Eff[Stack, Unit] = for {
      current <- ask[Stack, Int]
      _ <- tell[Stack, Int](current)
    } yield ()

    import cats.syntax.all._

    def l[A](fa: Eff[Stack, A]): Eff[Stack, A] = {
      actuallyLocal[Stack, Int, A](_ * 2)(fa)
    }

    val action = blab *> l(blab) *> blab
    val (_, written) = action.runReader(6).runWriter.run
    println(s"wrote: $written")
  }

  def mt() = {
    import cats.Id
    import cats.std.all._
    import cats.syntax.all._

    type F[A] = ReaderT[Writer[List[Int], ?], Int, A]

    val blab: F[Unit] = for {
      current <- ReaderT.ask[Writer[List[Int], ?], Int]
      _ <- WriterT.tell[Id, List[Int]](List(current)).liftT[ReaderT[?[_], Int, ?]]
    } yield ()

    def l[A](fa: F[A]): F[A] =
      ReaderT.local[Writer[List[Int], ?], A, Int](_ * 2)(fa)

    val action = blab *> l(blab) *> blab
    val alpha: Writer[List[Int], Unit] = action.run(6)
    val beta: (List[Int], Unit) = alpha.run
    val (written, _) = beta
    println(s"wrote $written")
  }

  def mt2() = {
    import cats.Id
    import cats.std.all._
    import cats.syntax.all._

    type F[A] = WriterT[Reader[Int, ?], List[Int], A]

    val blab: F[Unit] = for {
      current <- ReaderT.ask[Id, Int].liftT[WriterT[?[_], List[Int], ?]]
      _ <- WriterT.tell[Reader[Int, ?], List[Int]](List(current))
    } yield ()

    def l[A](fa: F[A]): F[A] = {
      val x = ReaderT.local[Id, (List[Int], A), Int](_ * 2)(fa.run)
      WriterT(x)
    }

    val action = blab *> l(blab) *> blab
    val alpha: Reader[Int, (List[Int], Unit)] = action.run
    val beta: (List[Int], Unit) = alpha.run(6)
    val (written, _) = beta
    println(s"wrote $written")
  }

  freer()
  println("---")
  freer2()
  println("---")
  mt()
  println("---")
  mt2()
}
