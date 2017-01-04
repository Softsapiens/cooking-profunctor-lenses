package algelab

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by dani on 16/12/2016.
  */

object cats {

  // https://github.com/purescript/purescript-prelude/blob/v2.1.0/src/Control/Semigroupoid.purs#L12-L13
  trait Semigroupoid[P[_, _]] {
    def >>>[A, B, C](f: P[A, B], g: P[B, C]): P[A, C]
  }

  // https://github.com/purescript/purescript-prelude/blob/v2.1.0/src/Control/Category.purs#L16-L17
  trait Category[P[_, _]] extends Semigroupoid[P] {
    def id[A]: P[A, A]
  }

  object instances {
    implicit val arrSemigroupoid = new Semigroupoid[Function1] {
      def >>>[A, B, C](f: A => B, g: B => C): A => C =
        f andThen g
    }

    implicit val arrCategory = new Category[Function1] {
      def id[A]: (A => A) = x => x

      def >>>[A, B, C](f: A => B, g: B => C): A => C =
        implicitly[Semigroupoid[Function1]].>>>(f, g)
    }
  }

}

object profunctor {

  trait Profunctor[P[_, _]] {
    def dimap[X, Y, A, B](f: X => A, g: B => Y)(p: P[A, B]): P[X, Y]

    def lmap[X, A, B](f: X => A)(p: P[A, B]): P[X, B] =
      dimap[X, B, A, B](f, identity)(p)

    def rmap[Y, A, B](g: B => Y)(p: P[A, B]): P[A, Y] =
      dimap[A, Y, A, B](identity, g)(p)
  }

  trait Strong[P[_, _]] extends Profunctor[P] {
    def first[X, A, B](p: P[A, B]): P[(A, X), (B, X)]

    def second[X, A, B](p: P[A, B]): P[(X, A), (X, B)]
  }

  trait Choice[P[_, _]] extends Profunctor[P] {
    def right[X, A, B](p: P[A, B]): P[Either[X, A], Either[X, B]]

    def left[X, A, B](p: P[A, B]): P[Either[A, X], Either[B, X]]
  }

  object instances {
    implicit val ArrProfunctor = new Profunctor[Function1] {
      override def dimap[X, Y, A, B](f: X => A, g: B => Y)(p: A => B): (X => Y) = g.compose(p.compose(f))
    }

    class ArrowStrong(implicit pro: Profunctor[Function1]) extends Strong[Function1] {

      override def dimap[X, Y, A, B](f: (X) => A, g: (B) => Y)(p: (A) => B) = pro.dimap(f, g)(p)

      def first[X, A, B](p: (A) => B) = {
        case (a, x) => (p(a), x)
      }

      def second[X, A, B](p: A => B) = {
        case (x, a) => (x, p(a))
      }
    }

    // Could be implicit
    val ArrStrong = new ArrowStrong()

    // Could be implicit
    val ArrChoice = new Choice[Function1] {

      override def dimap[X, Y, A, B](f: (X) => A, g: (B) => Y)(p: (A) => B) = implicitly[Profunctor[Function1]].dimap(f, g)(p)

      def right[X, A, B](p: A => B): Either[X, A] => Either[X, B] = _.map(p)

      def left[X, A, B](p: A => B): Either[A, X] => Either[B, X] = {
        case Left(a) => Left(p(a))
        case Right(c) => Right(c)
      }
    }
  }

}

object optics {
  // http://blog.functorial.com/posts/2015-11-20-Thermite.html
  // Intuitively, a lens represents a pair of a getter and a setter for a property of one type inside another, larger type.

  // Adapted from: https://gist.github.com/tel/ccfb747f93b748a9a6ec3cc957886ac3
  import scala.language.higherKinds
  import scala.language.implicitConversions
  import profunctor._

  type Lens[P[_, _], S, T, A, B] = Optic[Strong, P, S, T, A, B]
  type Forget[R, A, B] = A => R
  type Fold[R, S, T, A, B] = Optic[Id, Forget[R, ?, ?], S, T, A, B]
  type Getter[S, T, A, B] = Fold[A, S, T, A, B]
  type Setter[P[_, _], S, T, A, B] = Optic[Func, P, S, T, A, B]
  type Prism[P[_, _], S, T, A, B] = Optic[Choice, P, S, T, A, B]

  /*
  From packages:
    https://github.com/purescript/purescript-profunctor/blob/master/src/Data/Profunctor.purs
    https://github.com/purescript-contrib/purescript-profunctor-lenses/blob/master/src/Data/Lens/Lens.purs
  */
  def lens[P[_, _], S, T, A, B](getter: S => A, setter: (S, B) => T): Lens[P, S, T, A, B] =
    new Optic[Strong, P, S, T, A, B] {
      def apply(ex: Strong[P])(pab: P[A, B]): P[S, T] =
        ex.dimap[S, T, (A, B => T), (B, B => T)](s => (getter(s), b => setter(s, b)), { case (b, f) => f(b) })(ex.first[B => T, A, B](pab))
    }

  /*
    prism :: forall s t a b. (b -> t) -> (s -> Either t a) -> Prism s t a b
    prism to fro pab = dimap fro (either id id) (right (rmap to pab))

    // https://pursuit.purescript.org/packages/purescript-either/2.0.0/docs/Data.Either#v:either
    either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c

    // https://pursuit.purescript.org/packages/purescript-profunctor/2.0.0/docs/Data.Profunctor.Choice
    right :: forall a b c. p b c -> p (Either a b) (Either a c)
  */
  def prism[P[_, _], S, T, A, B](to: B => T, fro: S => Either[T, A]): Prism[P, S, T, A, B] =
    new Optic[Choice, P, S, T, A, B] {
      def apply(ex: Choice[P])(pab: P[A, B]): P[S, T] =
        ex.dimap[S, T, Either[T, A], Either[T, T]](fro, _.fold(identity, identity))(ex.right[T, A, T](ex.rmap[T, A, B](to)(pab)): P[Either[T, A], Either[T, T]])
    }

  trait Optic[O[_[_, _]], P[_, _], S, T, A, B] {
    // P[A, B] => P[S, T]
    def apply(ex: O[P])(p: P[A, B]): P[S, T]
  }

  trait Id[P[_, _]] {}

  trait Func[P[_, _]] {}

  object getter {
    def view[S, T, A, B](gttr: Getter[S, T, A, B], s: S): A = {
      gttr.apply(forget.instances.forgetId[A])(forget.id[A, B])(s)
    }

    // Convert a function into a getter.
    def to[R, S, T, A, B](f: S => A): Fold[R, S, T, A, B] =
      new Optic[Id, Forget[R, ?, ?], S, T, A, B] {
        // P[A, B] => P[S, T]
        override def apply(ex: Id[Forget[R, ?, ?]])(p: Forget[R, A, B]): Forget[R, S, T] = f andThen p
      }
  }

  object forget {
    def id[A, B]: Forget[A, A, B] = identity

    object instances {
      def forgetId[R] = new Id[Forget[R, ?, ?]] {}

      // https://github.com/purescript-contrib/purescript-profunctor-lenses/blob/master/src/Data/Lens/Internal/Forget.purs

      def forgetStrong[R] = new Strong[Forget[R, ?, ?]] {
        def dimap[X, Y, A, B](f: X => A, g: B => Y)(p: Forget[R, A, B]) = forgetProfunctor.dimap(f, g)(p)

        def first[X, A, B](p: Forget[R, A, B]): Forget[R, (A, X), (B, X)] = {
          case (a, x) => p(a)
        }

        def second[X, A, B](p: Forget[R, A, B]): Forget[R, (X, A), (X, B)] = {
          case (x, a) => p(a)
        }
      }

      def forgetProfunctor[R] = new Profunctor[Forget[R, ?, ?]] {
        override def dimap[X, Y, A, B](f: (X) => A, g: (B) => Y)(p: Forget[R, A, B]): Forget[R, X, Y] =
          f andThen p
      }

      def forgetChoice[R: Monoid] = new Choice[Forget[R, ?, ?]] {
        def dimap[X, Y, A, B](f: X => A, g: B => Y)(p: Forget[R, A, B]) = forgetProfunctor.dimap(f, g)(p)

        def right[X, A, B](p: Forget[R, A, B]): Forget[R, Either[X, A], Either[X, B]] = {
          case Right(a) => p(a)
          case Left(c) => implicitly[Monoid[R]].mempty
        }

        def left[X, A, B](p: Forget[R, A, B]): Forget[R, Either[A, X], Either[B, X]] = {
          case Left(a) => p(a)
          case Right(c) => implicitly[Monoid[R]].mempty
        }
      }

      trait Monoid[A] {
        def mempty: A

        def mappend(a1: A, a2: A): A
      }
    }

  }

  object prism {

    object either {
      // https://github.com/purescript-contrib/purescript-profunctor-lenses/blob/master/src/Data/Lens/Prism/Either.purs

      // left :: forall a b c. p a b -> p (Either a c) (Either b c)
      def _Left[P[_, _], A, B, C]: Prism[P, Either[A, C], Either[B, C], A, B] = new Optic[Choice, P, Either[A, C], Either[B, C], A, B] {
        def apply(ex: Choice[P])(pab: P[A, B]): P[Either[A, C], Either[B, C]] = ex.left[C, A, B](pab)
      }

      // _Right :: forall a b c. Prism (Either c a) (Either c b) a b
      def _Right[P[_, _], A, B, C]: Prism[P, Either[C, A], Either[C, B], A, B] = new Optic[Choice, P, Either[C, A], Either[C, B], A, B] {
        def apply(ex: Choice[P])(pab: P[A, B]): P[Either[C, A], Either[C, B]] = ex.right[C, A, B](pab)
      }
    }

  }

}

class FunProfun extends FlatSpec with Matchers {

  "newoptics1 -->  Testing Lens composition and/with Getter composition" should "work" in {
    import optics._
    import profunctor.instances._
    import getter._

    case class Age(years: Int)
    case class Person(name: String, age: Age)

    val p1 = Person("Dani", Age(40))
    val p2 = Person("Ana", Age(34))

    val _arrStrong = ArrStrong

    def _lpAge[P[_, _]] = lens[P, Person, Person, Age, Age](_.age, { case (p, v) => p.copy(age = v) })


    val pAge: (Age => Age) => (Person => Person) = _lpAge(_arrStrong)(_)
    val aYears: (Int => Int) => (Age => Age) = lens[Function1, Age, Age, Int, Int](_.years, { case (a, v) => a.copy(years = v) })(_arrStrong)(_)

    val pYears = aYears andThen pAge

    val _gtrYear: Getter[Age, Age, Int, Int] = getter.to(_.years)
    val _gtrAge: Getter[Person, Person, Age, Age] = getter.to(_.age)
    val _pgtrYear: Getter[Person, Person, Int, Int] = getter.to({
      ((p: Person) => p.age) andThen (_.years)
    })

    assert(view(_gtrYear, Age(40)) === 40)

    assert(view(_pgtrYear, pYears(identity)(p1)) === 40)
    assert(view(_pgtrYear, pYears(_ + 1)(p1)) === 41)

    val _gtrYear2 = getter.to[Int, Age, Age, Int, Int](_.years)(forget.instances.forgetId)(identity)
    val _gtrAge2 = getter.to[Age, Person, Person, Age, Age](_.age)(forget.instances.forgetId)(identity)
    val _pgtrYear2 = _gtrAge2 andThen _gtrYear2

    assert(_pgtrYear2(pYears(identity)(p1)) === 40)
    assert(_pgtrYear2(pYears(_ + 1)(p1)) === 41)
  }


  "Testing Prism composition" should "work" in {
    import optics._
    import profunctor.instances._
    import cats._
    import cats.instances._

    val _arrChoice = ArrChoice

    val _arrSemigroupoid = implicitly[Semigroupoid[Function1]]

    val _p1 = prism[Function1, Either[String, Int], Option[String], String, String](Some(_), {
      case Left(a) => Right(a + "1")
      case _ => Left(None)
    })(_arrChoice)(identity)

    val _p2 = prism[Function1, Option[String], String, String, String](identity, {
      case Some(a) => Right(a + "2")
      case _ => Left("")
    })(_arrChoice)(identity)

    assert(_p1(Left("Hola")) === Some("Hola1"))

    val _p12 = _arrSemigroupoid.>>>(_p1, _p2)
    val __p12 = _p1 andThen _p2 // Same as previous

    assert(_p12(Left("Hola")) === "Hola12")
    assert(__p12(Left("Hola")) === "Hola12")
  }

  "Testing Prisms" should "work" in {
    import optics._
    import profunctor.instances._

    val _arrChoice = ArrChoice //implicitly[Choice[Function1]]

    val _pEitherStrInt = prism[Function1, Either[String, Int], Option[String], String, String](Some(_), {
      case Left(a) => Right(a)
      case _ => Left(None)
    })(_arrChoice)(identity)

    assert(_pEitherStrInt(Left("Hola")) === Some("Hola"))
    assert(_pEitherStrInt(Right(69)) === None)
  }

  "Testing implicit Function1 Category instance" should "work" in {
    import cats._
    import cats.instances._

    val f: (Int => String) = _.toString()
    val g: (String => String) = _ + "+1"

    val _arrCat = implicitly[Category[Function1]]

    val z = _arrCat.>>>(f, g)

    assert(z(1) === "1+1")
  }

  "Testing implicit Function1 Profunctor instance" should "work" in {
    import profunctor.instances._

    val f: (String => Int) = _.toInt
    val p: (Int => Int) = _ + 1
    val g: (Int => String) = _.toString

    val _arrPro = ArrProfunctor

    val z = _arrPro.dimap(f, g)(p)

    assert(z("1") === "2")
  }
}
