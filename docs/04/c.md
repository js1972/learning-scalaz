
### Foldable

LYAHFGG:

> Because there are so many data structures that work nicely with folds, the `Foldable` type class was introduced. Much like `Functor` is for things that can be mapped over, Foldable is for things that can be folded up!

The equivalent in Scalaz is also called `Foldable`. Let's see [the typeclass contract]($scalazBaseUrl$/core/src/main/scala/scalaz/Foldable.scala#L10-14):

```scala
trait Foldable[F[_]] { self =>
  /** Map each element of the structure to a [[scalaz.Monoid]], and combine the results. */
  def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: Monoid[B]): B

  /**Right-associative fold of a structure. */
  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B

  ...
}
```

Here are [the operators]($scalazBaseUrl$/core/src/main/scala/scalaz/syntax/FoldableSyntax.scala):

```scala
/** Wraps a value `self` and provides methods related to `Foldable` */
final class FoldableOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Foldable[F]) extends Ops[F[A]] {
  ////
  import collection.generic.CanBuildFrom
  import Leibniz.===
  import Liskov.<~<

  final def foldMap[B: Monoid](f: A => B = (a: A) => a): B = F.foldMap(self)(f)
  final def foldMap1Opt[B: Semigroup](f: A => B = (a: A) => a): Option[B] = F.foldMap1Opt(self)(f)
  final def foldRight[B](z: => B)(f: (A, => B) => B): B = F.foldRight(self, z)(f)
  final def foldMapRight1Opt[B](z: A => B)(f: (A, => B) => B): Option[B] = F.foldMapRight1Opt(self)(z)(f)
  final def foldRight1Opt(f: (A, => A) => A): Option[A] = F.foldRight1Opt(self)(f)
  final def foldLeft[B](z: B)(f: (B, A) => B): B = F.foldLeft(self, z)(f)
  final def foldMapLeft1Opt[B](z: A => B)(f: (B, A) => B): Option[B] = F.foldMapLeft1Opt(self)(z)(f)
  final def foldLeft1Opt(f: (A, A) => A): Option[A] = F.foldLeft1Opt(self)(f)
  final def foldRightM[G[_], B](z: => B)(f: (A, => B) => G[B])(implicit M: Monad[G]): G[B] = F.foldRightM(self, z)(f)
  final def foldLeftM[G[_], B](z: B)(f: (B, A) => G[B])(implicit M: Monad[G]): G[B] = F.foldLeftM(self, z)(f)
  final def foldMapM[G[_] : Monad, B : Monoid](f: A => G[B]): G[B] = F.foldMapM(self)(f)
  final def fold(implicit A: Monoid[A]): A = F.fold(self)(A)
  final def foldr[B](z: => B)(f: A => (=> B) => B): B = F.foldr(self, z)(f)
  final def foldr1Opt(f: A => (=> A) => A): Option[A] = F.foldr1Opt(self)(f)
  final def foldl[B](z: B)(f: B => A => B): B = F.foldl(self, z)(f)
  final def foldl1Opt(f: A => A => A): Option[A] = F.foldl1Opt(self)(f)
  final def foldrM[G[_], B](z: => B)(f: A => ( => B) => G[B])(implicit M: Monad[G]): G[B] = F.foldrM(self, z)(f)
  final def foldlM[G[_], B](z: B)(f: B => A => G[B])(implicit M: Monad[G]): G[B] = F.foldlM(self, z)(f)
  final def length: Int = F.length(self)
  final def index(n: Int): Option[A] = F.index(self, n)
  final def indexOr(default: => A, n: Int): A = F.indexOr(self, default, n)
  final def sumr(implicit A: Monoid[A]): A = F.foldRight(self, A.zero)(A.append)
  final def suml(implicit A: Monoid[A]): A = F.foldLeft(self, A.zero)(A.append(_, _))
  final def toList: List[A] = F.toList(self)
  final def toVector: Vector[A] = F.toVector(self)
  final def toSet: Set[A] = F.toSet(self)
  final def toStream: Stream[A] = F.toStream(self)
  final def toIList: IList[A] = F.toIList(self)
  final def toEphemeralStream: EphemeralStream[A] = F.toEphemeralStream(self)
  final def to[G[_]](implicit c: CanBuildFrom[Nothing, A, G[A]]) = F.to[A, G](self)
  final def all(p: A => Boolean): Boolean = F.all(self)(p)
  final def ∀(p: A => Boolean): Boolean = F.all(self)(p)
  final def allM[G[_]: Monad](p: A => G[Boolean]): G[Boolean] = F.allM(self)(p)
  final def anyM[G[_]: Monad](p: A => G[Boolean]): G[Boolean] = F.anyM(self)(p)
  final def any(p: A => Boolean): Boolean = F.any(self)(p)
  final def ∃(p: A => Boolean): Boolean = F.any(self)(p)
  final def count: Int = F.count(self)
  final def maximum(implicit A: Order[A]): Option[A] = F.maximum(self)
  final def maximumOf[B: Order](f: A => B): Option[B] = F.maximumOf(self)(f)
  final def maximumBy[B: Order](f: A => B): Option[A] = F.maximumBy(self)(f)
  final def minimum(implicit A: Order[A]): Option[A] = F.minimum(self)
  final def minimumOf[B: Order](f: A => B): Option[B] = F.minimumOf(self)(f)
  final def minimumBy[B: Order](f: A => B): Option[A] = F.minimumBy(self)(f)
  final def longDigits(implicit d: A <:< Digit): Long = F.longDigits(self)
  final def empty: Boolean = F.empty(self)
  final def element(a: A)(implicit A: Equal[A]): Boolean = F.element(self, a)
  final def splitWith(p: A => Boolean): List[NonEmptyList[A]] = F.splitWith(self)(p)
  final def selectSplit(p: A => Boolean): List[NonEmptyList[A]] = F.selectSplit(self)(p)
  final def collapse[X[_]](implicit A: ApplicativePlus[X]): X[A] = F.collapse(self)
  final def concatenate(implicit A: Monoid[A]): A = F.fold(self)
  final def intercalate(a: A)(implicit A: Monoid[A]): A = F.intercalate(self, a)
  final def traverse_[M[_]:Applicative](f: A => M[Unit]): M[Unit] = F.traverse_(self)(f)
  final def traverseU_[GB](f: A => GB)(implicit G: Unapply[Applicative, GB]): G.M[Unit] =
    F.traverseU_[A, GB](self)(f)(G)
  final def traverseS_[S, B](f: A => State[S, B]): State[S, Unit] = F.traverseS_(self)(f)
  final def sequence_[G[_], B](implicit ev: A === G[B], G: Applicative[G]): G[Unit] = F.sequence_(ev.subst[F](self))(G)
  final def sequenceS_[S, B](implicit ev: A === State[S,B]): State[S,Unit] = F.sequenceS_(ev.subst[F](self))
  def sequenceF_[M[_],B](implicit ev: F[A] <~< F[Free[M,B]]): Free[M, Unit] = F.sequenceF_(ev(self))
  final def msuml[G[_], B](implicit ev: A === G[B], G: PlusEmpty[G]): G[B] = F.foldLeft(ev.subst[F](self), G.empty[B])(G.plus[B](_, _))
  ////
}
```

That was impressive. Looks almost like the collection libraries, except it's taking advantage of typeclasses like `Order`. Let's try folding:

```scala
scala> List(1, 2, 3).foldRight (1) {_ * _}
res49: Int = 6

scala> 9.some.foldLeft(2) {_ + _}
res50: Int = 11
```

These are already in the standard library. Let's try the `foldMap` operator. `Monoid[A]` gives us `zero` and `|+|`, so that's enough information to fold things over. Since we can't assume that `Foldable` contains a monoid we need a function to change from `A => B` where `[B: Monoid]`:

```scala
scala> List(1, 2, 3) foldMap { identity }
res53: Int = 6

scala> List(true, false, true, true) foldMap { Tags.Disjunction.apply }
res56: scalaz.@@[Boolean,scalaz.Tags.Disjunction] = true


```

This surely beats writing `Tags.Disjunction(true)` for each of them and connecting them with `|+|`.

Lets try and use `fold` which combines all the elements using a Monoid. Scalac tries to use `fold` from the standard scala `Travesable` type, so we need to force the use of the `Foldable#fold` with `implicitly[Foldable[List]]` or alternatively we can wrap the fold in a function and let the type parameters force the use of `Foldable#fold`:

```
scala> implicitly[Foldable[List]].fold(List(1, 2, 3))
res2: Int = 6

scala> implicitly[Foldable[List]].fold(Tags.Multiplication.subst(List(1, 2, 3, 4)))
res4: scalaz.@@[Int,scalaz.Tags.Multiplication] = 24

scala> implicitly[Foldable[List]].fold(Tags.Disjunction.subst(List(true, false, true, true)))
res5: scalaz.@@[Boolean,scalaz.Tags.Disjunction] = true


scala> def sumGeneric[F[_], A](fa: F[A])(implicit F: Foldable[F], A: Monoid[A]): A = fa.fold
sumGeneric: [F[_], A](fa: F[A])(implicit F: scalaz.Foldable[F], implicit A: scalaz.Monoid[A])A

scala> sumGeneric(List(1, 2, 3))
res6: Int = 6

scala> sumGeneric(Tags.Multiplication.subst(List(1, 2, 3, 4)))
res7: scalaz.@@[Int,scalaz.Tags.Multiplication] = 24

scala> sumGeneric(Tags.Disjunction.subst(List(true, false, true, true)))
res8: scalaz.@@[Boolean,scalaz.Tags.Disjunction] = true
```

`Tags.Multiplication.subst()` and `Tags.Disjunction.subst()` substitutes the tag into the container type like so:
```
scala> Tags.Disjunction.subst(List(true, false, true, true))
res9: List[scalaz.@@[Boolean,scalaz.Tags.Disjunction]] = List(true, false, true, true)
```

We can also use `suml`:

```
scala> Tags.Disjunction.subst(List(true, false, true, true)).suml
res10: scalaz.@@[Boolean,scalaz.Tags.Disjunction] = true
```

We will pick it up from here later. I'll be out on a business trip, it might slow down.
