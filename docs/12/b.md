
### The Essence of the Iterator Pattern

In 2006 the same author wrote [The Essence of the Iterator Pattern](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf). Linked is the revised 2009 version. This paper discusses applicative style by breaking down the GoF Iterator pattern into two aspects: *mapping* and *accumulating*.

The first half of the paper reviews functional iterations and applicative style. For applicative functors, it brings up the fact that there are three kinds of applicatives:
1. Monadic applicative functors
2. Naperian applicative functors
3. Monoidal applicative functors

We've brought up the fact that all monads are applicatives many times. Naperian applicative functor zips together data structure that are fixed in shape. Also apparently appliactive functors were originally named *idiom*, so *idiomatic* in this paper means *applicative*.

### Monoidal applicatives

Scalaz implements `Monoid[m].applicative` to turn any monoids into an applicative.

```scala
scala> Monoid[Int].applicative.ap2(1, 1)(0)
res99: Int = 2

scala> Monoid[List[Int]].applicative.ap2(List(1), List(1))(Nil)
res100: List[Int] = List(1, 1)
```

### Combining applicative functors

EIP:

> Like monads, applicative functors are closed under products; so two independent idiomatic
effects can generally be fused into one, their product.

In Scalaz, `product` is implemented under `Applicative` typeclass:

```scala
trait Applicative[F[_]] extends Apply[F] with Pointed[F] { self =>
  ...
  /**The product of Applicatives `F` and `G`, `[x](F[x], G[x]])`, is an Applicative */
  def product[G[_]](implicit G0: Applicative[G]): Applicative[({type λ[α] = (F[α], G[α])})#λ] = new ProductApplicative[F, G] {
    implicit def F = self
    implicit def G = G0
  }
  ...
}
```

Let's make a product of `List` and `Option`.

```scala
scala> Applicative[List].product[Option]
res0: scalaz.Applicative[[α](List[α], Option[α])] = scalaz.Applicative\$\$anon\$2@211b3c6a

scala> Applicative[List].product[Option].point(1)
res1: (List[Int], Option[Int]) = (List(1),Some(1))
```

The product seems to be implemented as a `Tuple2`. Let's use Applicative style to append them:

```scala
scala> ((List(1), 1.some) |@| (List(1), 1.some)) {_ |+| _}
res2: (List[Int], Option[Int]) = (List(1, 1),Some(2))

scala> ((List(1), 1.success[String]) |@| (List(1), "boom".failure[Int])) {_ |+| _}
res6: (List[Int], scalaz.Validation[String,Int]) = (List(1, 1),Failure(boom))
```

EIP:

> Unlike monads in general, applicative functors are also closed under composition; so
two sequentially-dependent idiomatic effects can generally be fused into one, their composition.

This is called `compose` under `Applicative`:

```scala
trait Applicative[F[_]] extends Apply[F] with Pointed[F] { self =>
...
  /**The composition of Applicatives `F` and `G`, `[x]F[G[x]]`, is an Applicative */
  def compose[G[_]](implicit G0: Applicative[G]): Applicative[({type λ[α] = F[G[α]]})#λ] = new CompositionApplicative[F, G] {
    implicit def F = self
    implicit def G = G0
  }
...
}
```

Let's compose `List` and `Option`.

```scala
scala> Applicative[List].compose[Option]
res7: scalaz.Applicative[[α]List[Option[α]]] = scalaz.Applicative\$\$anon\$1@461800f1

scala> Applicative[List].compose[Option].point(1)
res8: List[Option[Int]] = List(Some(1))
```

EIP:

> The two operators `⊗` and `⊙` allow us to combine idiomatic computations in two different ways; we call them *parallel* and *sequential composition*, respectively.

The fact that we can compose applicatives and it remain applicative is neat. I am guessing that this characteristics enables modularity later in this paper.

### Idiomatic traversal

EIP:

> *Traversal* involves iterating over the elements of a data structure, in the style of a `map`, but interpreting certain function applications idiomatically.

The corresponding typeclass in Scalaz 7 is called [`Traverse`]($scalazBaseUrl$/core/src/main/scala/scalaz/Traverse.scala):

```scala
trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverseImpl[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]]
}
```

This introduces `traverse` operator:

```scala
trait TraverseOps[F[_],A] extends Ops[F[A]] {
  final def traverse[G[_], B](f: A => G[B])(implicit G: Applicative[G]): G[F[B]] =
    G.traverse(self)(f)
  ...
}
```

Here's how we can use it for `List`:

```scala
scala> List(1, 2, 3) traverse { x => (x > 0) option (x + 1) }
res14: Option[List[Int]] = Some(List(2, 3, 4))

scala> List(1, 2, 0) traverse { x => (x > 0) option (x + 1) }
res15: Option[List[Int]] = None
```

The `option` operator is injected to `Boolean`, which expands `(x > 0) option (x + 1)` to `if (x > 0) Some(x + 1) else None`.

EIP:

> In the case of a monadic applicative functor, traversal specialises to monadic map, and has the same uses.

It does have similar feel to `flatMap`, except now the passed in function returns `G[B]` where `[G: Applicative]` instead of requiring `List`.

EIP:

> For a monoidal applicative functor, traversal accumulates values. The function *reduce* performs that accumulation, given an argument that assigns a value to each element.

```scala
scala> Monoid[Int].applicative.traverse(List(1, 2, 3)) {_ + 1}
res73: Int = 9
```

I wasn't able to write this as `traverse` operator.

### Shape and contents

EIP:

> In addition to being parametrically polymorphic in the collection elements, the generic *traverse* operation is parametrised along two further dimensions: the datatype being traversed, and the applicative functor in which the traversal is interpreted. Specialising the latter to lists as a monoid yields a generic *contents* operation.

```scala
scala> def contents[F[_]: Traverse, A](f: F[A]): List[A] =
         Monoid[List[A]].applicative.traverse(f) {List(_)}
contents: [F[_], A](f: F[A])(implicit evidence\$1: scalaz.Traverse[F])List[A]

scala> contents(List(1, 2, 3))
res87: List[Int] = List(1, 2, 3)

scala> contents(NonEmptyList(1, 2, 3))
res88: List[Int] = List(1, 2, 3)

scala> val tree: Tree[Char] = 'P'.node('O'.leaf, 'L'.leaf)
tree: scalaz.Tree[Char] = <tree>

scala> contents(tree)
res90: List[Char] = List(P, O, L)
```

Now we can take any data structure that supports `Traverse` and turn it into a `List`. We can also write `contents` as follows:

```scala
scala> def contents[F[_]: Traverse, A](f: F[A]): List[A] =
         f.traverse[({type l[X]=List[A]})#l, A] {List(_)}
contents: [F[_], A](f: F[A])(implicit evidence\$1: scalaz.Traverse[F])List[A]
```

> The other half of the decomposition is obtained simply by a map, which is to say, a traversal interpreted in the identity idiom.

The "identity idiom" is the `Id` monad in Scalaz.

```scala
scala> def shape[F[_]: Traverse, A](f: F[A]): F[Unit] =
  f traverse {_ => ((): Id[Unit])}
shape: [F[_], A](f: F[A])(implicit evidence\$1: scalaz.Traverse[F])F[Unit]

scala> shape(List(1, 2, 3))
res95: List[Unit] = List((), (), ())

scala> shape(tree).drawTree
res98: String =
"()
|
()+-
|
()`-
"
```

EIP:

> This pair of traversals nicely illustrates the two aspects of iterations that we are focussing on, namely mapping and accumulation.

Let's also implement `decompose` function:

```scala
scala> def decompose[F[_]: Traverse, A](f: F[A]) = (shape(f), contents(f))
decompose: [F[_], A](f: F[A])(implicit evidence\$1: scalaz.Traverse[F])(F[Unit], List[A])

scala> decompose(tree)
res110: (scalaz.Tree[Unit], List[Char]) = (<tree>,List(P, O, L))
```

This works, but it's looping the tree structure twice. Remember a product of two applicatives are also an applicative?

```scala
scala> def decompose[F[_]: Traverse, A](f: F[A]) =
         Applicative[Id].product[({type l[X]=List[A]})#l].traverse(f) { x => (((): Id[Unit]), List(x)) }
decompose: [F[_], A](f: F[A])(implicit evidence\$1: scalaz.Traverse[F])(scalaz.Scalaz.Id[F[Unit]], List[A])

scala> decompose(List(1, 2, 3, 4))
res135: (scalaz.Scalaz.Id[List[Unit]], List[Int]) = (List((), (), (), ()),List(1, 2, 3, 4))

scala> decompose(tree)
res136: (scalaz.Scalaz.Id[scalaz.Tree[Unit]], List[Char]) = (<tree>,List(P, O, L))
```

Since the above implementation relys on type annotation to get the monoidal applicative functor, I can't write it as nice as the Haskell example:

```haskell
decompose = traverse (shapeBody ⊗ contentsBody)
```

### Sequence

There's a useful method that `Traverse` introduces called `sequence`. The names comes from Haskell's `sequence` function, so let's Hoogle it:

> ```haskell sequence :: Monad m => [m a] -> m [a]```
> Evaluate each action in the sequence from left to right, and collect the results.

Here's `sequence` method:

```scala
  /** Traverse with the identity function */
  final def sequence[G[_], B](implicit ev: A === G[B], G: Applicative[G]): G[F[B]] = {
    val fgb: F[G[B]] = ev.subst[F](self)
    F.sequence(fgb)
  }
```

Instead of `Monad`, the requirement is relaxed to `Applicative`. Here's how we can use it:

```scala
scala> List(1.some, 2.some).sequence
res156: Option[List[Int]] = Some(List(1, 2))

scala> List(1.some, 2.some, none).sequence
res157: Option[List[Int]] = None
```

This looks cool. And because it's a `Traverse` method, it'll work for other data structures as well:

```scala
scala> val validationTree: Tree[Validation[String, Int]] = 1.success[String].node(
         2.success[String].leaf, 3.success[String].leaf)
validationTree: scalaz.Tree[scalaz.Validation[String,Int]] = <tree>

scala> validationTree.sequence[({type l[X]=Validation[String, X]})#l, Int]
res162: scalaz.Validation[String,scalaz.Unapply[scalaz.Traverse,scalaz.Tree[scalaz.Validation[String,Int]]]{type M[X] = scalaz.Tree[X]; type A = scalaz.Validation[String,Int]}#M[Int]] = Success(<tree>)

scala> val failedTree: Tree[Validation[String, Int]] = 1.success[String].node(
         2.success[String].leaf, "boom".failure[Int].leaf)
failedTree: scalaz.Tree[scalaz.Validation[String,Int]] = <tree>

scala> failedTree.sequence[({type l[X]=Validation[String, X]})#l, Int]
res163: scalaz.Validation[String,scalaz.Unapply[scalaz.Traverse,scalaz.Tree[scalaz.Validation[String,Int]]]{type M[X] = scalaz.Tree[X]; type A = scalaz.Validation[String,Int]}#M[Int]] = Failure(boom)
```

### Collection and dispersal

EIP:

> We have found it convenient to consider special cases of effectful traversals, in which the mapping aspect is independent of the accumulation, and vice versa. The ﬁrst of these traversals accumulates elements effectfully, with an operation of type `a → m ()`, but modiﬁes those elements purely and independently of this accumulation, with a function of type `a → b`.

This is mimicking the use of `for` loop with mutable variable accumulating the value outside of the loop. `Traverse` adds `traverseS`, which is a specialized version of `traverse` for `State` monad. Using that we can write `collect` as following:

```scala
scala> def collect[F[_]: Traverse, A, S, B](t: F[A])(f: A => B)(g: S => S) =
         t.traverseS[S, B] { a => State { (s: S) => (g(s), f(a)) } }
collect: [F[_], A, S, B](t: F[A])(f: A => B)(g: S => S)(implicit evidence\$1: scalaz.Traverse[F])scalaz.State[S,scalaz.Unapply[scalaz.Traverse,F[A]]{type M[X] = F[X]; type A = A}#M[B]]

scala> val loop = collect(List(1, 2, 3, 4)) {(_: Int) * 2} {(_: Int) + 1}
loop: scalaz.State[Int,scalaz.Unapply[scalaz.Traverse,List[Int]]{type M[X] = List[X]; type A = Int}#M[Int]] = scalaz.package\$State\$\$anon\$1@3926008a

scala> loop(0)
res165: (Int, scalaz.Unapply[scalaz.Traverse,List[Int]]{type M[X] = List[X]; type A = Int}#M[Int]) = (4,List(2, 4, 6, 8))
```

EIP:

> The second kind of traversal modiﬁes elements purely but dependent on the state, with a binary function of type `a → b → c`, evolving this state independently of the elements, via a computation of type `m b`.

This is the same as `traverseS`. Here's how we can implement `label`:

```scala
scala> def label[F[_]: Traverse, A](f: F[A]): F[Int] =
         (f.traverseS {_ => for {
           n <- get[Int]
           x <- put(n + 1)
         } yield n}) eval 0
label: [F[_], A](f: F[A])(implicit evidence\$1: scalaz.Traverse[F])F[Int]
```

It's ignoring the content of the data structure, and replacing it with a number starting with 0. Very effecty. Here's how it looks with `List` and `Tree`:

```scala
scala> label(List(10, 2, 8))
res176: List[Int] = List(0, 1, 2)

scala> label(tree).drawTree
res177: String =
"0
|
1+-
|
2`-
"
```

### Links

EIP seems to be a popular paper to cover among Scala fp people.

[Eric Torreborre (@etorreborre)](https://twitter.com/etorreborre)'s [The Essence of the Iterator Pattern](http://etorreborre.blogspot.com/2011/06/essence-of-iterator-pattern.html) is the most thorough study of the paper. It also covers lots of ground works, so it's worth digging in.

[Debasish Ghosh (@debasishg)](https://twitter.com/debasishg)'s [Iteration in Scala - effectful yet functional](http://debasishg.blogspot.in/2011/01/iteration-in-scala-effectful-yet.html) is shorter but covering the good part by focusing on Scalaz.

[Marc-Daniel Ortega (@patterngazer)](https://twitter.com/patterngazer)'s [Where we traverse, accumulate and collect in Scala](http://patterngazer.blogspot.com/2012/03/where-we-traverse-accumulate-and.html) also covers `sequence` and `collect` using Scalaz.

We'll pick it up from here later.
