
### Unapply

One thing that I've been fighting the Scala compiler over is the lack of type inference support across the different kinded types like `F[M[_, _]]` and `F[M[_]]`, and `M[_]` and `F[M[_]]`.

For example, an instance of `Applicative[M[_]]` is `(* -> *) -> *` (a type constructor that takes another type constructor that takes exactly one type). It's known that `Int => Int` can be treated as an applicative by treating it as `Int => A`:

```scala
scala> Applicative[Function1[Int, Int]]
<console>:14: error: Int => Int takes no type parameters, expected: one
              Applicative[Function1[Int, Int]]
                          ^

scala> Applicative[({type l[A]=Function1[Int, A]})#l]
res14: scalaz.Applicative[[A]Int => A] = scalaz.std.FunctionInstances\$\$anon\$2@56ae78ac
```

This becomes annoying for `M[_,_]` like `Validation`. One of the ways Scalaz helps you out is to provide meta-instances of typeclass instances called [`Unapply`]($scalazBaseUrl$/core/src/main/scala/scalaz/Unapply.scala).

```scala
trait Unapply[TC[_[_]], MA] {
  /** The type constructor */
  type M[_]
  /** The type that `M` was applied to */
  type A
  /** The instance of the type class */
  def TC: TC[M]
  /** Evidence that MA =:= M[A] */
  def apply(ma: MA): M[A]
}
```

When Scalaz method like `traverse` requires you to pass in `Applicative[M[_]]`, it instead could ask for `Unapply[Applicative, X]`. During compile time, Scalac can look through all the implicit converters to see if it can coerce `Function1[Int, Int]` into `M[A]` by fixing or adding a parameter and of course using an existing typeclass instance.

```scala
scala> implicitly[Unapply[Applicative, Function1[Int, Int]]]
res15: scalaz.Unapply[scalaz.Applicative,Int => Int] = scalaz.Unapply_0\$\$anon\$9@2e86566f
```

We can treat `Int` as `Applicative`. But because it requires `TC0: TC[({type λ[α] = A0})#λ]` implicitly, it does not allow just any type to be promoted as `Applicative`.

```scala
scala> implicitly[Unapply[Applicative, Int]]
res0: scalaz.Unapply[scalaz.Applicative,Int] = scalaz.Unapply_3\$\$anon\$1@5179dc20

scala> implicitly[Unapply[Applicative, Any]]
<console>:14: error: Unable to unapply type `Any` into a type constructor of kind `M[_]` that is classified by the type class `scalaz.Applicative`
1) Check that the type class is defined by compiling `implicitly[scalaz.Applicative[<type constructor>]]`.
2) Review the implicits in object Unapply, which only cover common type 'shapes'
(implicit not found: scalaz.Unapply[scalaz.Applicative, Any])
              implicitly[Unapply[Applicative, Any]]
                        ^
```

Works. The upshot of all this is that we can now rewrite the following a bit cleaner:

```scala
scala> val failedTree: Tree[Validation[String, Int]] = 1.success[String].node(
         2.success[String].leaf, "boom".failure[Int].leaf)
failedTree: scalaz.Tree[scalaz.Validation[String,Int]] = <tree>

scala> failedTree.sequence[({type l[X]=Validation[String, X]})#l, Int]
res2: scalaz.Validation[java.lang.String,scalaz.Tree[Int]] = Failure(boom)
```

Here's [sequenceU]($scalazBaseUrl$/core/src/main/scala/scalaz/Traverse.scala):

```scala
scala> failedTree.sequenceU
res3: scalaz.Validation[String,scalaz.Tree[Int]] = Failure(boom)
```

Boom.


#### More info on using Unapply ####

See the typelevel.org blog [post](http://typelevel.org/blog/2013/09/11/using-scalaz-Unapply.html) on using scalaz.unapply for some good examples. I've copied in the general gist of it below.

Lets take a look at Unapply with a simple example to sequence a List.

```scala
def sequenceList[F[_]: Applicative, A](xs: List[F[A]]): F[List[A]] =
    xs.foldRight(List.empty[A].point[F])((a, b) => ^(a, b)(_ :: _))
                                                  //> sequenceList: [F[_], A](xs: List[F[A]])(implicit evidence$2: scalaz.Applicat
                                                  //| ive[F])F[List[A]]
```

This works with:

```scala
sequenceList(List(some(1), some(2)))            //> res0: Option[List[Int]] = Some(List(1, 2))
sequenceList(List(some(1), none))               //> res1: Option[List[Int]] = None
```

That worked fine... because the type of List[Option[Int]] could be neatly destructured into F and A type params. It has the "shape" F[X].

But what happens if we use something else with a convenient Applicative instance like Either (`\/`):

```scala
sequenceList(List(\/.right(42), \/.left(NonEmptyList("oops"))))
<console>:23: error: no type parameters for method 
  sequenceList: (xs: List[F[A]])(implicit evidence$1: scalaz.Applicative[F])F[List[A]]
  exist so that it can be applied to arguments
  (List[scalaz.\/[scalaz.NonEmptyList[String],Int]])
 --- because ---
argument expression's type is not compatible with formal parameter type;
 found   : List[scalaz.\/[scalaz.NonEmptyList[String],Int]]
 required: List[?F]

              sequenceList(List(\/.right(42), \/.left(NonEmptyList("oops"))))
              ^
```

This is basically saying that you need to use a typelambda: ```({type λ[α] = NonEmptyList[String] \/ α})#λ```

```scala
sequenceList[({type l[A] = NonEmptyList[String] \/ A})#l, Int](List(\/.right(42), \/.left(NonEmptyList("oops"))))
                                                  //> res2: scalaz.\/[scalaz.NonEmptyList[String],List[Int]] = -\/(NonEmptyList(o
                                                  //| ops))
```

The problem was that NonEmptyList[String] `\/` Int has the shape F[A, B], whereas it wants F[A].

#### Finding an Unapply instance ####

[Unapply]($scalazBaseUrl$/core/src/main/scala/scalaz/Unapply.scala#L210) __does__ have instances matching the F[A, B] shape: unapplyMAB1 and unapplyMAB2 in it companion object; so always visible.

Lets seee if one of them works.

```scala
//Unapply.unapplyMAB1[Applicative, \/, NonEmptyList[String], Int]   <=== Either is right-biased so this one doesn't apply
val u = Unapply.unapplyMAB2[Applicative, \/, NonEmptyList[String], Int]
                                                  //> u  : scalaz.Unapply[scalaz.Applicative,scalaz.\/[scalaz.NonEmptyList[String
                                                  //| ],Int]]{type M[X] = scalaz.\/[scalaz.NonEmptyList[String],X]; type A = Int}
                                                  //|  = scalaz.Unapply_0$$anon$13@6bba7d61
```

This effectively hides the typelambda inside Unapply.
Here, the type res7.M represents the typelambda being passed to sequenceList. You can see this work here:

```scala
val l = sequenceList[u.M, u.A](List(\/.right(42), \/.left(NonEmptyList("oops"))))
                                                  //> l  : scalaz.stuff.unapply.u.M[List[scalaz.stuff.unapply.u.A]] = -\/(NonEmpt
                                                  //| yList(oops))

l: NonEmptyList[String] \/ List[Int]              //> res3: scalaz.\/[scalaz.NonEmptyList[String],List[Int]] = -\/(NonEmptyList(o
                                                  //| ops))
                                                  
```

The ```l: NonEmptyList[String] \/ List[Int]``` conformance test shows that Scala can still reduce the path-dependent u.M and u.A types at this level, outside sequenceList.

Note that scalaZ provides [sequenceU]($scalazBaseUrl$/core/src/main/scala/scalaz/Traverse.scala#L108) which takes care of the Unapply for us...

Now that we have worked out Unapply, we can abstract this sequenceList function so that it works for other types and not just Either (`\/`).

```
def sequenceListU[FA](xs: List[FA])(implicit U: Unapply[Applicative, FA]): U.M[List[U.A]] =
  sequenceList(U.leibniz.subst(xs))(U.TC)         //> sequenceListU: [FA](xs: List[FA])(implicit U: scalaz.Unapply[scalaz.Applica
                                                  //| tive,FA])U.M[List[U.A]]

sequenceListU(List(\/.right(42), \/.left(NonEmptyList("oops"))))
                                                  //> res4: scalaz.\/[scalaz.NonEmptyList[String],List[Int]] = -\/(NonEmptyList(o
                                                  //| ops))

```
