# Qidong

Qidong is a Scala pipeline library which provides generic type safe combinators to glue functions together.

# Features
- type safe function composition
- resumable computation
- call trace
- state update notification
- error handling
- onFinish

# Get Start

it is still under construction. please clone it from the link above

# First Program
Suppose that we have three functions:
```scala
val f1 = (i: Int) => i + 1
val f2 = (i: Int) => i + 2
val f3 = (i: Int) => i + 3
```
We can glue them together by the operator `=>:`
```scala
import scalaz._ , Scalaz._
import qidong.pipeline.ops._
val fs = f1 =>: f2 =>: f3
```
`fs` is a value which should be evaluated in a context: (currently support Scalaz Task and Monix Task)
```scala
import scalaz.concurrent.Task
val task = fs.run[Task](0)
```
the return value `task` is a value which has the type of `Task[\/[MFailure[Task, MSuccess[Int]], MSuccess[Int]]]`. it is a recursive type on MSuccess. which can be read as the expression will be evaluated to MSucess[Int] when no failure occurs, will be evaluated to MFailure[Task, MSuccess[Int]] when failure ocurs and if we keep trying (call resume method on it) it will eventually evaluated to MSuccess[Int], potentially infinitely. 

To get the result of it, we can call one of Scalaz Task's `unsafe-*` methods:
```scala
val \/-(ret) = task.unsafePerformSync
```
if all good, we get back a value of type `\/-[MSuccess[Int]]`.
`MSucess` is defined as:
```scala
final case class MSuccess[IO](trace: Tree[MTraceNode], data: IO)
```
the `data` is the return value of evaluating the whole expression. the `trace` looks like this (call println(ret.trace.drawTree)):
```
MRoot
+- MSuccNode(1df5d6ad-deab-49e1-baff-0c4f0bdace3d,Timing(2017-06-02T13:10:26.365,2017-06-02T13:10:26.368))
+- MSuccNode(f3ae0541-fa65-4a04-81df-c3ca24b1747a,Timing(2017-06-02T13:10:26.368,2017-06-02T13:10:26.369))
+- MSuccNode(35db7231-4272-4d33-9d24-250ab0eb556d,Timing(2017-06-02T13:10:26.369,2017-06-02T13:10:26.369))
```
which is the logs or call trace of the evaluation. where the first part of MSuccNode is an uuid which is used as an identitifer and second part recording the start and end time of running the function. MSuccNode indicates that the computation is successfully completed. apart from MSuccNode there are other nodes I will talk about later.

## Naming
The uuid reprensentation of a function is not very human readable. We can give each function a name:
```scala
val f1 = ((i: Int) => i + 1).name("function 1")
val f2 = ((i: Int) => i + 2).name("function 2")
val f3 = ((i: Int) => i + 3).name("function 3")
```
then re-evaluate the expression:
```scala
import qidong.pipeline.ops._
val fs = f1 =>: f2 =>: f3
val \/-(ret) = fs.run[Task](0).unsafePerformSync
println(ret.trace.drawTree)
```
the output will be:
```
MRoot
+- MSuccNode(function 1,Timing(2017-06-02T13:22:07.797,2017-06-02T13:22:07.800))
+- MSuccNode(function 2,Timing(2017-06-02T13:22:07.800,2017-06-02T13:22:07.800))
`- MSuccNode(function 3,Timing(2017-06-02T13:22:07.800,2017-06-02T13:22:07.800))
```
## When Exception happens
Scala function can throw exceptions:
```scala
val f1 = ((i: Int) => i + 1).name("function 1")
val f2 = ((i: Int) => {throw new Exception("oops"); i + 2}).name("function 2")
val f3 = ((i: Int) => i + 3).name("function 3")
val fs = f1 =>: f2 =>: f3
val -\/(fail) = fs.run[Task](0).unsafePerformSync
```
evaluation of `fs` will return a `MFailure` object which among other stuff also has a `trace` attribute of type `Tree[MTraceNode]`.
let's print the trace:
```scala
println(fail.trace.drawTree)
```
and the output looks like:
```
MRoot
+- MSuccNode(function 1,Timing(2017-06-02T13:37:04.837,2017-06-02T13:37:04.837))
+- MFailNode(function 2,java.lang.Exception: oops,Timing(2017-06-02T13:37:04.837,2017-06-02T13:37:04.837))
`- MNotRunNode(function 3)
```
beside `MSuccNode`, there are other two nodes:
- The `MFailNode` indicates which functions is failed, when and why. 
- The `MNotRunNode` tells which functions are not running because of previous failure.
so, the `trace` gives us not only which functions are evaluated successfully, or failed but also tells us which functions are not evaluated in this expression.

## Type safety
The `=>:` combinator can compose any function in a sensible way. Compilation error will occurs when two function is not compatiable. in terms of compatiable I mean that the first function's return type should be subtype of the second function's parameter type.
```scala
   import shapeless.test.illTyped
   import qidong.pipeline.ops._
    val m1 = (i: Int) => "a"
    val m2 = (s: String) => 1
    val m3 = (s: String) => 1
    val ms = m1 =>: m2
    illTyped("""m2 =>: m3""")
```
`m1` can compose with `m2` because `m1`'s return type(String) is compatiable to `m2`'s parameter type(String).
`m2` can not compose with `m3` because `m2`'s return type(Int) is incompatiable to `m3`'s parameter type(String). Incompatiable combination will fail the compiler.

and here is an example of subtype relation:
```scala
    class A
    class B extends A
    class C extends B

    val f1 = () => new C
    val f2 = (b: B) => new A
    val fs = f1 =>: f2
```
`f1` and `f2` are composable because f1's return type( C ) is subtype of f2's parameter type(B).

There are two classes of functions `() => A` and `Unit => A`, where `A` is a type parameter, which are internally transformed to `Any => A`. `Any` is the top type in Scala so that they can follow any other functions.
```scala
    val m1 = () => 1
    val m2 = (Unit: Unit) => 1
    val m3 = (i: Int) => i.toString
    val ms1 = m3 =>: m1
    val ms2 = m3 =>: m2
```
both `ms1` and `ms2` are type checked.

## Effectful type
functions have the shape `I => F[O]` is effectful when F is one of the following type constructor:
- Either[Throwable, ?]
- Try
- \\/[Throwable,?]
- Future

`I` and `O` are type parameters. \\/ is the scalaz disjunction type.
composition of those functions follow the same compatiable rule.
```scala
    val f1 = (i: Int) => Try(i)
    val f2 = (i: Int) => i.right[Throwable]
    val f3 = (i: Int) => Future(i)
    val f4: Int => Either[Throwable, Int] = (i: Int) => Right(i)
    val f5 = (i:Int) => i
    val ms = f1 =>: f2 =>: f3 =>: f4 =>: f5 =>: f1
```
`ms` is a valid expression and type checked.

## Aside type safety
Qidong use shapeless' HList as its internal reprensentation. therefore, you can circumvent the validation checking by directly using `::`:
```scala
    val f1 = (i: Int) => Try(i)
    val ms = f1 :: "xyz" :: 123 :: HNil
```
`ms` is type checked. but when you try to evaluate `ms`, you will get a compilation error.(implicit value can not be found blah, blah)
```scala
  val ev = ms.run[Task](0)
```
it fails at compilation time, not runtime anyway.

## Adaptation
when we want to compose two functions where the subtype relation is not hold, Qidong provides a few methods to adjust the input and output of functions as per your need.
suppose we have two functions in hand which normally can not be composed:
```scala
    import shapeless.test.illTyped
    val m1 = ((i: Int) => "a").name("m1")
    val m2 = ((i: Int) => 1).name("m2")
    illTyped("""m1 =>: m2""")
```
### mapfst
we can use `mapfst` to adjust `m2`'s input so that they can be composed:
```scala
    val ms = m1 =>: m2.mapfst((s: String) => s.toInt + 1)
```
`mapfst` also works for grouped functions:
```scala
    val m3 = (i:Int) => i + 1
    val ms =  m1 =>: (m2 =>: m3).mapfst((s: String) => s.toInt + 1)
```
which is equivalent to:
```scala
    val m3 = (i:Int) => i + 1
    val ms =  m1 =>: m2.mapfst((s: String) => s.toInt + 1) =>: m3
```

### mapsnd
or we can use `map` or `mapsnd` to adjust `m1`'s output
```scala
    val ms2 = m1.mapsnd(_.toInt) =>: m2
    val ms3 = m1.map(_.toInt) =>: m2
```
`map` and `mapsnd` also work for grouped functions:
```scala
    val m3 = (i:Int) => i + 1
    val ms2 = (m3 =>: m1).mapsnd(_.toInt) =>: m2
    val ms3 = (m3 =>: m1).map(_.toInt) =>: m2
```
which is equivalent to
```scala
    val m3 = (i:Int) => i + 1
    val ms2 = m3 =>: m1.mapsnd(_.toInt) =>: m2
    val ms3 = m3 =>: m1.map(_.toInt) =>: m2
```

the internal representation of a function is essential a Scalaz's UpStarF type which is an instance of Profunctor.

### keep
the `keep` method simply copy first function's input to its output:
```scala
    val m1 = (i: Int) => i
    val m2 = (i: Int, j: Int) => i + j
    val ms = m1.keep =>: m2.tupled
```
### long distance keep
for grouped functions, the `keep` method copy first function's input to the last function's output:
```scala
    val m1 = (i: Int) => i
    val m2 = (i: Int, j: Int) => i + j
    val ms = (m1 =>: m1 =>: m1).keep =>: m2.tupled
```
the first m1's input will be copied to the output of the group.
internally, Qidong rewrites all the functions in the group.

## Resume computation
evaluation failure is resumable.

failed evaluation of an expression will return a `Mfailure` object. which is defined as:
```scala
  final case class MFailure[E[_], O](
    name: String,
    resume: () => E[\/[MFailure[E, O], O]],
    ex: Throwable,
    trace: Tree[MTraceNode],
    timing: Timing)
```
the `resume` attribute is a continuation which can be used to continue the evaluation from the failure-point. when `resume()` is called on a failed computation, Qidong will re-evaluate the last failed function and if success, going on to the end.
```scala
  import qidong.pipeline.ops._
  val m1 = ((i: Int) => { i + 1 }).name("m1")
  val m3 = ((i: Int) => { i + 1 }).name("m3")
    var flag = 0
  val rm2 = (i: Int) => 
      if (flag == 0) { flag += 1; throw new Exception("haha")
      } else i + 1
  val ms = m1 =>: rm2.name("rm2") =>: m3
  val -\/(ret) = ms.run[Task].apply(1).unsafePerformSync
  val \/-(resumed) = ret.resume().unsafePerformSync
```
Resume does respect to the `resume-boundary` when we set up a function group then resume is not restarted from the failure point but the beginning of the group:
```scala
  import qidong.pipeline.ops._
  val m1 = ((i: Int) => { i + 1 }).name("m1")
  val m3 = ((i: Int) => { i + 1 }).name("m3")
    var flag = 0
  val rm2 = (i: Int) => 
      if (flag == 0) { flag += 1; throw new Exception("haha")
      } else i + 1
  val ms = (m1 =>: rm2.name("rm2")).name("group1") =>: m3
  val -\/(ret) = ms.run[Task].apply(1).unsafePerformSync
  val \/-(resumed) = ret.resume().unsafePerformSync
```
when resumed, the computation start from `group1`'s start function m1 in this case.

## Group functions
naming a group:
```scala
val g1 = (m1 =>: m2 =>: m3).name("group1")
```
when you give a name to a few of functions connected by `=>:`, you create a logic group.
here is a example of grouped functions and its output.
```scala
  import qidong.pipeline.ops._
  val m0 = ((i: Int) => { println("call m0"); i + 1 }).name("m0")
  val m1 = ((i: Int) => { println("call m1"); i + 1 }).name("m1")
  val m2 = ((i: Int) => { println("call m2"); i + 1 }).name("m2")
  val m3 = ((i: Int) => { println("call m3"); throw new Exception("aa"); i + 1 }).name("m3")
  val m4 = ((i: Int) => { println("call m4"); i + 1 }).name("m4")
  val m5 = ((i: Int) => { println("call m5"); i + 1 }).name("m5")
  val ms = m0 =>: (m1 =>: m2).name("group1") =>: m5 =>: (m3 =>: m4).name("group2") =>: m5
  val -\/(ret) = ms.run[Task](1).unsafePerformSync
  println(ret.trace.drawTree)
```
will output:
```
MRoot
+- MSuccNode(m0,Timing(2017-06-02T17:21:46.276,2017-06-02T17:21:46.283))
+- MGroupNode(group1)
|  +- MSuccNode(m1,Timing(2017-06-02T17:21:46.285,2017-06-02T17:21:46.285))
|  `- MSuccNode(m2,Timing(2017-06-02T17:21:46.285,2017-06-02T17:21:46.286))
+- MSuccNode(m5,Timing(2017-06-02T17:21:46.287,2017-06-02T17:21:46.287))
+- MGroupNode(group2)
|  +- MFailNode(m3,Timing(2017-06-02T17:21:46.287,2017-06-02T17:21:46.287))
|  `- MNotRunNode(m4)
`- MNotRunNode(m5)
```
the group structure is preserved.
## Error handling
Error handling can be applied on function level:
```scala
    val ms = m2 =>: m3.handleError((i, ex) => 1) =>: m4 =>: m5
    val \/-(ret) = ms.run[Task](0).unsafePerformSync
```
where, i is the input parameter of m3 and ex is Throwable
`handleError` take a function which has the type: 
```scala
 (I, Throwable) => O
```
where I is the parameter type and O is the return type of the function

## State update notification
State update notification will be fired after a function is completely evaluated. we can setup a listener to get notified:
```scala
    val mm = m3.stateUpdate(println)
    val ms = m0 =>: mm =>: m2 =>: m3
```
when complete evaluation of mm an event will be fired.
`stateUpdate` take a function which has the type:
```scala
MCompleted => Unit
```
where MCompleted should be one of the following type:
```scala
final case class MFailNode(override val name: String, ex: Throwable, timing: Timing)
final case class MSuccNode(override val name: String, timing: Timing)
final case class MRecoveredByErrorHandlerNode(override val name: String, ex: Throwable, timing: Timing)
```

## onFinish handler
We can attach to each function an onFinish handler which is guarenteed to be called after the function is complete evaluated, successfully or with failure:
```scala
    val ms = m0 =>: m1 =>: m2.onFinish(h => println(h)) =>: m4
    val \/-(ret) = ms.run[Task](0).unsafePerformSync
```
`onFinish` take a function which has the type:
```scala
I => Unit
```
where I is the parameter type of the function.

## Expression tree
once we have an expression, we can show its structure without evaluating it:
```scala
  println(ms.drawTree)
```
will output a tree-like structure:
```
"root"
+- "m0"
+- "group1"
|  -- "m1"
|  -- "m2"
+- "m5"
+- "group2"
|  -- "m3"
|  -- "m4"
`- "m5"
```
## Internal Detail
- Qidong relies on shapeless and scalaz. 
- the expression generated by `=>:` operator is nothing but a HList.
- functions internally wrapped in M case class, and grouped functions, Ms case class.
- auto-transformation from normal function to M and Ms is implemented in MBuilder.
- Excution Context adaption is implemented in Evalable
- Evaluation is implemented in Decomposer

## About the name of the project
https://en.wikipedia.org/wiki/Qidong,_Jiangsu

## Licence
Apache 2.0



