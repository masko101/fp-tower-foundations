package exercises.dataprocessing

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

// For example, here is a ParList[Int] with two partitions:
// ParList(
//  List(1,2,3,4,5,6,7,8), // partition 1
//  List(9,10)             // partition 2
// )
// Note that we used the `apply` method with a varargs argument.
case class ParList[A](partitions: List[List[A]])(implicit executionContext: ExecutionContext) {
  def toList: List[A] = partitions.flatten
  def size: Int       = parFoldMap(_ => 1)(Monoid.sumInt)

  def map[To](f: A => To): ParList[To] = this.copy(partitions.map(_.map(f)))

  def foldLeft[To](default: To)(combine: (To, A) => To)(combineR: (To, To) => To): To =
    partitions.foldLeft(default)((a, i) => combineR(a, i.foldLeft(default)(combine)))

  // Only works correctly when the default is such that combine(default, x) is equal to x
  def monoFoldLeft(mp: Monoid[A]): A =
    partitions.foldLeft(mp.default)((a, i) => mp.combine(a, i.foldLeft(mp.default)(mp.combine)))
//  def monoFoldLeft(default: A)(combine: (A, A) => A): A =
//    partitions.foldLeft(default)((a, i) => combine(a, i.foldLeft(default)(combine)))

  def foldMap[To](f: A => To)(monoid: Monoid[To]): To =
    partitions.foldLeft(monoid.default)((pto, pa) =>
      monoid.combine(pto, pa.foldLeft(monoid.default)((to, a) => monoid.combine(to, f(a))))
    )

  def parFoldMap[To](f: A => To)(monoid: Monoid[To]): To = {
//    val eventualTo = partitions.foldLeft(Future(monoid.default)(ec))((futurePto, pa) =>
//      futurePto.flatMap(pto =>
//        Future {
//          println(s"Stared ${Thread.currentThread().getName}")
//          val res = monoid.combine(pto, pa.foldLeft(monoid.default)((to, a) => monoid.combine(to, f(a))))
//          println(s"Finished ${Thread.currentThread().getName}")
//          res
//        }(ec)
//      )(ec)
//    )
//    Await.result(eventualTo, 1.minutes)

//    partitions.reduceLeftOption()
    partitions
      .map(pa =>
        Future {
          val res = pa.foldLeft(monoid.default)((to, a) => monoid.combine(to, f(a)))
          res
        }(executionContext)
      )
      .map(Await.result(_, 1.minutes))
      .foldLeft(monoid.default)(monoid.combine)
  }

  //   See `reduceFoldLeftOption` on `List`.
  //
  //   Could you implement `reduceMap` on `ParList`?
  //   def reduceMap[To](zoom: A => To)(combine: (To, To) => To): Option[To]
  //   or

  def reduceMap[To](zoom: A => To)(semigroup: Semigroup[To]): Option[To] = {
    def reducePartition(as: List[A]): To =
      as match {
        case head :: rest => rest.foldLeft(zoom(head))((to, a) => semigroup.combine(to, zoom(a)))
      }
    partitions
      .filter(_.nonEmpty)
      .map(reducePartition)
      .reduceLeftOption(semigroup.combine)
  }

  def parReduceMap[To](zoom: A => To)(semigroup: Semigroup[To]): Option[To] = {
    def reducePartition(as: List[A]): Future[To] = {
      Future {
        as match {
          case head :: rest => rest.foldLeft(zoom(head))((to, a) => semigroup.combine(to, zoom(a)))
        }
      }
    }
    partitions
      .filter(_.nonEmpty)
      .map(reducePartition)
      .map(Await.result(_, 1.minutes))
      .reduceLeftOption(semigroup.combine)
  }

}

object ParList {
  // The `*` at the end of List[A] is called a varargs. It means we can put as many arguments
  // as we want of type List[A] and the Scala compiler will automatically packs these arguments
  // into a collection.
  // For example, ParList(List(1,2), List(3,4)) == ParList(List(List(1,2), List(3,4)))
  // This is why we can create a List using the syntax List(1,2,3) instead of 1 :: 2 :: 3 :: Nil
  def apply[A](partitions: List[A]*)(implicit executionContext: ExecutionContext): ParList[A] =
    ParList(partitions.toList)

  // Creates a ParList by grouping a List into partitions of fixed size.
  // If the length of input list is not divisible by the partition size, then
  // the last partition will be smaller. For example:
  // byPartitionSize(3, List(1,2,3,4,5,6,7,8,9,10)) == ParList(
  //   List(1,2,3),
  //   List(4,5,6),
  //   List(7,8,9),
  //   List(10)
  // )
  def byPartitionSize[A](partitionSize: Int, items: List[A])(implicit executionContext: ExecutionContext): ParList[A] =
    if (items.isEmpty) ParList()
    else ParList(items.grouped(partitionSize).toList)
}
