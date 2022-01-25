package exercises.dataprocessing

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.reflect.ClassTag

// For example, here is a ParArray[Int] with two partitions:
// ParArray(
//  List(1,2,3,4,5,6,7,8), // partition 1
//  List(9,10)             // partition 2
// )
// Note that we used the `apply` method with a varargs argument.
case class ParArray[A: ClassTag](data: Array[A], partitionSize: Int)(implicit executionContext: ExecutionContext) {
  val partitions: Array[Range] = {
    val array: Array[Range] =
      if (data.length < 1)
        Array.empty[Range]
      else {
        val numPartitions: Int = data.length / partitionSize + 1
        (0 until numPartitions)
          .map(pn => {
            val parStart = pn * partitionSize
            val parEnd   = math.max(0, if (pn != numPartitions - 1) parStart + partitionSize - 1 else data.length - 1)
            parStart to parEnd
          })
          .toArray
      }

    val arr = array.map(r => s"${r.start} => ${r.end}").mkString(",")
    array
  }
  val str = partitions.map(r => s"${r.start} => ${r.end}").mkString(",")

  def toList: List[A] = data.toList
  def size: Int       = parFoldMap(_ => 1)(Monoid.sumInt)

  override def toString: String = s"ParArray($str,$partitionSize)"

  def minByOption[To: ClassTag](zoom: A => To)(implicit ordering: Ordering[To]): Option[A] = {
    val semi = Semigroup.minBy(zoom)
    def reducePartition(as: Range): A =
      as.map(a => a).toList match {
        case head :: rest =>
          rest.foldLeft(data(head))((to, ai) => {
            semi.combine(to, data(ai))
          })
      }
    partitions
      .filter(_.nonEmpty)
      .map(reducePartition)
      .reduceLeftOption(semi.combine)
  }

  def map[To: ClassTag](f: A => To): ParArray[To] = this.copy(data.map(f))

  def foldLeft[To](default: To)(combine: (To, A) => To)(combineR: (To, To) => To): To =
    partitions.foldLeft(default)((a, i) => combineR(a, i.foldLeft(default)((to, ai) => combine(to, data(ai)))))

  // Only works correctly when the default is such that combine(default, x) is equal to x
  def monoFoldLeft(mp: Monoid[A]): A =
    partitions.foldLeft(mp.default)((a, i) =>
      mp.combine(a, i.foldLeft(mp.default)((to, ai) => mp.combine(to, data(ai))))
    )

  def foldMap[To](zoom: A => To)(monoid: Monoid[To]): To =
    partitions.foldLeft(monoid.default)((pto, pa) =>
      monoid.combine(pto, pa.foldLeft(monoid.default)((to, a) => monoid.combine(to, zoom(data(a)))))
    )

  def parFoldMap[To: ClassTag](zoom: A => To)(monoid: Monoid[To]): To = {
    partitions
      .map(pa =>
        Future {
          val res = pa.foldLeft(monoid.default)((to, a) => monoid.combine(to, zoom(data(a))))
          res
        }(executionContext)
      )
      .map(Await.result(_, 1.minutes))
      .foldLeft(monoid.default)(monoid.combine)
  }

  private def reducePartition[To](zoom: A => To)(semigroup: Semigroup[To])(as: Range): To = {
    var res: To = zoom(data(as.head))
    for (x <- as)
      res = semigroup.combine(res, zoom(data(x)))
    res
  }

  def reduceMap[To: ClassTag](zoom: A => To)(semigroup: Semigroup[To]): Option[To] = {
    val reduce = reducePartition(zoom)(semigroup)(_)
    partitions
      .filter(_.nonEmpty)
      .map(reduce)
      .reduceLeftOption(semigroup.combine)
  }

  def parReduceMap[To: ClassTag](zoom: A => To)(semigroup: Semigroup[To]): Option[To] = {
    val reduce = (as: Range) => Future { reducePartition(zoom)(semigroup)(as) }
    partitions
      .filter(_.nonEmpty)
      .map(reduce)
      .map(Await.result(_, 1.minutes))
      .reduceLeftOption(semigroup.combine)
  }

}

object ParArray {
  // The `*` at the end of List[A] is called a varargs. It means we can put as many arguments
  // as we want of type List[A] and the Scala compiler will automatically packs these arguments
  // into a collection.
  // For example, ParArray(List(1,2), List(3,4)) == ParArray(List(List(1,2), List(3,4)))
  // This is why we can create a List using the syntax List(1,2,3) instead of 1 :: 2 :: 3 :: Nil
  //  def apply[A](partitions: List[A]*)(implicit executionContext: ExecutionContext): ParArray[A] =
  //    ParArray(partitions.toList)

  // Creates a ParArray by grouping a List into partitions of fixed size.
  // If the length of input list is not divisible by the partition size, then
  // the last partition will be smaller. For example:
  // byPartitionSize(3, List(1,2,3,4,5,6,7,8,9,10)) == ParArray(
  //   List(1,2,3),
  //   List(4,5,6),
  //   List(7,8,9),
  //   List(10)
  // )
  def byPartitionSize[A: ClassTag](partitionSize: Int,
                                   items: List[A])(implicit executionContext: ExecutionContext): ParArray[A] =
    //Need a better way t ovalidate partition size
    ParArray(items.toArray, math.max(1, partitionSize))
}
