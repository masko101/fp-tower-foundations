package exercises.dataprocessing

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  def zip[A, B](semigroupA: Semigroup[A], semigroupB: Semigroup[B]): Semigroup[(A, B)] =
    (x: (A, B), y: (A, B)) => (semigroupA.combine(x._1, y._1), semigroupB.combine(x._2, y._2))

  def sumNumber[T](implicit numeric: Numeric[T]): Semigroup[T] = (x: T, y: T) => numeric.plus(x, y)

  val sumInt: Semigroup[Int] = sumNumber[Int]

  val sumDouble: Semigroup[Double] = (x: Double, y: Double) => x + y

  def compare[T](compare: (T, T) => T): Semigroup[T] = (x: T, y: T) => compare(x, y)

  def minBy[From, To: Ordering](f: From => To): Semigroup[From] =
    (x: From, y: From) => Ordering.by(f).min(x, y)

  def maxBy[From, To: Ordering](f: From => To): Semigroup[From] =
    (x: From, y: From) => Ordering.by(f).max(x, y)

  val minSample: Semigroup[Sample] = {
    minBy((s: Sample) => s.temperatureFahrenheit)
  }

  val maxSample: Semigroup[Sample] = {
    maxBy((s: Sample) => s.temperatureFahrenheit)
  }

  val minInt: Semigroup[Int] = minBy(identity)

  val maxInt: Semigroup[Int] = maxBy(identity)

  val sumAndSize: Semigroup[(Double, Int)] = zip(sumDouble, sumInt)

//  def mergeMap[Key, T](semigroup: Semigroup[T]): Semigroup[Map[Key, T]] = (x: Map[Key, T], y: Map[Key, T]) => x. semigroup.combine()

}
