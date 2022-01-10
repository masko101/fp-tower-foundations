package exercises.dataprocessing

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

object Semigroup {
  def zip[A, B](semigroupA: Semigroup[A], semigroupB: Semigroup[B]): Semigroup[(A, B)] =
    (x: (A, B), y: (A, B)) => (semigroupA.combine(x._1, y._1), semigroupB.combine(x._2, y._2))

  val minInt: Semigroup[Int] = (x: Int, y: Int) => math.min(x, y)

  val maxInt: Semigroup[Int] = (x: Int, y: Int) => math.max(x, y)

  def sumNumber[T](implicit numeric: Numeric[T]): Semigroup[T] = (x: T, y: T) => numeric.plus(x, y)

  val sumInt: Semigroup[Int] = sumNumber[Int]

  val sumDouble: Semigroup[Double] = (x: Double, y: Double) => x + y

  def compare[T](compare: (T, T) => T): Semigroup[T] = (x: T, y: T) => compare(x, y)

  val minSample: Semigroup[Sample] =
    compare((x, y) => if (x.temperatureFahrenheit < y.temperatureFahrenheit) x else y)

  val maxSample: Semigroup[Sample] =
    compare((x, y) => if (x.temperatureFahrenheit > y.temperatureFahrenheit) x else y)

  val sumAndSize: Semigroup[(Double, Int)] = zip(sumDouble, sumInt)

}
