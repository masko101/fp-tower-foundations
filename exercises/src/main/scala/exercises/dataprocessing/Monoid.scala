package exercises.dataprocessing

trait Monoid[A] {
  val default: A
  def combine(x: A, y: A): A
}

object Monoid {

  def zip[A, B](monoidA: Monoid[A], monoidB: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override val default: (A, B)                       = (monoidA.default, monoidB.default)
    override def combine(x: (A, B), y: (A, B)): (A, B) = (monoidA.combine(x._1, y._1), monoidB.combine(x._2, y._2))
  }

  val minInt: Monoid[Int] = new Monoid[Int] {
    override val default: Int                 = 0
    override def combine(x: Int, y: Int): Int = math.min(x, y)
  }

  val maxInt: Monoid[Int] = new Monoid[Int] {
    override val default: Int                 = 0
    override def combine(x: Int, y: Int): Int = math.max(x, y)
  }

  def sumNumber[T](implicit numeric: Numeric[T]): Monoid[T] = new Monoid[T] {
    override val default: T             = Numeric[T].zero
    override def combine(x: T, y: T): T = numeric.plus(x, y)
  }

  val sumInt: Monoid[Int] = sumNumber[Int]

  val sumDouble: Monoid[Double] = new Monoid[Double] {
    override val default: Double = 0

    override def combine(x: Double, y: Double): Double =
      x + y
  }

  val compareSample: ((Sample, Sample) => Sample) => Monoid[Option[Sample]] = (compare: (Sample, Sample) => Sample) =>
    new Monoid[Option[Sample]] {
      override val default: Option[Sample] = None
      override def combine(x: Option[Sample], y: Option[Sample]): Option[Sample] =
        (x, y) match {
          case (Some(x), Some(y)) => Some(compare(x, y))
          case (Some(x), None)    => Some(x)
          case (None, Some(y))    => Some(y)
          case (None, None)       => None
        }
    }

  def compare[T](compare: (T, T) => T): Monoid[Option[T]] =
    new Monoid[Option[T]] {
      override val default: Option[T] = None
      override def combine(x: Option[T], y: Option[T]): Option[T] =
        (x, y) match {
          case (Some(x), Some(y)) => Some(compare(x, y))
          case (Some(x), None)    => Some(x)
          case (None, Some(y))    => Some(y)
          case (None, None)       => None
        }
    }

  val minSample: Monoid[Option[Sample]] =
    compareSample((x, y) => if (x.temperatureFahrenheit < y.temperatureFahrenheit) x else y)

  val maxSample: Monoid[Option[Sample]] =
    compareSample((x, y) => if (x.temperatureFahrenheit > y.temperatureFahrenheit) x else y)

  val minSummary: Monoid[Option[Sample]] =
    compareSample((x, y) => if (x.temperatureFahrenheit < y.temperatureFahrenheit) x else y)

  val maxSummary: Monoid[Option[Sample]] =
    compareSample((x, y) => if (x.temperatureFahrenheit > y.temperatureFahrenheit) x else y)

  val sumAndSize: Monoid[(Double, Int)] = zip(sumDouble, sumInt)

}
