package exercises.dataprocessing

import scala.collection.mutable

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

  def minByOption[From, To: Ordering](f: From => To): Monoid[Option[From]] = compare(Semigroup.minBy(f).combine)

  def maxByOption[From, To: Ordering](f: From => To): Monoid[Option[From]] = compare(Semigroup.maxBy(f).combine)

  val minSample: Monoid[Option[Sample]] = minByOption((s: Sample) => s.temperatureFahrenheit)

  val maxSample: Monoid[Option[Sample]] = maxByOption((s: Sample) => s.temperatureFahrenheit)

  val sumAndSize: Monoid[(Double, Int)] = zip(sumDouble, sumInt)

  def mergeMap[Key, T](semigroup: Semigroup[T]): Monoid[Map[Key, T]] = new Monoid[Map[Key, T]] {
    override val default: Map[Key, T] = Map.empty[Key, T]
    override def combine(x: Map[Key, T], y: Map[Key, T]): Map[Key, T] =
      y.foldLeft(x)((x, ye) =>
        x.updatedWith(ye._1) {
          case Some(xe) => Some(semigroup.combine(xe, ye._2))
          case None     => Some(ye._2)
        }
      )

  }

//  def mergeMapMutable[Key, T](semigroup: Semigroup[T]): Monoid[mutable.Map[Key, T]] = new Monoid[mutable.Map[Key, T]] {
//    override val default: mutable.Map[Key, T] = mutable.Map.empty[Key, T]
//    override def combine(x: mutable.Map[Key, T], y: mutable.Map[Key, T]): mutable.Map[Key, T] =
//      y.foldLeft(x)((x, ye) => x.uupdatedWith(ye._1)(xe => Some(semigroup.combine(xe.getOrElse(default), ye._2))))
//
//  }
}
