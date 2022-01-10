package exercises.dataprocessing

import exercises.dataprocessing.Semigroup.{maxSample, minSample, sumDouble, sumInt}

object SummarySemi {
  val summarySemigroup: Semigroup[SummarySemi] =
    (x: SummarySemi, y: SummarySemi) =>
      SummarySemi(minSample.combine(x.min, y.min),
                  maxSample.combine(x.max, y.max),
                  sumDouble.combine(x.sum, y.sum),
                  sumInt.combine(x.size, y.size))
}

case class SummarySemi(
  min: Sample, // Sample with lowest temperature
  max: Sample, // Sample with highest temperature
  sum: Double, // sum of all temperatures in Fahrenheit
  size: Int    // number of Samples
) {

  def average: Option[Double] =
    Option.unless(size == 0)(sum / size)

  override def toString: String =
    f"Summary(avg = ${average.getOrElse(0.0)}%.2f, " +
      s"size = $size,\n  " +
      s"min = $min,\n  " +
      s"max = $max\n)"
}
/*
package exercises.dataprocessing

import exercises.dataprocessing.Monoid.{maxSample, minSample, sumDouble, sumInt}

object Summary {
  val summaryMonoid: Monoid[Summary] = new Monoid[Summary] {
    override val default: Summary = Summary(None, None, 0, 0)

    override def combine(x: Summary, y: Summary): Summary =
      Summary(minSample.combine(x.min, y.min),
        maxSample.combine(x.max, y.max),
        sumDouble.combine(x.sum, y.sum),
        sumInt.combine(x.size, y.size))
  }
}

case class Summary(
                    min: Option[Sample], // Sample with lowest temperature
                    max: Option[Sample], // Sample with highest temperature
                    sum: Double,         // sum of all temperatures in Fahrenheit
                    size: Int            // number of Samples
                  ) {

  def average: Option[Double] =
    Option.unless(size == 0)(sum / size)

  override def toString: String =
    f"Summary(avg = ${average.getOrElse(0.0)}%.2f, " +
      s"size = $size,\n  " +
      s"min = $min,\n  " +
      s"max = $max\n)"
}
 */
