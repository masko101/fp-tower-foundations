package exercises.dataprocessing

import scala.concurrent.ExecutionContext

object TemperatureExercises {
  // b. Implement `minSampleByTemperature` which finds the `Sample` with the coldest temperature.
  // `minSampleByTemperature` should work as follow:
  // Step 1: Find the local minimums (for each partition the `Sample` with the coldest temperature).
  // Step 2: Find the minimum value among the local minimums.
  // Note: We'll write test in the file `ParListTest.scala`
  def minSampleByTemperature(samples: ParList[Sample])(implicit executionContext: ExecutionContext): Option[Sample] = {
    samples.parFoldMap(Option[Sample])(Monoid.minSample)
//    samples.partitions.flatMap(_.minByOption(_.temperatureCelsius)).minByOption(_.temperatureCelsius)
  }

  def maxSampleByTemperature(samples: ParList[Sample]): Option[Sample] =
    samples.partitions.flatMap(_.maxByOption(_.temperatureCelsius)).maxByOption(_.temperatureCelsius)

  // c. Implement `averageTemperature` which finds the average temperature across all `Samples`.
  // `averageTemperature` should work as follow:
  // Step 1: Compute the sum of all samples temperatures
  //   a) Compute the sum per partition
  //   b) Sum-up the sum of each partition
  // Step 2: Compute the size of the dataset
  //   a) Compute the size of each partition
  //   b) Sum-up the size of each partition
  // Step 3: Divide the total temperature by the size of dataset.
  // In case the input `ParList` is empty we return `None`.
  // Bonus: Can you calculate the size and sum in one go?
  def averageTemperature(samples: ParList[Sample])(implicit executionContext: ExecutionContext): Option[Double] = {
    samples.parFoldMap(s => (s.temperatureFahrenheit, 1))(Monoid.sumAndSize) match {
      case (_, 0)      => None
      case (sum, size) => Some(sum / size)
    }
  }

//  def sumTemperatures(s: ParList[Sample]): (Double, Int) = {
//    s.partitions.foldLeft((0d, 0))((a, p) => (a._1 + p.map(_.temperatureFahrenheit).sum, a._2 + p.size))
//  }

  def sumTemperatures[A](s: ParList[Sample])(implicit executionContext: ExecutionContext): Double = {
    s.parFoldMap(_.temperatureFahrenheit)(Monoid.sumDouble)
  }

  def sumTemperaturesSample[A](s: ParList[Sample]): Double = {
    s.monoFoldLeft(new Monoid[Sample] {
        override val default: Sample = Sample("", "", None, "", 0, 0, 0, 0)

        override def combine(x: Sample, y: Sample): Sample =
          x.copy(temperatureFahrenheit = x.temperatureFahrenheit + y.temperatureFahrenheit)
      })
      .temperatureFahrenheit
  }

  //  def averageTemperature(samples: ParList[Sample]): Option[Double] = {
//    val listAverageF: List[Sample] => Option[Double] = listAverage((s: Sample) => s.temperatureFahrenheit)
//    listAverage((a: Double) => a)(samples.partitions.flatMap(listAverageF))
//  }
//
//  def listAverage[A](t: A => Double)(s: List[A]): Option[Double] = {
//    if (s.isEmpty)
//      None
//    else {
//      val average: Double = s.foldLeft(0d)((a, i) => a + t(i))
//      Some(average / s.size)
//    }
//  }

  // d. Implement `foldLeft` and then move it inside the class `ParList`.
  // `foldLeft` should work as follow:
  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the intermediate results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->    res1 (intermediate result of partition 1) \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->    res2 (intermediate result of partition 2) - finalResult
  // Partition 3:                          Nil -> default (partition 3 is empty)               /
  def foldLeft[From, To](parList: ParList[From], default: To)(combine: (To, From) => To)(combineR: (To, To) => To): To =
    parList.partitions.foldLeft(default)((a, i) => combineR(a, i.foldLeft(default)(combine)))

  // e. Implement `monoFoldLeft`, a version of `foldLeft` that does not change the element type.
  // Then move `monoFoldLeft` inside  the class `ParList`.
  // `monoFoldLeft` should work as follow:
  // Step 1: Fold each partition into a single value.
  // Step 2: Fold the results of all partitions together.
  // For example,
  // Partition 1: List(a1, b1, c1, d1, e1, f1) ->       x   (folded partition 1)  \
  // Partition 2: List(a2, b2, c2, d2, e2, f2) ->       y   (folded partition 2) - z (final result)
  // Partition 3:                          Nil -> default (partition 3 is empty)  /
  def monoFoldLeft[A](parList: ParList[A], default: A)(combine: (A, A) => A): A =
    parList.partitions.foldLeft(default)((a, i) => combine(a, i.foldLeft(default)(combine)))

  // `summaryList` iterate 4 times over `samples`, one for each field.
  def summaryList(samples: List[Sample]): Summary =
    Summary(
      min = samples.minByOption(_.temperatureFahrenheit),
      max = samples.maxByOption(_.temperatureFahrenheit),
      sum = samples.foldLeft(0.0)((state, sample) => state + sample.temperatureFahrenheit),
      size = samples.size
    )

  def summaryListOnePass(samples: List[Sample]): Summary =
    samples.foldLeft(
      Summary(
        min = None,
        max = None,
        sum = 0.0,
        size = 0
      )
    )((state, sample) =>
      Summary(
        min = state.min.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit <= sample.temperatureFahrenheit) Some(current)
          else Some(sample)
        ),
        max = state.max.fold(Some(sample))(current =>
          if (current.temperatureFahrenheit >= sample.temperatureFahrenheit) Some(current)
          else Some(sample)
        ),
        sum = state.sum + sample.temperatureFahrenheit,
        size = state.size + 1
      )
    )

  // Implement `summaryParList` by calling `parFoldMap` once for each field of Summary.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParList`
  // should return the same result as `summaryList`
  def summaryParList(samples: ParList[Sample]): Summary =
    Summary(
      min = samples.parFoldMap(s => Option(s))(Monoid.minSample),
      max = samples.parFoldMap(s => Option(s))(Monoid.maxSample),
      sum = samples.parFoldMap(_.temperatureFahrenheit)(Monoid.sumDouble),
      size = samples.parFoldMap(_ => 1)(Monoid.sumInt)
    )

  // Implement `summaryParListOnePass` using `parFoldMap` only ONCE.
  // Note: In `ParListTest.scala`, there is already a test checking that `summaryParListOnePass`
  // should return the same result as `summaryList`
  def summaryParListOnePass(samples: ParList[Sample]): Summary =
    samples.parFoldMap(s => Summary(Option(s), Option(s), s.temperatureFahrenheit, 1))(Summary.summaryMonoid)

  def summaryReduceMap(samples: ParList[Sample]): Summary =
    Summary.fromSummary(
      samples.reduceMap(s => SummarySemi(s, s, s.temperatureFahrenheit, 1))(SummarySemi.summarySemigroup)
    )

  def summaryParReduceMap(samples: ParList[Sample]): Summary =
    Summary.fromSummary(
      samples.parReduceMap(s => SummarySemi(s, s, s.temperatureFahrenheit, 1))(SummarySemi.summarySemigroup)
    )
}
