package exercises.dataprocessing

import exercises.dataprocessing.ThreadPoolUtil.fixedSizeExecutionContext
import exercises.dataprocessing.TimeUtil.{bench, Labelled}
import kantan.csv._
import kantan.csv.ops._

import scala.concurrent.ExecutionContext

// Run the notebook using green arrow (if available in your IDE)
// or run `sbt` in your terminal to open sbt in shell mode then type:
// exercises/runMain exercises.dataprocessing.TemperatureNotebook
object TemperatureNotebook extends App {

//  private val executorService: ExecutorService = Executors.newFixedThreadPool(8)
//  implicit val executionContext                = ExecutionContext.fromExecutor(executorService)
  implicit val executionContext: ExecutionContext = fixedSizeExecutionContext(8);
  // !!!!  IMPORTANT !!!!
  // Download the dataset from https://www.dropbox.com/s/4pf6h2oxw4u7xsq/city_temperature.csv?dl=0
  // and place the csv file in the resource directory (exercises/src/main/resources)

  // We use kantan.csv library to parse the each csv raw into a case class `Sample`
  // such as 1 column in the csv maps to one field in the case class.
  // See https://nrinaudo.github.io/kantan.csv/rows_as_case_classes.html
  val reader: CsvReader[Either[ReadError, Sample]] = getClass
    .getResource("/city_temperature.csv")
    .asCsvReader[Sample](rfc.withHeader)

  val rows: List[Either[ReadError, Sample]] = reader.toList

  val failures: List[ReadError] = rows.collect { case Left(error)   => error }
  val samples: List[Sample]     = rows.collect { case Right(sample) => sample }

  // we can also extract failures and successes in one go using `partitionMap`
  // val (failures, successes) = rows.partitionMap(identity)

  println(s"Parsed ${samples.size} rows successfully and ${failures.size} rows failed ")

  // a. Implement `samples`, a `ParList` containing all the `Samples` in `successes`.
  // Partition `parSamples` so that it contains 10 partitions of roughly equal size.
  // Note: Check `ParList` companion object
  val partitionSize: Int               = math.ceil(samples.size.toDouble / 10).toInt
  lazy val parSamples: ParList[Sample] = ParList.byPartitionSize(partitionSize, samples)

  parSamples.partitions.zipWithIndex.foreach { case (p, i) => println(s"Partition $i has size ${p.size}") }

  // b. Implement `minSampleByTemperature` in TemperatureExercises
  lazy val coldestSample: Option[Sample] =
    TemperatureExercises.minSampleByTemperature(parSamples)
  println(s"coldest is $coldestSample")

  // c. Implement `averageTemperature` in TemperatureExercises
  lazy val averageTemperature: Option[Double] =
    TemperatureExercises.averageTemperature(parSamples)

  println(s"The average is $averageTemperature")

  private val (sum, size) =
    parSamples.parFoldMap(s => (s.temperatureFahrenheit, 1))(Monoid.sumAndSize)
  println(s"The average is ${sum / size} - $sum - $size")

  //////////////////////
  // Benchmark ParList
  //////////////////////

  // Compare the runtime performance of various implementations of `sum`:
  // * List foldLeft
  // * List map + sum
  // * TODO ParList foldMap
  // * TODO ParList parFoldMap
//  bench("sum", iterations = 200, warmUpIterations = 40)(
//    Labelled("List foldLeft", () => samples.foldLeft(0.0)((state, sample) => state + sample.temperatureFahrenheit)),
//    Labelled("List map + sum", () => samples.map(_.temperatureFahrenheit).sum),
//    Labelled("ParList foldMap", () => parSamples.foldMap(_.temperatureFahrenheit)(Monoid.sumDouble)),
//    Labelled("ParList parFoldMap", () => parSamples.parFoldMap(_.temperatureFahrenheit)(Monoid.sumDouble))
//  )

  // Compare the runtime performance of various implementations of `summary`
  // * List with 4 iterations
  // * List with 1 iterations
  // * TODO ParList with 4 iterations
  // * TODO ParList with 1 iteration
  bench("summary", iterations = 150, warmUpIterations = 20)(
    Labelled("List 4 iterations", () => TemperatureExercises.summaryList(samples)),
    Labelled("List 1 iteration", () => TemperatureExercises.summaryListOnePass(samples)),
    Labelled("ParList 4 iterations", () => TemperatureExercises.summaryParList(parSamples)),
    Labelled("ParList 1 iteration", () => TemperatureExercises.summaryParListOnePass(parSamples)),
    Labelled("ParList reduceMap 1 iterations", () => TemperatureExercises.summaryReduceMap(parSamples)),
    Labelled("ParList parReduceMap iteration", () => TemperatureExercises.summaryParReduceMap(parSamples))
  )

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // Generalise Monoid sum to accept all types of number (Hint: check `Numeric`, e.g. Numeric[Int], `Numeric[Double]`)

  // Generalise Monoid minBy/maxBy from a hard-coded `Sample => Double` to a generic `From => To`
  // Is it possible to write such a Monoid for any type `From` and `To` or do you need additional constraints?

  // What would happen if we aggregate the dataset by city and country and Mexico is part of the dataset?
  // Update `aggregateByLabel` to avoid this problem.

  // Calculate the n smallest/largest value, e.g. what are the 5 coldest samples in the dataset?

  // Implement a shuffle method on `ParList` so that it is unlikely all the expensive
  // tasks are in the same partition. Will this affect the result of `parFoldMap`?

  // Define a random generator of `Monoid[Int]` (i.e. `Gen[Monoid[Int]]`) and use it in PBTs.
  // (Hint: You can use `Gen.oneOf`)

  //////////////////////////////////////////////
  // Ideas to improve `ParList` performance
  //////////////////////////////////////////////

  // 1. When we defined `Summary`, we made `min` and `max` an `Option` because the `ParList`
  //   can be empty. However, it is quite expensive because we wrap and unwrap an `Option` for
  //   every value in the dataset. Instead we could check if the `ParList` is empty at the beginning,
  //   if it is we return None, otherwise we can `reduce` the `ParList` without `Option`.
  //   See `reduceFoldLeftOption` on `List`.
  //
  //   Could you implement `reduceMap` on `ParList`?
  //   def reduceMap[To](zoom: A => To)(combine: (To, To) => To): Option[To]
  //   or
  //   def reduceMap[To](zoom: A => To)(semigroup: Semigroup[To]): Option[To]
  //   where `Semigroup` is like a `Monoid` but without `default` value.

  // 2. use Array instead of List as underlying data structure for better caching.
  //   Furthermore, we only need to store a partition as a pair of index:
  //   partition 1 from 0      to 10 000
  //   partition 2 from 10 001 to 25 000

  // 3. `parFoldMap` need to wait for ALL intermediate results to be ready before starting
  //    to fold them. Instead, could we fold the intermediate results as soon as they
  //    are available? Will we always get the same results this way?
  //    (Hint: You would need to make the state thread-safe)

}
