package exercises.dataprocessing

import org.scalacheck.{Arbitrary, Gen}

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}
import scala.reflect.ClassTag

trait ParListTestInstances {
  val sampleGen: Gen[Sample] =
    for {
      (region, country, state, city) <- Gen.oneOf(
        ("Africa", "Algeria", None, "Algiers"),
        ("Africa", "Burundi", None, "Bujumbura"),
        ("Asia", "Uzbekistan", None, "Tashkent"),
        ("Asia", "Turkmenistan", None, "Ashabad"),
        ("Europe", "France", None, "Bordeaux"),
        ("Europe", "Germany", None, "Munich"),
        ("North America", "US", Some("Florida"), "Jacksonville"),
        ("North America", "US", Some("California"), "Fresno")
      )
      minDate = LocalDate.of(1975, 1, 1)
      maxDate = LocalDate.of(2020, 1, 1)
      date        <- Gen.choose[Long](minDate.toEpochDay, maxDate.toEpochDay).map(LocalDate.ofEpochDay)
      temperature <- Gen.choose(-50.0f, 150.0f)
    } yield Sample(
      region = region,
      country = country,
      state = state,
      city = city,
      month = date.getMonthValue,
      day = date.getDayOfMonth,
      year = date.getYear,
      temperatureFahrenheit = temperature
    )
  implicit val sampleArb: Arbitrary[Sample] = Arbitrary(sampleGen)

  implicit def parListArb[A](implicit arbA: Arbitrary[A], classTag: ClassTag[A]): Arbitrary[ParList[A]] =
    Arbitrary(
      parListGen
    )

  def parListGen[A: ClassTag](implicit arbA: Arbitrary[A]): Gen[ParList[A]] = {
    implicit val global: ExecutionContextExecutor = ExecutionContext.global
    for {
      l     <- Gen.listOf(arbA.arbitrary)
      pSize <- Gen.choose(1, 10)
    } yield {
      ParList.byPartitionSize(pSize, l)
    }
//For list impl
    //    Gen
//      .listOf(Gen.listOf(arbA.arbitrary))
//      .map(partitions => ParList(partitions)(ExecutionContext.global))
  }

  implicit def parArrayArb[A](implicit arbA: Arbitrary[A], classTag: ClassTag[A]): Arbitrary[ParArray[A]] =
    Arbitrary(
      parArrayGen
    )

  def parArrayGen[A: ClassTag](implicit arbA: Arbitrary[A]): Gen[ParArray[A]] = {
    implicit val global: ExecutionContextExecutor = ExecutionContext.global
    for {
      l     <- Gen.listOf(arbA.arbitrary)
      pSize <- Gen.choose(1, 10)
    } yield {
      ParArray(l.toArray, pSize)
    }
    //For list impl
    //    Gen
    //      .listOf(Gen.listOf(arbA.arbitrary))
    //      .map(partitions => ParList(partitions)(ExecutionContext.global))
  }
  val summaryGen: Gen[Summary] =
    for {
      sample1 <- Arbitrary.arbitrary[Sample]
      sample2 <- Arbitrary.arbitrary[Sample]
      sum     <- Gen.choose(-10000000.0f, 10000000.0f)
      size    <- Gen.choose(0, 1000000)
      samples = List(sample1, sample2)
      min     = samples.minByOption(_.temperatureFahrenheit)
      max     = samples.maxByOption(_.temperatureFahrenheit)
    } yield Summary(min, max, sum, size)

  implicit val summaryArb: Arbitrary[Summary] = Arbitrary(summaryGen)

}
