package exercises.dataprocessing

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import TemperatureExercises._
import org.scalacheck.{Arbitrary, Gen}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class ParListTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks with ParListTestInstances {

  implicit val ec: ExecutionContext = ThreadPoolUtil.fixedSizeExecutionContext(8)
  test("minSampleByTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(
      minSampleByTemperature(parSamples) ==
        Some(Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1))
    )
  }

  test("minSampleByTemperature returns the coldest Sample") {
    forAll { (samples: List[Sample]) =>
      val parSamples = ParList.byPartitionSize(3, samples)

      for {
        coldest <- minSampleByTemperature(parSamples)
        sample  <- samples
      } assert(coldest.temperatureFahrenheit <= sample.temperatureFahrenheit)
    }
  }

  test("minSampleByTemperature returns the same sample on the partition as on the whole list") {
    forAll { (samples: List[Sample]) =>
      val parSamples            = ParList.byPartitionSize(3, samples)
      val coldestFromPartOption = minSampleByTemperature(parSamples)
      val coldestOption         = minSampleByTemperature(ParList(parSamples.partitions.flatten))
      assert(coldestFromPartOption == coldestOption)
    }
  }

  test("averageTemperature example") {
    val samples = List(
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 50),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 56.3),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 23.4),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 89.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 22.1),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 34.7),
      Sample("Africa", "Algeria", None, "Algiers", 8, 1, 2020, 99.0)
    )
    val parSamples = ParList.byPartitionSize(3, samples)

    assert(averageTemperature(parSamples) == Some(53.6))
  }

  test("averageTemperature always >= min and <= max") {
    forAll { (samples: List[Sample]) =>
      val parSamples    = ParList.byPartitionSize(3, samples)
      val average       = averageTemperature(parSamples)
      val coldestOption = minSampleByTemperature(ParList(parSamples.partitions.flatten)).map(_.temperatureFahrenheit)
      val hottestOption = maxSampleByTemperature(ParList(parSamples.partitions.flatten)).map(_.temperatureFahrenheit)
      assert(
        (average.isEmpty && coldestOption.isEmpty && hottestOption.isEmpty) ||
          (average.getOrElse(0d) >= coldestOption.getOrElse(0d) && average.getOrElse(0d) <= hottestOption.getOrElse(0d))
      )
    }
  }

//  Gen.function1()
  test("monoFoldLeft sum") {
    forAll { (nums: ParList[Int]) => assert(nums.monoFoldLeft(Monoid.sumInt) == nums.partitions.map(_.sum).sum) }
  }

  test("foldMap is consistent with map followed by monoFoldLeft") {
    forAll { (nums: ParList[Int], update: Int => Double) =>
      assert(nums.foldMap(update)(Monoid.sumDouble) == nums.map(update).monoFoldLeft(Monoid.sumDouble))
    }
  }

  test("foldMap(identity) is consistent with map followed by monoFoldLeft(identity)") {
    forAll { (nums: ParList[Int]) =>
      assert(nums.foldMap(identity)(Monoid.sumInt) == nums.map(identity).monoFoldLeft(Monoid.sumInt))
    }
  }

  test("parFoldMap is consistent with map followed by foldMap") {
    forAll { (nums: ParList[Int], update: Int => Double) =>
      assert(nums.parFoldMap(update)(Monoid.sumDouble) == nums.foldMap(update)(Monoid.sumDouble))
    }
  }

  test("parFoldMap(identity) is consistent with map followed by monoFoldLeft(identity)") {
    forAll { (nums: ParList[Int]) =>
      assert(nums.parFoldMap(identity)(Monoid.sumInt) == nums.foldMap(identity)(Monoid.sumInt))
    }
  }

  //This liimits the size and number of decimal places.
  val genInt                 = Gen.chooseNum(Int.MinValue, Int.MaxValue)
  val doubleGen: Gen[Double] = Gen.choose(-100f, 100f).map(_.toDouble)
//  val sumAndSizeGen: Gen[(Double, Int)] = Gen.choose(-100f, 100f).map(d => (d.toDouble, 1))
  val doubleIntGen: Gen[(Double, Int)] = Gen.zip(Gen.chooseNum(-100f, 100f).map(_.toDouble), Gen.oneOf(Seq(1)))

  checkMonoid("Int", Monoid.sumInt, genInt)
  checkMonoid("Double", Monoid.sumDouble, doubleGen)
  checkMonoid("zip", Monoid.zip(Monoid.sumDouble, Monoid.sumInt), Gen.zip(doubleGen, genInt))

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 1000)

  def checkMonoid[A: Arbitrary](name: String, monoFoldParam: Monoid[A], gen: Gen[A]) = {
    test(s"Monoid $name combine = no-op for default") {
      forAll(gen) { (num: A) =>
        assert(monoFoldParam.combine(num, monoFoldParam.default) == num)
        assert(monoFoldParam.combine(monoFoldParam.default, num) == num)
      }
    }
    test(s"Monoid $name is associative") {
      forAll(gen, gen, gen) { (num: A, num2: A, num3: A) =>
        val comb1 = monoFoldParam.combine(monoFoldParam.combine(num, num2), num3)
        val comb2 = monoFoldParam.combine(num, monoFoldParam.combine(num2, num3))
        assert(comb1 == comb2)
      }
    }
  }

  test("MonoFoldLeft is associative  compare to fold") {
    forAll(Gen.listOf[Int](genInt), Gen.chooseNum(1, Int.MaxValue), Gen.chooseNum(1, Int.MaxValue)) {
      (nums: List[Int], split1: Int, split2: Int) =>
        val par1 =
          ParList.byPartitionSize(math.ceil(nums.size / math.max(1, (split1 % math.max(1, nums.size)))).toInt, nums)
        val par2 =
          ParList.byPartitionSize(math.ceil(nums.size / math.max(1, (split2 % math.max(1, nums.size)))).toInt, nums)
        assert(par1.monoFoldLeft(Monoid.sumInt) == par2.monoFoldLeft(Monoid.sumInt))
    }
  }

  // Will never work except for 0 because the default will be applied once per partiton not once per list.
//  test("monoFoldLeft compare to fold") {
//    forAll { (nums: ParList[Int], default: Int) =>
////      , combine: (Int, Int) => Int
//      assert(nums.monoFoldLeft(default)(_ + _) == nums.toList.foldLeft(default)(_ + _))
//    }
//  }

  test("summary is consistent between implementations") {
    forAll { (samples: ParList[Sample]) =>
      val samplesList = samples.partitions.flatten
      val reference   = summaryList(samples.partitions.flatten)
      List(
        summaryListOnePass(samplesList),
        summaryParList(samples)
//        summaryParListOnePass(samples)
      ).foreach { other =>
        assert(reference.size == other.size)
        assert((reference.sum - other.sum).abs < 0.00001)
        assert(reference.min == other.min)
        assert(reference.max == other.max)
      }
    }
  }
}
