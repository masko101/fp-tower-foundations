package exercises.dataprocessing

import exercises.dataprocessing.ForLoopExercises.{min, _}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ForLoopExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("sum") {
    assert(sum(List(1, 5, 2)) == 8)
    assert(sum(Nil) == 0)
  }

  test("sum is consistent with List sum") {
    forAll { (numbers: List[Int]) => assert(sum(numbers) == numbers.sum) }
  }

  test("size") {
    assert(size(List(2, 5, 1, 8)) == 4)
    assert(size(Nil) == 0)
  }

  test("size prop") {
    val intGen = Gen.listOf(Gen.chooseNum(Int.MinValue, Int.MaxValue))
    forAll(intGen) { (l: List[Int]) => assert(size(l) == l.size) }
  }

  test("min") {
    assert(min(List(2, 5, 1, 8)) == Some(1))
    assert(min(Nil) == None)
  }

  test("min prop") {
    val intListGen                                      = Gen.listOf(Gen.posNum[Int])
    implicit val arbitraryIntList: Arbitrary[List[Int]] = Arbitrary(intListGen)
    forAll { (l: List[Int]) => assert(min(l).fold(true)(m => !l.exists(e => e < m))) }
  }

  test("min prop2") {
    val intListGen                                      = Gen.listOf(Gen.chooseNum(Int.MinValue, Int.MaxValue))
    implicit val arbitraryIntList: Arbitrary[List[Int]] = Arbitrary(intListGen)
    forAll { (l: List[Int]) =>
      for {
        m      <- min(l)
        number <- l
      } assert(number >= m)
    }
  }

  test("wordCount") {
    assert(wordCount(List("Hi", "Hello", "Hi")) == Map("Hi" -> 2, "Hello" -> 1))
    assert(wordCount(Nil) == Map.empty)
  }

}
