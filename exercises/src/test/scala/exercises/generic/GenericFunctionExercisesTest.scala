package exercises.generic

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import exercises.generic.GenericFunctionExercises._
import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Try

class GenericFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  ////////////////////
  // Exercise 1: Pair
  ////////////////////

  test("Pair swap") {
    forAll(alphaStr, alphaStr) { (one, two) => assert(Pair(one, two).swap == Pair(two, one)) }
  }

  test("Pair map") {
    assert(Pair(0, 1).map(i => i.toString) == Pair("0", "1"))
  }

  test("Pair decoded") {
    s"${decoded.first} ${decoded.second}" == ""
  }

  test("Pair zipWith") {
    assert(Pair(0, 2).zipWith(Pair(3, 4))((x, y) => x + y) == Pair(3, 6))
  }

  test("Pair productNames") {}

  ////////////////////////////
  // Exercise 2: Predicate
  ////////////////////////////

  test("Predicate &&") {
    assert((isEven && isPositive)(12))
    assert(!(isEven && isPositive)(11))
    assert(!(isEven && isPositive)(-4))
    assert(!(isEven && isPositive)(-7))
  }

  test("Predicate && PBT") {
    forAll { (eval1: Int => Boolean, eval2: Int => Boolean, eval3: Int => Boolean, value: Int) =>
      val p1 = Predicate(eval1)
      val p2 = Predicate(eval2)
      val p3 = Predicate(eval3)

      def False[A] = Predicate[A](_ => false)
      def True[A]  = Predicate[A](_ => true)
      assert(!(p1 && False)(value))
      assert((p1 && True)(value) == p1(value))
    }
  }
  test("Predicate ||") {
    assert((isEven || isPositive)(12))
    assert((isEven || isPositive)(11))
    assert((isEven || isPositive)(-4))
    assert(!(isEven || isPositive)(-7))

  }

  test("Predicate flip") {
    assert(isEven.flip(11))
    assert(!isEven.flip(12))
  }

  test("Is valid user") {
    assert(!isValidUser(User("Bob", 11)))
    assert(!isValidUser(User("Bo", 18)))
    assert(!isValidUser(User("bob", 18)))
    assert(isValidUser(User("Bobvy", 18)))
  }

  ////////////////////////////
  // Exercise 3: JsonDecoder
  ////////////////////////////

  test("JsonDecoder UserId") {
    forAll { (idInt: Int) => assert(userIdDecoder.decode(s"$idInt") == UserId(idInt)) }
  }

  test("JsonDecoder LocalDate") {
    assert(localDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020, 3, 26))
    assert(Try(localDateDecoder.decode("2020-03-26")).isFailure)
    assert(Try(localDateDecoder.decode("hello")).isFailure)
  }

  val genLocalDate: Gen[LocalDate]                = Gen.chooseNum(0L, LocalDate.now().toEpochDay).map(LocalDate.ofEpochDay)
  implicit val arbLocalDate: Arbitrary[LocalDate] = Arbitrary(genLocalDate)

  test("JsonDecoder LocalDate Prop") {
    forAll { (testDate: LocalDate) =>
      val testDateStr = testDate.format(DateTimeFormatter.ISO_DATE)
      assert(localDateDecoder.decode("\"" + testDateStr + "\"") == testDate)
    }
  }

  test("JsonDecoder weirdLocalDateDecoder") {
    assert(weirdLocalDateDecoder.decode("\"2020-03-26\"") == LocalDate.of(2020, 3, 26))
    assert(weirdLocalDateDecoder.decode("18347") == LocalDate.of(2020, 3, 26))
    assert(Try(weirdLocalDateDecoder.decode("hello")).isFailure)
  }

}
