package exercises.valfunction

import exercises.valfunction.ValueFunctionExercises._
import org.scalacheck.Gen
import org.scalacheck.Gen.{alphaChar, alphaNumChar, negNum, numChar, oneOf, posNum}
import org.scalacheck.Prop.False
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.lang.Math.abs

class ValueFunctionExercisesTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  /////////////////////////////////////////////////////
  // Exercise 1: String API with higher-order functions
  /////////////////////////////////////////////////////

  // replace `ignore` by `test` to enable the test
  test("selectDigits examples") {
    assert(selectDigits("hello4world-80") == "480")
    assert(selectDigits("welcome") == "")
  }

  test("idempotency") {
    forAll { (text: String) => assert(selectDigits(selectDigits(text)) == selectDigits(text)) }
  }

  test("output characters are all digits") {
    forAll { (text: String) =>
      if (selectDigits(text) != "") {
        assert(selectDigits(text).toInt >= 0)
      }
    }
  }

  test("removed characters are not digits") {
    forAll { (text: String) =>
      val setDifference = text.toSet.diff(selectDigits(text).toSet)
      assert(!setDifference.exists(_.isDigit))
    }
  }

  // replace `ignore` by `test` to enable the test
  ignore("selectDigits length is smaller") {
    forAll { (text: String) => assert(selectDigits(text).length <= text.length) }
  }

  test("secret welcome") {
    assert(secret("welcome") == "*******")
  }

  test("secret is idempotent") {
    forAll { (text: String) => assert(secret(text) == secret(secret(text))) }
  }

  test("secret has same length as original(Rebecca)") {
    forAll { (text: String) =>
      assert(text.length == secret(text).length)
      secret(text).foreach(s => assert(s == '*'))
    }
  }

  test("char is valid char (Juliette)") {
    val anChar = Gen.alphaNumChar
    forAll(anChar) { (anChar: Char) =>
//      println(s"--$anChar--")
      assert(isValidUsernameCharacter(anChar))
    }
  }

  test("username is valid char (Kim)") {
//    val anChar = Gen.alphaNumStr
    val validUser =
      Gen.nonEmptyListOf(Gen.frequency((9, alphaNumChar), (1, oneOf(Seq('-', '_'))))).map(_.mkString)
    forAll(validUser) { (username: String) =>
      println(s"--$username--")
      assert(isValidUsername(username))
    }
  }

  ///////////////////////
  // Exercise 2: Point
  ///////////////////////
  test("point is positive if x,y and z are positive") {
    val posAndZero = Gen.frequency((20, posNum[Int]), (1, oneOf(Seq(0))))
    forAll(posAndZero, posAndZero, posAndZero) { (x: Int, y: Int, z: Int) =>
      val point = Point(x, y, z)
      assert(point.isPositive)
    }
  }

  test("point is not positive if x is negative, and y and z are positive") {
    forAll(negNum[Int], posNum[Int], posNum[Int]) { (x: Int, y: Int, z: Int) =>
      val point = Point(x, y, z)
      assert(!point.isPositive)
    }
  }

  test("point is not positive if y is negative, and x and z are positive") {
    forAll(posNum[Int], negNum[Int], posNum[Int]) { (x: Int, y: Int, z: Int) =>
      val point = Point(x, y, z)
      assert(!point.isPositive)
    }
  }

  test("point is not positive if z is negative, and x and y are positive") {
    forAll(posNum[Int], posNum[Int], negNum[Int]) { (x: Int, y: Int, z: Int) =>
      val point = Point(x, y, z)
      assert(!point.isPositive)
    }
  }

  test("point is even if x, y and z are even") {
    forAll { (x: Int, y: Int, z: Int) =>
      val point = Point(getEven(x), getEven(y), getEven(z))
      assert(point.isEven)
    }
  }

  private def getEven(x: Int) = {
    x * 2
  }

  test("point is not even if x is odd, and y and z are even") {
    forAll { (x: Int, y: Int, z: Int) =>
      val point = Point(getOdd(x), getEven(y), getEven(z))
      assert(!point.isEven)
    }
  }

  private def getOdd(x: Int) = {
    getEven(x) - 1
  }

  test("point is not even if y is odd, and x and z are even") {
    forAll(posNum[Int], negNum[Int], posNum[Int]) { (x: Int, y: Int, z: Int) =>
      val point = Point(getEven(x), getOdd(y), getEven(z))
      assert(!point.isEven)
    }
  }

  test("point is not even if z is odd, and x and y are even") {
    forAll(posNum[Int], posNum[Int], negNum[Int]) { (x: Int, y: Int, z: Int) =>
      val point = Point(getEven(x), getEven(y), getOdd(z))
      assert(!point.isEven)
    }
  }
}
