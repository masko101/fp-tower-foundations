package exercises.action.fp.search

import exercises.action.DateGenerator._
import exercises.action.fp.IO
import exercises.action.fp.search.Airport._
import exercises.action.fp.search.SearchFlightGenerator._
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant, LocalDate}
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global
import scala.util.Random

// Run the test using the green arrow next to class name (if using IntelliJ)
// or run `sbt` in the terminal to open it in shell mode, then type:
// testOnly exercises.action.fp.search.SearchFlightServiceTest
class SearchFlightServiceTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("fromTwoClients example") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight2 = Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3 = Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight3, flight1)))
    val client2 = SearchFlightClient.constant(IO(List(flight2, flight4)))

    val service = SearchFlightService.fromTwoClients(client1, client2)
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight1, flight2, flight3, flight4)))
  }

  test("fromTwoClients should ignore errors from the clients") {
    forAll(airportGen, airportGen, dateGen, clientGen, clientGen) { (from, to, today, client1, client2) =>
      val service = SearchFlightService.fromTwoClients(client1, client2)
      val result  = service.search(from, to, today).attempt(ExecutionContext.global).unsafeRun()

      assert(result.isSuccess)
    }
  }

  test("fromClients example") {
    val now   = Instant.now()
    val today = LocalDate.now()

    val flight1 = Flight("1", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(100), 0, 89.5, "")
    val flight2 = Flight("2", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(105), 0, 96.5, "")
    val flight3 = Flight("3", "BA", parisOrly, londonGatwick, now, Duration.ofMinutes(140), 1, 234.0, "")
    val flight4 = Flight("4", "LH", parisOrly, londonGatwick, now, Duration.ofMinutes(210), 2, 55.5, "")

    val client1 = SearchFlightClient.constant(IO(List(flight3, flight1)))
    val client2 = SearchFlightClient.constant(IO(List(flight2, flight4)))

    val service = SearchFlightService.fromClients(List(client1, client2))
    val result  = service.search(parisOrly, londonGatwick, today).unsafeRun()

    assert(result == SearchResult(List(flight1, flight2, flight3, flight4)))
  }

  test("fromClients should ignore errors from the clients") {
    forAll(airportGen, airportGen, dateGen, Gen.listOf(clientGen)) { (from, to, today, clients) =>
      val service = SearchFlightService.fromClients(clients)
      val result  = service.search(from, to, today).attempt(ExecutionContext.global).unsafeRun()

      assert(result.isSuccess)
    }
  }

  test("fromClients client order doesn't matter") {
    forAll(airportGen, airportGen, dateGen, Gen.listOf(clientGen)) { (from, to, today, clients) =>
      val service1 = SearchFlightService.fromClients(clients)
      val service2 = SearchFlightService.fromClients(Random.shuffle(clients))
      val result1  = service1.search(from, to, today).attempt(ExecutionContext.global).unsafeRun()
      val result2  = service2.search(from, to, today).attempt(ExecutionContext.global).unsafeRun()

      assert(result1 == result2)
    }
  }
}
