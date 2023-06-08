package exercises.errorhandling.project

import exercises.errorhandling.project.OrderError.{EmptyBasket, InvalidStatus}
import exercises.errorhandling.project.OrderGenerator._
import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.time.{Duration, Instant}

class OrderTest extends AnyFunSuite with ScalaCheckDrivenPropertyChecks {

  test("checkout successful example") {
    val order = Order(
      id = "AAA",
      status = "Draft",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    order.checkout match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == "Checkout")
    }
  }

  test("checkout empty basket example") {
    val order = Order(
      id = "AAA",
      status = "Draft",
      basket = Nil,
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.checkout == Left(EmptyBasket))
  }

  test("checkout invalid status example") {
    val order = Order(
      id = "AAA",
      status = "Delivered",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.checkout == Left(InvalidStatus("Delivered")))
  }

  test("submit successful example") {
    val order = Order(
      id = "AAA",
      status = "Checkout",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    order.submit(Instant.now()) match {
      case Left(value)     => fail(s"Expected success but got $value")
      case Right(newOrder) => assert(newOrder.status == "Submit")
    }
  }

  test("submit no address example") {
    val order = Order(
      id = "AAA",
      status = "Checkout",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = None,
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(
      order.submit(Instant.now()) == Left(OrderError.MissingDeliveryAddress)
    ) // replace ??? by the error you created for that scenario
  }

  test("submit invalid status example") {
    val order = Order(
      id = "AAA",
      status = "Delivered",
      basket = List(Item("A1", 2, 12.99)),
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.submit(Instant.now()) == Left(InvalidStatus("Delivered")))
  }

  test("submit empty basket example") {
    val order = Order(
      id = "AAA",
      status = "Checkout",
      basket = Nil,
      deliveryAddress = Some(Address(12, "E16 8TR")),
      createdAt = Instant.now(),
      submittedAt = None,
      deliveredAt = None
    )

    assert(order.submit(Instant.now()) == Left(EmptyBasket))
  }

  test("happy path") {
    forAll(draftGen, Gen.nonEmptyListOf(itemGen), durationGen, addressGen, durationGen) {
      (order: Order, items: List[Item], submitTime: Duration, address: Address, deliverTime: Duration) =>
        val submittedAt = order.createdAt.plus(submitTime)
        val deliveredAt = submittedAt.plus(deliverTime)

        val result = for {
          order <- items.foldLeft(Right[OrderError, Order](order): Either[OrderError, Order])((a, i) =>
            a.flatMap(_.addItem(i))
          )
          order         <- order.checkout
          order         <- order.updateDeliveryAddress(address)
          order         <- order.submit(submittedAt)
          orderDuration <- order.deliver(deliveredAt)
        } yield orderDuration

        assert(
          result.map(_._1) == Right(
            Order(
              id = order.id,
              status = "Deliver",
              basket = items,
              deliveryAddress = Some(address),
              createdAt = order.createdAt,
              submittedAt = Some(submittedAt),
              deliveredAt = Some(deliveredAt)
            )
          )
        )

        assert(result.map(_._2) == Right(deliverTime))
    }
  }

}
