package org.buttercoin.common.testhelper

import org.scalacheck.Arbitrary
import org.scalacheck.Gen.{ uuid => GenUUID }
import org.scalacheck.Arbitrary.arbitrary

import akka.actor._
import akka.testkit.TestActorRef
import java.util.UUID
import org.buttercoin.common.models.core._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._
import org.scalatest.BeforeAndAfterEach
import scala.language.implicitConversions

object Gen {
  def arbCurrency[T <: Currency : CurrencyFactory] = Arbitrary {
    for {
      v <- arbitrary[Double]
    } yield implicitly[CurrencyFactory[T]].apply(math.abs(v % 1e12))
  }

  implicit val arbUSD = arbCurrency[USD]
  implicit val arbBTC = arbCurrency[BTC]

  implicit val arbUUID = Arbitrary { GenUUID }
  implicit val arbAccountID = Arbitrary { arbitrary[UUID].map(AccountID) }

  val ratioGen = for {
    x <- arbitrary[Double] map Math.abs
    y <- arbitrary[Double] map Math.abs
    if x != y && x > 1 && y > 1
  } yield Math.min(x, y) / Math.max(x, y)

  def nonEmptyString = arbitrary[String] suchThat (!_.isEmpty)

  def positiveInt = arbitrary[Int] map Math.abs
  def positiveLong = arbitrary[Long] map Math.abs
  def positiveBigDecimal = arbitrary[Int] map { x => BigDecimal(Math.abs(x)) }
  def pos[T <: Currency : CurrencyFactory : Arbitrary] = arbitrary[T] suchThat (_.amount > 0)
}

trait CallForwarding { def !>(message: Any): Unit }

case class Receive(test: PartialFunction[Any, Boolean]) {
  var forward: Option[Any] = None
  def forwards(value: Any): Receive = {
    forward = Some(value)
    this
  }
}

case class Watch(other: ActorRef)
class ExpectationActor extends Actor {
  import akka.event.Logging
  import org.buttercoin.common.testhelper
  val log = Logging(context.system, this)

  def receive = {
    case x: testhelper.Receive => expect(x)
    case Watch(x) => context.watch(x)
    case x => handleMessage(x)
  }

  var expectations = Set[testhelper.Receive]()
  var errors = List[String]()

  def expect(test: testhelper.Receive) = {
    expectations = expectations + test
  }

  def isSatisfied: Boolean = {
    if (expectations.size > 0) {
      log.error(expectations.size + " unmet expectations")
    }
    errors.foreach(log.error(_))
    expectations.size == 0 && errors.size == 0
  }

  def handleMessage(msg: Any) = {
    val cases = expectations filter {(x: testhelper.Receive) => x.test.isDefinedAt(msg) && x.test(msg)}
    if (cases.size > 0) { 
      expectations = expectations - cases.head
      cases.head.forward match {
        case Some(f) => sender ! f
        case None =>
      }
    } else {
      val error = ("Unexpected message: " + msg)
      println(error)
      errors = error :: errors
    }
  }
}

trait ActorSystemRestarter extends BeforeAndAfterEach {
  self: org.scalatest.Suite =>
  implicit val timeout = akka.util.Timeout(1000)
  implicit var system: ActorSystem = _ 
  override def beforeEach() = {
    system = ActorSystem("testSystem")//, ConfigFactory.load.getConfig("test"))
    super.beforeEach()
  }

  override def afterEach() = {
    try { super.afterEach() }
    finally {
      system.shutdown()
      system = null
    }
  }
}

trait ExpectActor extends ActorSystemRestarter {
  self: org.scalatest.Suite =>
  implicit var callForwarding: CallForwarding = _
  var expect: TestActorRef[ExpectationActor] = _ 
  implicit def forwardCaller(a: ActorRef): CallForwarding =
    new CallForwarding { def !>(msg: Any) = a.tell(msg, expect) }

  implicit def toActorObject[T <: Actor](a: TestActorRef[T]) : T = a.underlyingActor

  override def beforeEach() = {
    super.beforeEach()
    expect = TestActorRef(new ExpectationActor, "expectationActor")
    setUpEach.map(_())
  }

  override def afterEach() = {
    teardownEach.map(_())
    super.afterEach()
  }

  def validateExpected = {
    Thread.sleep(100) // Hack to let any futures resolve
    assert(expect.underlyingActor.isSatisfied, "Actor expectations violated")
  }

  private var setUpEach: Option[() => Unit] = None
  private var teardownEach: Option[() => Unit] = None
  def setUp(f: => Unit) = setUpEach = Some(() => f)
  def teardown(f: => Unit) = teardownEach = Some(() => f)
}
