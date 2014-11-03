package test.actors

import org.scalatest.{ FunSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalamock.scalatest.MockFactory
import akka.actor._
import akka.testkit.{ TestActorRef, TestKitBase}
import scala.concurrent.duration._

import scala.language.postfixOps
import scala.language.implicitConversions
import org.buttercoin.engine.actors._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money.CurrencyImplicits._
import org.buttercoin.common.actor.SetUpstream

import scalaz._
import Scalaz._

class ExecutorActorSpec extends FunSpec
  with Matchers
  with TestKitBase
  with MockFactory
  with GeneratorDrivenPropertyChecks
{
  implicit def self = testActor
  implicit lazy val system = ActorSystem("executorActorSpec")
  implicit def testActorRefObject[T <: Actor](testRef: TestActorRef[T]): T = testRef.underlyingActor

  def initExec = {
      val executor = TestActorRef(new ExecutorActor)

      executor ! AddEngine(self)
      executor ! SetUpstream(self)
      executor ! 'Ready

      executor
  }

  describe("ExecutorActor") {
    it("should be able to add an engine before becoming ready") {
      val executor = TestActorRef(new ExecutorActor)

      executor ! AddEngine(self)

      within(200 milliseconds) {
        executor.engineSet.size should be ( 1 )
        executor.engineSet.head should be ( self )
      }
    }
  }
}
