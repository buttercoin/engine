package test.messages

import org.scalatest.{ FunSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import akka.actor.ActorRef
import java.util.UUID
import org.buttercoin.common.messages._
import org.buttercoin.common.models.core._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.order._
import org.buttercoin.common.testhelper.MessageGen._

class MessagesSpec extends FunSpec with Matchers with GeneratorDrivenPropertyChecks {

  describe("InterprocessMessages") {
    forAll { (d: MessageRequest) =>
      d.isInstanceOf[RoutedMessage] should be(true)
      d.message.isInstanceOf[Request] should be(true)
      MessageRequest(d.message, d.id) should be(d)
      d.copy(id = Some(UUID.randomUUID)) should not be(d)
    }

    forAll { (d: MessageSuccess) =>
      d.isInstanceOf[MessageResponse] should be(true)
      d.result.isInstanceOf[Result] should be(true)
      MessageSuccess(d.result, d.id) should be(d)
      d.copy(id = UUID.randomUUID) should not be(d)
    }

    forAll { (d: MessageFailure) =>
      d.isInstanceOf[MessageResponse] should be(true)
      MessageFailure(d.errors, d.id) should be(d)
      d.copy(id = UUID.randomUUID) should not be(d)
    }

    forAll { (d: ResultSeq) =>
      d.isInstanceOf[Result] should be(true)
      ResultSeq(d.seq) should be(d)
      d.copy(seq = d.seq.tail) should not be(d)
    }

    forAll { (d: BroadcastMessage) =>
      d.isInstanceOf[RoutedResult] should be(true)
      d.isInstanceOf[RoutedMessage] should be(true)
      d.message.isInstanceOf[Result] should be(true)
      BroadcastMessage(d.message) should be(d)
    }

    forAll { (d: UserMessage[Result]) =>
      d.isInstanceOf[RoutedResult] should be(true)
      d.isInstanceOf[RoutedMessage] should be(true)
      d.message.isInstanceOf[Result] should be(true)
      UserMessage(d.message, d.acctId) should be(d)
      d.copy(acctId = AccountID(UUID.randomUUID)) should not be(d)
    }
  }

  describe("ChannelRegistrationMessages") {
    val register = RegisterForBroadcast(ActorRef.noSender)
    RegisterForBroadcast(register.actor) should be(register)

    val unregister = UnregisterForBroadcast(ActorRef.noSender)
    UnregisterForBroadcast(unregister.actor) should be(unregister)

    val rChannel = RegisterForChannel(ActorRef.noSender, "test")
    RegisterForChannel(rChannel.actor, rChannel.channelName) should be(rChannel)

    val uChannel = UnregisterForChannel(ActorRef.noSender, "test")
    UnregisterForChannel(uChannel.actor, uChannel.channelName) should be(uChannel)
  }

  describe("ExchangeSuccessErrorMessages") {
    val successResult = ExchangeOperationSuccessResult()
    successResult.isInstanceOf[Message] should be(true)
    successResult.isInstanceOf[Result] should be(true)
    ExchangeOperationSuccessResult() should be(successResult)

    val authSuccessResult = AuthenticationSuccess()
    authSuccessResult.isInstanceOf[Result] should be(true)
    AuthenticationSuccess() should be(authSuccessResult)

    forAll { (d: OperationRequestError) =>
      d.isInstanceOf[Result] should be(true)
      d.isInstanceOf[RequestError] should be(true)
      OperationRequestError(d.message) should be(d)
      d.copy(message = d.message + "-" + d.message) should not be(d)
    }

    forAll { (d: BackendOperationRequestError) =>
      d.isInstanceOf[Result] should be(true)
      d.isInstanceOf[RequestError] should be(true)
      BackendOperationRequestError(d.message) should be(d)
      d.copy(message = d.message + "-" + d.message) should not be(d)
    }

    forAll { (d: FieldOperationRequestError) =>
      d.isInstanceOf[Result] should be(true)
      d.isInstanceOf[RequestError] should be(true)
      FieldOperationRequestError(d.fieldName, d.messages) should be(d)
      d.copy(fieldName = d.fieldName + "-" + d.fieldName) should not be(d)
    }
  }

  describe("CoreExchangeMessage") {
    forAll { (d: GetTicker) =>
      d.isInstanceOf[Query] should be(true)
      d.isInstanceOf[MarketQuery] should be(true)
      GetTicker(d.left, d.right) should be(d)
      d.copy(left = d.left + "-" + d.left) should not be(d)
    }

    forAll { (d: Ticker) =>
      d.isInstanceOf[Result] should be(true)
      d.isInstanceOf[QueryResult] should be(true)
      Ticker(d.bid, d.ask, d.last) should be(d)
      d.copy(bid = btc(BigDecimal("1"))) should not be(d)
    }

    val operationFailure = OperationFailure()
    operationFailure.isInstanceOf[Throwable] should be(true)
    operationFailure.isInstanceOf[OperationResult] should be(true)
    OperationFailure() should be(operationFailure)

    forAll { (d: GetBalances) =>
      d.isInstanceOf[AccountQuery] should be(true)
      GetBalances(d.accountId) should be(d)
      d.copy(accountId = AccountID(UUID.randomUUID)) should not be(d)
    }

    forAll { (d: Balances) =>
      d.isInstanceOf[Result] should be(true)
      d.isInstanceOf[QueryResult] should be(true)
      Balances(d.balances) should be(d)
      d.copy(balances = d.balances.tail) should not be(d)
    }

    forAll { (d: BalanceChanged) =>
      d.isInstanceOf[Result] should be(true)
      d.isInstanceOf[OperationResult] should be(true)
      BalanceChanged(d.accountId, d.newBalance) should be(d)
      d.copy(accountId = AccountID(UUID.randomUUID)) should not be(d)
    }
  }

  describe("MarketDepthInfoMessages") {
    it("should have requests and responses for currency market state") {
      forAll { (d: CreditTrade[USD]) =>
        d.isInstanceOf[AccountOperation] should be(true)
        d.isInstanceOf[AssociatedAccount] should be(true)
        d.amount.isInstanceOf[USD] should be(true)
        CreditTrade(d.accountId, d.orderId, d.amount, d.completedAs) should be(d)
        d.copy(accountId = AccountID(UUID.randomUUID)) should not be(d)
      }

      forAll { (d: GetFeeHistory) =>
        d.isInstanceOf[AccountQuery] should be(true)
        GetFeeHistory(d.accountId) should be(d)
        d.copy(accountId = AccountID(UUID.randomUUID)) should not be(d)
      }

      forAll { (f: FeeAssessed) =>
        f.isInstanceOf[Result] should be(true)
        f.noFeeAssessed() should be(f.feeAmount.amount == BigDecimal("0"))
        FeeAssessed(f.accountId, f.orderId, f.fundedAmount, f.feeRate, f.feeAmount, f.tradeType) should be(f)
        f.copy(accountId = AccountID(UUID.randomUUID)) should not be(f)
      }

      forAll { (d: DepthChange) =>
        d.parity.isInstanceOf[Parity] should be(true)
        DepthChange(d.parity, d.price, d.quantity) should be(d)
        d.copy(parity = d.parity match { case Ask => Bid; case Bid => Ask }) should not be(d)
      }

      forAll { (d: MarketDepth) =>
        d.isInstanceOf[Result] should be(true)
        MarketDepth(d.parity, d.price, d.quantity) should be(d)
        d.copy(parity = d.parity match { case Ask => Bid; case Bid => Ask }) should not be(d)
      }

      forAll { (d: GetMarketDepth) =>
        d.isInstanceOf[Query] should be(true)
        GetMarketDepth(d.priceCode, d.quantityCode) should be(d)
        d.copy(priceCode = d.priceCode + "_price") should not be(d)
      }
    }
  }

  describe("StoreQueryMessages") {
    GetTotalOpenOrders.isInstanceOf[Request] should be(true)

    forAll { (d: TotalOpenOrders) =>
      d.isInstanceOf[Result] should be(true)
      TotalOpenOrders(d.balances) should be(d)
      d.copy(balances = List.empty) should not be(d)
    }

    forAll { (d: GetTotalLedgerBalances) =>
      d.isInstanceOf[Request] should be(true)
      GetTotalLedgerBalances(d.accountsBlacklist) should be(d)
      d.copy(accountsBlacklist = List.empty) should not be(d)
    }

    forAll { (d: TotalLedgerBalances) =>
      d.isInstanceOf[Result] should be(true)
      TotalLedgerBalances(d.balances) should be(d)
      d.copy(balances = List.empty) should not be(d)
    }

    forAll { (d: CancelOrder) =>
      d.isInstanceOf[AccountOperation] should be(true)
      CancelOrder(d.accountId, d.orderId) should be(d)
      d.copy(accountId = AccountID(UUID.randomUUID)) should not be(d)
    }

    forAll { (d: GetOrderHistory) =>
      d.isInstanceOf[AccountQuery] should be(true)
      d.isInstanceOf[InternalQuery] should be(true)
      GetOrderHistory(d.accountId, d.skip, d.limit) should be(d)
      d.copy(accountId = AccountID(UUID.randomUUID)) should not be(d)
    }

    forAll { (d: InternalOrderQuery) =>
      d.isInstanceOf[InternalQuery] should be(true)
      InternalOrderQuery(d.accountId, d.skip, d.limit, d.dateMin, d.dateMax, d.orderId, d.status, d.parity, d.orderType, d.queryType) should be(d)
      d.copy(accountId = Some(AccountID(UUID.randomUUID))) should not be(d)
    }

    forAll { (d: GetOpenOrders) =>
      d.isInstanceOf[AccountQuery] should be(true)
      d.isInstanceOf[InternalQuery] should be(true)
      GetOpenOrders(d.accountId, d.skip, d.limit) should be(d)
      d.copy(accountId = AccountID(UUID.randomUUID)) should not be(d)
    }
  }
}
