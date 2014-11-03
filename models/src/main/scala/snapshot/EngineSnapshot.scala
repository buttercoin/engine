package org.buttercoin.engine.models.snapshot

import org.buttercoin.common.fees.FeeStrategy
import org.buttercoin.common.models.core._
import org.buttercoin.common.models.order.OrderID
import org.buttercoin.common.models.orderInfo.OrderInfoHistory
import org.buttercoin.engine.models.Account

import scala.collection.{ immutable => I }
import scala.concurrent.stm._
import scala.concurrent.{ promise, Future }
import scala.concurrent.ExecutionContext.Implicits.global

@SerialVersionUID(1L)
case class LedgerSnapshot(
  accounts: I.Seq[(AccountID, (Account, FeeStrategy))]
)

case class AccountSnapshotOffer(
  accountId: AccountID,
  account: Account,
  feeStrategy: FeeStrategy)

@SerialVersionUID(1L)
case class MarketSnapshot(
  priceCode: String,
  quantityCode: String,
  market: Any
)

@SerialVersionUID(1L)
case class OrderStoreSnapshot(
  ordersById: I.Seq[(OrderID, OrderInfoHistory)],
  ordersByAcct: I.Seq[(AccountID, I.Set[OrderID])]
)

/**
 * The serialized form of all snapshotted Engine elements
 */
@SerialVersionUID(1L)
case class EngineSnapshot(
  ledgers: I.List[LedgerSnapshot],
  markets: I.List[MarketSnapshot],
  orderStore: OrderStoreSnapshot
)

case class EngineSnapshotOffer(snap: EngineSnapshot)

/**
 * A request for a snapshot of the entire Engine system with synchronization
 * support.
 */
case class EngineSnapshotRequest() {
  val ledgers: Ref[I.List[LedgerSnapshot]] = Ref(Nil)
  val markets: Ref[I.List[MarketSnapshot]] = Ref(Nil)
  val orderStore: Ref[Option[OrderStoreSnapshot]] = Ref(None)

  /**
   * A future which completes once all elements of the snapshot have been registered.
   * The complete EngineSnapshot is provided when the future resolves.
   *
   * It expects 4 ledger actors and 2 markets
   */
  val future = Future[EngineSnapshot] {
    atomic { implicit txn =>
      if(ledgers().length < 4  ||
         markets().length < 2  ||
         orderStore().isEmpty) { retry }
    }

    this.asSnapshot
  }

  private def asSnapshot: EngineSnapshot = {
    EngineSnapshot(
      ledgers.single.get,
      markets.single.get,
      orderStore.single.get.get
    )
  }
}
