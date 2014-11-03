package test.datastore

import org.scalatest.{ FunSpec, BeforeAndAfter, Matchers }

import org.buttercoin.engine.models.{ Account, Balance }
import org.buttercoin.common.models.core._
import org.buttercoin.common.models.currency._
import org.buttercoin.common.models.money._
import org.buttercoin.common.models.money.CurrencyImplicits._

class AccountSpec extends FunSpec with Matchers with BeforeAndAfter {
  var account: Account = _
  val bacct: Account = new Account(AccountID(java.util.UUID.randomUUID))
  var balance: Balance[USD] = null

  before {
    account = new Account(AccountID(java.util.UUID.randomUUID))
    balance = new Balance[USD](usd("0"))
  }

  describe("Account") {

    it("should have balances for USD and BTC") {
      account balance("USD") should be ( Some(usd("0")) )
      account balance("BTC") should be ( Some(btc("0")) )
    }

    describe("Balance") {
      it("should be able to credit a balance") {
        balance credit 1 should be ( usd("1") )
        balance.current should be ( usd("1") )
      }

      it("should be able to debit a balance") {
        balance credit 3
        balance debit 1 should be ( Some(usd("2")) )
        balance.current should be ( usd("2") )
      }
      it("should not be able to debit a balance if it would become negative") {
        balance debit 1 should be ( None )
        balance debit 0 should be ( Some(usd("0")) )
      }
    }
  }
}
