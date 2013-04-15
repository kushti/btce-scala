package org.chepurnoy.btce.test

import org.specs2.mutable._
import org.chepurnoy.btce._
import scala.Some

trait FakeHttpApiClient extends HttpApiClient{
  protected def getRequest(url: String): String = url match {
    case s:String if (s.contains("fee")) => "{\"trade\":0.2}"
    case s:String if (s.contains("ticker")) => "{\"ticker\":{\"high\":259,\"low\":100,\"avg\":179.5,\"vol\":8845988.11163,\"vol_cur\":51665.3736,\"last\":171,\"buy\":170.99,\"sell\":170.05,\"server_time\":1365659114}}"
    case s:String if (s.contains("trades")) => "[{\"date\":1365659102,\"price\":171,\"amount\":0.529173,\"tid\":1794926,\"price_currency\":\"USD\",\"item\":\"BTC\",\"trade_type\":\"ask\"},{\"date\":1365659093,\"price\":170.99,\"amount\":0.028974,\"tid\":1794917,\"price_currency\":\"USD\",\"item\":\"BTC\",\"trade_type\":\"bid\"}]"
    case s:String if (s.contains("depth")) => "{\"asks\":[[170.99,8.47082719],[171.873,0.013]], \"bids\":[[170.05,4.71834685],[170.02,3.58278578]]}"
    case _ => ""
  }

  protected def signedPostRequest(url: String, key: String, Secret: String, postBody: String): String = {
    postBody match {
      case s:String if (s.contains("method=getInfo")) => "{\"success\":1,\"return\":{\"funds\":{\"usd\":0.00000047,\"rur\":0.00000238,\"eur\":0,\"btc\":0.00000001,\"ltc\":1439.7515316,\"nmc\":0,\"nvc\":0,\"trc\":0,\"ppc\":0},\"rights\":{\"info\":1,\"trade\":1,\"withdraw\":0},\"transaction_count\":1921,\"open_orders\":0,\"server_time\":1365779613}}"
      case s:String if (s.contains("method=TransHistory")) => "{\"success\":1,\"return\":{\"18586103\":{\"type\":4,\"amount\":463.62480000,\"currency\":\"LTC\",\"desc\":\"Cancel order :order:6918639:\",\"status\":2,\"timestamp\":1365745808},\"18586085\":{\"type\":4,\"amount\":400.00000000,\"currency\":\"LTC\",\"desc\":\"Cancel order :order:6917762:\",\"status\":2,\"timestamp\":1365745805},\"18586019\":{\"type\":4,\"amount\":76.00000000,\"currency\":\"LTC\",\"desc\":\"Cancel order :order:6720577:\",\"status\":2,\"timestamp\":1365745792},\"17256829\":{\"type\":4,\"amount\":62.11409917,\"currency\":\"LTC\",\"desc\":\"Buy 62.23857632 LTC (-0.2%) from your order :order:7040565: by price 4.703 USD\",\"status\":2,\"timestamp\":1365596676}}}"
      case s:String if (s.contains("method=OrderList")) => "{\"success\":1,\"return\":{\"7615740\":{\"pair\":\"ltc_usd\",\"type\":\"sell\",\"amount\":739.75153160,\"rate\":5.67100000,\"timestamp_created\":1365785706,\"status\":0}}}"
      case _ => ""
    }
  }

  def releaseConnections {}
}

object FakeCredentials extends ClientCredentials {
  val Key = ""
  val Secret = ""
}


class ApplicationSpec extends Specification {
  val testTradeClient = new TradeApiClient(FakeCredentials) with FakeHttpApiClient

  "Trade API Client" should {
    "make correct post body from map" in {
      testTradeClient.makePostDataFromMap(Map()) mustEqual ""
      testTradeClient.makePostDataFromMap(Map("from"->5)) mustEqual("&from=5")
      testTradeClient.makePostDataFromMap(Map("from"->5, "count"->3)) mustEqual("&from=5&count=3")
    }

    "make correct getInfo call" in {
      testTradeClient.getInfo().get.rur mustEqual 0.00000238
    }

    "make correct TransHistory call" in {
     val t0 = testTradeClient.transactionsHistory().get.head
     t0.id mustEqual 18586103
     t0.amount mustEqual 463.6248

     val t1 = testTradeClient.transactionsHistory().get.tail.head
     t1.orderId.get mustEqual 6917762
    }

    "make correct OrderList call" in {
      val order = testTradeClient.orderList().get.head
      order.currency1.toString mustEqual "LTC"
      order.amount mustEqual 739.7515316
      order.time.year.get mustEqual 2013
    }
  }

  val testDataClient = new MarketDataApiClient with FakeHttpApiClient

  "Marked Data API Client" should {
    "get correct fee info" in {
      testDataClient.fee(Currency.BTC, Currency.USD) mustEqual Some(0.2)
    }

    "get correct ticker info" in {
      testDataClient.ticker(Currency.BTC, Currency.USD).get.high mustEqual 259
    }

    "get correct trades info" in {
      val list = testDataClient.trades(Currency.BTC, Currency.USD).get
      list(0).price mustEqual 171
      list(1).amount mustEqual 0.028974
    }

    "get correct depth info" in {
      val (asks, bids) = testDataClient.depth(Currency.BTC, Currency.USD).get
      asks(0).price mustEqual 170.99
      bids(0).amount mustEqual 4.71834685
      bids(1).price mustEqual 170.02
    }
  }
}