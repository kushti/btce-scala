/**
 * Copyright (C) 2013 Alexander Chepurnoy (http://chepurnoy.org/)
 *
 * If you like it, send donations to
 * BTC: 1DLFrNmQboQmZzo9aPKrcrJkLtXKh2hQuU
 * LTC: LS5D18dfqQVUqSGVsYCinwvTuwUZ9FJ89V
 *
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Please refer to https://btc-e.com/api/documentation for API description
 *
 */

package org.chepurnoy.btce

import org.joda.time.DateTime
import net.liftweb.json._

trait ClientCredentials {
  val Key: String
  val Secret: String
}

case class FundsInfo(usd: Double, rur: Double, eur: Double, btc: Double, ltc: Double, nmc: Double, nvc: Double, trc: Double, ppc: Double) {
  override def toString = s"USD: $usd RUR: $rur EUR: $eur BTC: $btc LTC: $ltc NMC: $nmc NVC: $nvc TRC: $trc PPC: $ppc"
}

case class TransactionInfo(id: Long, transType: Int, amount: Double, currency: Currency.Value, desc: String, status: Int, timestamp: DateTime, orderId: Option[Long])

case class OrderInfo(orderId: Long, currency1: Currency.Value, currency2: Currency.Value, orderType: Direction.Value,
                     amount: Double, rate: Double, time: DateTime, status: Int)

case class ArchivedTrade(tradeId: Long, currency1: Currency.Value, currency2: Currency.Value, tradeType: Direction.Value,
                         amount: Double, rate: Double, order_id: Long, isYour: Boolean, time: DateTime)

case class TradeCommandResult(received: Double, remains: Double, order_id: Long, funds: FundsInfo)




case class RawTransactionInfo(`type`: Int, amount: Double, currency: String, desc: String, status: Int, timestamp: Long)
case class RawOrderInfo(pair: String, `type`: String, amount: Double, rate: Double, timestamp_created: Long, status: Int)
case class RawAccountTradeInfo(pair: String, `type`: String, amount: Double, rate: Double, order_id: Long, is_your_order: Short, timestamp: Long)



abstract class TradeApiClient(credentials: ClientCredentials) extends HttpApiClient {
  implicit val formats = net.liftweb.json.DefaultFormats

  val url = "https://btc-e.com/tapi"

  def makePostDataFromMap(map: Map[String, Any]) = map.foldLeft("") {
    case (a, (k, v)) => a + s"&$k=$v"
  }

  def request(methodName: String, postParams: String): String = {
    val nonce = System.currentTimeMillis() / 1000 + ""
    val postData = "nonce=" + nonce + "&method=" + methodName + postParams
    signedPostRequest(url, credentials.Key, credentials.Secret, postData)
  }

  def getParsedJson(method: String, paramsMap: Map[String, Any]): Option[JValue] = {
    try {
      val resp = request(method, makePostDataFromMap(paramsMap))
      val json = JsonParser.parse(resp)

      if ((json \ "success").extractOpt[Int].getOrElse(0) == 1) {
        Some(json \ "return")
      } else {
        None
      }
    } catch {
      case t: Throwable => None
    }
  }

  def getInfo(): Option[FundsInfo] = {
    getParsedJson("getInfo", Map()).map {
      js => (js \ "funds").extract[FundsInfo]
    }
  }

  def transactionsHistory(transFrom: Option[Long], transCount: Option[Int], transFromId: Option[Long], transEndId: Option[Long],
                          sortingOrder: Option[SortingOrder.Value], sinceUnixTime: Option[Long], endUnixTime: Option[Long]): Option[List[TransactionInfo]] = {
    def extractOrderId(desc: String): Option[Long] = """:order:(\d{7}):""".r findFirstMatchIn (desc) map {
      _.group(1).toLong
    }

    val params = Map("from" -> transFrom, "count" -> transCount,
      "from_id" -> transFromId, "end_id" -> transEndId, "order" -> sortingOrder, "since" -> sinceUnixTime, "end" -> endUnixTime).filter(_._2.isDefined).map {
      case (s, opt) => (s, opt.get)
    }

    val rawInfo = getParsedJson("TransHistory", params).map {
      _.children.map {
        case f: JField => (f.name, f.extract[RawTransactionInfo])
      }
    }

    rawInfo.map {
      _.map {
        case (s: String, r: RawTransactionInfo) =>
          TransactionInfo(s.toLong, r.`type`, r.amount, Currency.withName(r.currency), r.desc, r.status, new DateTime(r.timestamp * 1000L), extractOrderId(r.desc))
      }
    }
  }

  def tradeHistory(transFrom: Option[Long], transCount: Option[Int], transFromId: Option[Long], transEndId: Option[Long],
                   sortingOrder: Option[SortingOrder.Value], sinceUnixTime: Option[Long], endUnixTime: Option[Long], pair: Option[String]): Option[List[ArchivedTrade]] = {
    val params = Map("from" -> transFrom, "count" -> transCount,
      "from_id" -> transFromId, "end_id" -> transEndId, "order" -> sortingOrder, "since" -> sinceUnixTime,
      "end" -> endUnixTime, "pair" -> pair).filter(_._2.isDefined).map {
      case (s, opt) => (s, opt.get)
    }

    val raws = getParsedJson("TradeHistory", params).map {
      _.children.map {
        case f: JField => (f.name, f.extract[RawAccountTradeInfo])
      }
    }
    raws.map {
      _.map {
        case (s: String, t: RawAccountTradeInfo) =>
          val cur1 = t.pair.substring(0, 3).toUpperCase
          val cur2 = t.pair.substring(4, 7).toUpperCase
          ArchivedTrade(s.toLong, Currency.withName(cur1), Currency.withName(cur2), Direction.withName(t.`type`),
            t.amount, t.rate, t.order_id, (t.is_your_order == 1), new DateTime(t.timestamp * 1000L))
      }
    }
  }

  def orderList(): Option[List[OrderInfo]] = {
    val params = Map[String, Any]()

    val raws = getParsedJson("OrderList", params).map {
      json => json.children.map {
        case f: JField => (f.name, f.extract[RawOrderInfo])
      }
    }
    raws.map {
      _.map {
        case (s: String, o: RawOrderInfo) =>
          val cur1 = o.pair.substring(0, 3).toUpperCase
          val cur2 = o.pair.substring(4, 7).toUpperCase
          OrderInfo(s.toLong, Currency.withName(cur1), Currency.withName(cur2), Direction.withName(o.`type`), o.amount,
            o.rate, new DateTime(o.timestamp_created * 1000L), o.status)
      }
    }
  }

  def trade(currency1: Currency.Value, currency2: Currency.Value,
            direction: Direction.Value, rate: Double, amount: Double): Option[TradeCommandResult] = {
    val params = Map("pair" -> s"${currency1.toString.toLowerCase}_${currency2.toString.toLowerCase}",
      "type" -> s"${direction.toString.toLowerCase}",
      "rate" -> rate,
      "amount" -> amount
    )

    getParsedJson("Trade", params) map {
      json =>
        json.extract[TradeCommandResult]
    }

  }

  def cancelOrder(orderId: Long): Option[FundsInfo] = {
    val params = Map("order_id" -> orderId)
    getParsedJson("CancelOrder", params) map {
      json =>
        (json \ "funds").extract[FundsInfo]
    }
  }

  //API sugar

  def transactionsHistory(sinceUnixTime: Long, endUnixTime: Long): Option[List[TransactionInfo]] =
    transactionsHistory(None, None, None, None, None, Some(sinceUnixTime), Some(endUnixTime))

  def transactionsHistory(from: Long, count: Int): Option[List[TransactionInfo]] =
    transactionsHistory(Some(from), Some(count), None, None, None, None, None)

  def transactionsHistory(): Option[List[TransactionInfo]] = transactionsHistory(None, None, None, None, None, None, None)


  def tradeHistory(pair:String): Option[List[ArchivedTrade]] =
    tradeHistory(None, None, None, None, None, None, None, Some(pair))

  def tradeHistory(sinceUnixTime: Long, endUnixTime: Long): Option[List[ArchivedTrade]] =
    tradeHistory(None, None, None, None, None, Some(sinceUnixTime), Some(endUnixTime), None)

  def tradeHistory(sinceUnixTime: Long, endUnixTime: Long, pair:String): Option[List[ArchivedTrade]] =
    tradeHistory(None, None, None, None, None, Some(sinceUnixTime), Some(endUnixTime), Some(pair))

  def tradeHistory(from: Long, count: Int): Option[List[ArchivedTrade]] =
    tradeHistory(Some(from), Some(count), None, None, None, None, None, None)

  def tradeHistory(from: Long, count: Int, pair:String): Option[List[ArchivedTrade]] =
    tradeHistory(Some(from), Some(count), None, None, None, None, None, Some(pair))

  def tradeHistory(): Option[List[ArchivedTrade]] = tradeHistory(None, None, None, None, None, None, None, None)
}


class DefaultTradeApiClient(credentials: ClientCredentials) extends TradeApiClient(credentials) with WsHttpApiClient
