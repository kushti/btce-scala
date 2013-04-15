/**
 * Copyright (C) 2013 Alexander Chepurnoy (http://chepurnoy.org/)
 *
 * If you like it, send donations to
 * BTC: 1NJPJf1dqkEXwfBCHBLvLoQBMd6TG7gdLy
 * LTC: 4G5g5Ht6rSGz3bZ6NXua7rSgBkRboBVygi
 *
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Please refer to 'API' section at https://btc-e.com/page/2 for API description
 *
 */

package org.chepurnoy.btce

import net.liftweb.json._
import net.liftweb.json.JsonAST.JDouble
import org.joda.time.DateTime


case class TradeInfo(tradeId: Long, price: Double, amount: Double, currency1:Currency.Value, currency2:Currency.Value, tradeType:Direction.Value, time:DateTime)

case class TickerData(currency1:Currency.Value, currency2:Currency.Value, high:Double, low:Double, avg:Double, cur1Vol:Double, cur2Vol:Double,
                      last: Double, buy:Double, sell:Double, time:DateTime){
  override def toString = s"Time: ${time} High: $high, Low: $low, Avg: $avg, Volume(${currency1}): $cur1Vol, Volume(${currency2}): $cur2Vol, Last $last, Buy: $buy, Sell: $sell"
}

class BidOrAsk(price: Double, amount: Double)
case class Bid(price: Double, amount: Double) extends BidOrAsk(price, amount)
case class Ask(price: Double, amount: Double) extends BidOrAsk(price, amount)



case class RawTradeInfo(date: Long, price: Double, amount: Double, tid: Long, price_currency: String, item: String, trade_type: String)
case class RawTickerData(high: Double, low: Double, avg: Double, vol: Double, vol_cur: Double, last: Double, buy: Double, sell: Double, server_time: Long){
  override def toString = s"High: $high, Low: $low, Avg: $avg, Volume: $vol, Last $last, Buy: $buy, Sell: $sell"
}

abstract class MarketDataApiClient extends HttpApiClient {
  implicit val formats = net.liftweb.json.DefaultFormats

  def baseUrl(currency1: Currency.Value, currency2: Currency.Value) = s"https://btc-e.com/api/2/${currency1.toString.toLowerCase}_${currency2.toString.toLowerCase}"

  def getParsedJson(urlSuffix: String, currency1: Currency.Value, currency2: Currency.Value): Option[JValue] = {
    try {
      val resp = getRequest(s"${baseUrl(currency1, currency2)}/$urlSuffix")
      Some(JsonParser.parse(resp))
    } catch {
      case t: Throwable => None
    }
  }

  def fee(currency1: Currency.Value, currency2: Currency.Value): Option[Double] = {
    getParsedJson("fee", currency1, currency2).map {
      json => val JDouble(d) = (json \ "trade")
      d
    }
  }

  def ticker(currency1: Currency.Value, currency2: Currency.Value) = {
    val raw = getParsedJson("ticker", currency1, currency2).map {
      json => (json \ "ticker").extract[RawTickerData]
    }
    raw.map {r=>
      TickerData(currency1, currency2, r.high, r.low, r.avg, r.vol_cur, r.vol, r.last, r.buy, r.sell, new DateTime(r.server_time * 1000L))
    }
  }

  def trades(currency1: Currency.Value, currency2: Currency.Value) = {
    val raws = getParsedJson("trades", currency1, currency2).map {
      json => json.extract[List[RawTradeInfo]]
    }
    raws.map {_.map{raw=>
      val d = if(raw.trade_type=="ask") Direction.Sell else Direction.Buy

      TradeInfo(raw.tid, raw.price, raw.amount, Currency.withName(raw.item),Currency.withName(raw.price_currency),
                d,
                new DateTime(raw.date * 1000L))
    }
    }
  }


  def depth(currency1: Currency.Value, currency2: Currency.Value): Option[(List[Ask], List[Bid])] = {
    getParsedJson("depth", currency1, currency2).map {
      json =>
        (((json \ "asks").extract[List[List[Double]]]).map(l => Ask(l(0), l(1))),
          ((json \ "bids").extract[List[List[Double]]]).map(l => Bid(l(0), l(1))))
    }
  }
}

class DefaultMarketDataApiClient extends MarketDataApiClient with WsHttpApiClient