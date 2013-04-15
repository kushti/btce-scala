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
 */

package org.chepurnoy.btce

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import org.apache.commons.codec.binary.Hex
import play.api.libs.ws.WS
import concurrent.Await
import concurrent.duration._


object SortingOrder extends Enumeration {
  type SortingOrder = Value
  val Asc = Value("ASC")
  val Desc = Value("DESC")
}

object Currency extends Enumeration {
  type Currency = Value
  val USD = Value("USD")
  val RUR = Value("RUR")
  val EUR = Value("EUR")
  val BTC = Value("BTC")
  val LTC = Value("LTC")
  val NMC = Value("NMC")
  val NVC = Value("NVC")
  val TRC = Value("TRC")
  val PPC = Value("PPC")
}

object Direction extends Enumeration {
  type Direction = Value
  val Sell = Value("sell")
  val Buy = Value("buy")
}

trait HttpApiClient {
  protected def signQuery(postData: String, secret: String) = {
    val key = new SecretKeySpec(secret.getBytes("UTF-8"), "HmacSHA512")
    val mac = Mac.getInstance("HmacSHA512")
    mac.init(key)
    new String(Hex.encodeHex(mac.doFinal(postData.getBytes("UTF-8"))))
  }

  protected def getRequest(url: String): String

  protected def signedPostRequest(url: String, key: String, Secret: String, postBody: String): String

  def releaseConnections
}


trait WsHttpApiClient extends HttpApiClient {
  val timeout = 30 seconds

  override def getRequest(url: String): String = {
    val futResp = WS.url(url).get()
    val resp = Await.result(futResp, timeout)
    resp.body
  }

  override def signedPostRequest(url: String, key: String, secret: String, postData: String): String = {
    val headers = Seq(("useragent", """BTC-E Scala API Client // by chepurnoy.org"""),
      ("Content-type", "application/x-www-form-urlencoded"),
      ("Key", key),
      ("Sign", signQuery(postData, secret)),
      ("accept", "application/json"))

    val resp = Await.result(WS.url(url).withHeaders(headers: _*).post(postData), timeout)
    resp.body
  }

  override def releaseConnections = WS.client.close
}



object Tester{
  def main(args:Array[String]){
    object MyClientCredentials extends ClientCredentials {
      val Key = "my key"
      val Secret = "my secret"
    }

    if (MyClientCredentials.Key.contains("key")||MyClientCredentials.Key.contains("secret")){
      println("Please set key and value in the MyClientCredentials object")
    }else{
      val client = new DefaultTradeApiClient(MyClientCredentials)
      client.getInfo.map(println(_))
      client.orderList.getOrElse(List()).foreach{o => println(s"${o.currency1}/${o.currency2} ${o.amount}@${o.rate}")}
      client.releaseConnections
    }
  }
}