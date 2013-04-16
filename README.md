btce-scala
==========

Scala Client for BTC-E Trade and Market Data APIs. BTC-E is the Cryptocurrencies(Bitcoin/Litecoin/Namecoin/...) Broker.



Dependencies
------------

Client depends on Lift-JSON library (a part of the Lift framework, but could be used separately). Add following SBT dependency:

`"net.liftweb" % "lift-json_2.10" % "2.5-M4"`

or download and put jar to appropriate folder etc.


Specs2 framework used for testing, add following dependency for it :

`"org.specs2" %% "specs2" % "1.14" % "test"`

(not needed if you use PlayFramework 2.x)

How to Use the Client
---------------------

(more info [in my blog](http://chepurnoy.org/blog/2013/04/scala-clients-for-btc-e-trade-and-public-data-apis-my-first-opensource-released/))

To set credentials, create own implementation of ClientCredentials trait:

    object MyClientCredentials extends ClientCredentials {
        val Key = "my key"
        val Secret = "my secret"
    }

By default, WS library from Play framework is used for HTTP requests. To replace 
it, implement abstract functions *getRequest* (for Market Data API Client), *signedPostRequest* (for Trade API Client), *releaseConnections* (if needed) from HttpApiClient trait
and make own trade API and market data API implementations as:

`class MyTradeApiClient(credentials: ClientCredentials) extends TradeApiClient(credentials) with MyHttpApiClient`

and

`class MyMarketDataApiClient extends MarketDataApiClient with MyHttpApiClient`


More info [in my blog][]

Weak points
-----------

* Current version extracts only funds information from *getInfo* method results
* Only WS HTTP library (from Play framework) supported now. But it's easy to work with an another

Donations
---------

Please, support my work:

BTC: 1DLFrNmQboQmZzo9aPKrcrJkLtXKh2hQuU

LTC: LS5D18dfqQVUqSGVsYCinwvTuwUZ9FJ89V

