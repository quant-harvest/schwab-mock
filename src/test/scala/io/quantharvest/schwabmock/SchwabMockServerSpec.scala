package io.quantharvest.schwabmock

import cats.effect.IO
import io.circe.Json
import io.circe.parser.parse
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.implicits.*
import org.typelevel.ci.CIString

class SchwabMockServerSpec extends CatsEffectSuite:

  test("SchwabMockServer serves OAuth2 tokens, account numbers, option chains, and quotes matching Schwab schema") {
    val engine = new ExchangeMatchingEngine()
    val app    = SchwabMockServer.routes(engine).orNotFound

    for
      // 1. OAuth token
      tokResp <- app.run(Request[IO](Method.POST, uri"/v1/oauth/token"))
      _        = assertEquals(tokResp.status, Status.Ok)
      tokBody <- tokResp.as[String]
      tokJson  = parse(tokBody).toOption.get
      _        = assert(tokJson.hcursor.downField("access_token").as[String].isRight)

      // 2. Account numbers
      accResp <- app.run(Request[IO](Method.GET, uri"/trader/v1/accounts/accountNumbers"))
      _        = assertEquals(accResp.status, Status.Ok)
      accBody <- accResp.as[String]
      accArr   = parse(accBody).toOption.get.asArray.get
      _        = assertEquals(accArr.size, 2)
      hash1    = accArr.head.hcursor.downField("hashValue").as[String].toOption.get

      // 3. Option chains
      chainResp <- app.run(Request[IO](Method.GET, uri"/marketdata/v1/chains?symbol=SPY&contractType=CALL"))
      _          = assertEquals(chainResp.status, Status.Ok)
      chainBody <- chainResp.as[String]
      chainJson  = parse(chainBody).toOption.get
      _          = assertEquals(chainJson.hcursor.downField("status").as[String], Right("SUCCESS"))
      _          = assert(chainJson.hcursor.downField("callExpDateMap").focus.exists(_.asObject.exists(_.nonEmpty)))

      // 4. Quotes (stock + OCC option)
      quoteResp <- app.run(Request[IO](Method.GET, uri"/marketdata/v1/quotes?symbols=SPY,NVDA"))
      _          = assertEquals(quoteResp.status, Status.Ok)
      quoteBody <- quoteResp.as[String]
      quoteJson  = parse(quoteBody).toOption.get
      spyMark    = quoteJson.hcursor.downField("SPY").downField("quote").downField("mark").as[Double]
      _          = assertEquals(spyMark, Right(585.0))
      _          = assert(hash1.nonEmpty)
    yield ()
  }

  test("SchwabMockServer validates tick increments, executes limit order cancel/replace walk, and updates account positions on fill") {
    val engine = new ExchangeMatchingEngine()
    val app    = SchwabMockServer.routes(engine).orNotFound
    val hash   = "MOCK_HASH_84920172_E5F6G7H8"

    // 1. Sub-penny price ($1.234) is rejected with HTTP 400
    val badTickPayload =
      """{
        |  "orderType": "LIMIT",
        |  "session": "NORMAL",
        |  "duration": "DAY",
        |  "orderStrategyType": "SINGLE",
        |  "price": 1.234,
        |  "orderLegCollection": [
        |    {
        |      "orderLegType": "OPTION",
        |      "legId": 1,
        |      "instruction": "BUY_TO_OPEN",
        |      "quantity": 2,
        |      "instrument": { "symbol": "SPY   261030C00585000", "assetType": "OPTION" }
        |    }
        |  ]
        |}""".stripMargin

    // 2. Passive bid ($0.50) rests as WORKING, then PUT replace to $25.00 crosses and fills immediately
    val passiveBidPayload = badTickPayload.replace("1.234", "0.50")
    val aggressiveReplacePayload = badTickPayload.replace("1.234", "25.00")

    for
      badResp <- app.run(Request[IO](Method.POST, Uri.unsafeFromString(s"/trader/v1/accounts/$hash/orders")).withEntity(badTickPayload))
      _        = assertEquals(badResp.status, Status.BadRequest)

      workResp <- app.run(Request[IO](Method.POST, Uri.unsafeFromString(s"/trader/v1/accounts/$hash/orders")).withEntity(passiveBidPayload))
      _         = assertEquals(workResp.status, Status.Created)
      locHdr    = workResp.headers.get(CIString("Location")).map(_.head.value).getOrElse("")
      orderId   = locHdr.split('/').last.toLong
      workBody <- workResp.as[String]
      workStatus = parse(workBody).toOption.get.hcursor.downField("status").as[String]
      _         = assertEquals(workStatus, Right("WORKING"))

      // Replace order with crossing limit price -> fills immediately
      repResp <- app.run(Request[IO](Method.PUT, Uri.unsafeFromString(s"/trader/v1/accounts/$hash/orders/$orderId")).withEntity(aggressiveReplacePayload))
      _        = assertEquals(repResp.status, Status.Ok)
      repBody <- repResp.as[String]
      repJson  = parse(repBody).toOption.get
      _        = assertEquals(repJson.hcursor.downField("status").as[String], Right("FILLED"))

      // Verify account now holds 2 long contracts of SPY 261030C00585000
      detResp <- app.run(Request[IO](Method.GET, Uri.unsafeFromString(s"/trader/v1/accounts/$hash?fields=positions")))
      detBody <- detResp.as[String]
      detJson  = parse(detBody).toOption.get
      positions = detJson.hcursor.downField("securitiesAccount").downField("positions").focus.flatMap(_.asArray).getOrElse(Vector.empty)
      _        = assertEquals(positions.size, 1)
      longQty  = positions.head.hcursor.downField("longQuantity").as[Double]
      _        = assertEquals(longQty, Right(2.0))
    yield ()
  }

  test("SchwabMockServer ChaosConfig simulates Fill-Before-Replace race condition (HTTP 400 on PUT while order transitions to FILLED)") {
    val engine = new ExchangeMatchingEngine()
    engine.setChaosConfig(ChaosConfig(simulateFillBeforeReplaceRace = true))
    val app  = SchwabMockServer.routes(engine).orNotFound
    val hash = "MOCK_HASH_84920172_E5F6G7H8"

    val passivePayload =
      """{
        |  "orderType": "LIMIT",
        |  "session": "NORMAL",
        |  "duration": "DAY",
        |  "orderStrategyType": "SINGLE",
        |  "price": 0.50,
        |  "orderLegCollection": [
        |    {
        |      "orderLegType": "OPTION",
        |      "legId": 1,
        |      "instruction": "BUY_TO_OPEN",
        |      "quantity": 1,
        |      "instrument": { "symbol": "QQQ   261030C00505000", "assetType": "OPTION" }
        |    }
        |  ]
        |}""".stripMargin

    for
      postResp <- app.run(Request[IO](Method.POST, Uri.unsafeFromString(s"/trader/v1/accounts/$hash/orders")).withEntity(passivePayload))
      _         = assertEquals(postResp.status, Status.Created)
      orderId   = postResp.headers.get(CIString("Location")).get.head.value.split('/').last.toLong

      // Attempt PUT replacement when simulateFillBeforeReplaceRace = true -> returns 400 BadRequest AND fills the original order
      putResp  <- app.run(Request[IO](Method.PUT, Uri.unsafeFromString(s"/trader/v1/accounts/$hash/orders/$orderId")).withEntity(passivePayload.replace("0.50", "0.55")))
      _         = assertEquals(putResp.status, Status.BadRequest)

      filledResp <- app.run(Request[IO](Method.GET, Uri.unsafeFromString(s"/trader/v1/accounts/$hash/orders?status=FILLED")))
      filledBody <- filledResp.as[String]
      filledArr   = parse(filledBody).toOption.get.asArray.get
      _           = assertEquals(filledArr.size, 1)
      _           = assertEquals(filledArr.head.hcursor.downField("orderId").as[Long], Right(orderId))
    yield ()
  }
