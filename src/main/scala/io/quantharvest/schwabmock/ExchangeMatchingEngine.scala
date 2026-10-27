package io.quantharvest.schwabmock

import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax.*
import java.time.{Instant, LocalDate, ZoneId}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.{AtomicLong, AtomicReference}
import scala.jdk.CollectionConverters.*
import scala.util.Try

final case class ChaosConfig(
  enforceTickSize: Boolean = true,
  enforceBuyingPower: Boolean = true,
  autoFillCrossingOrders: Boolean = true,
  autoFillMidTolerancePct: Double = 0.015,
  simulateFillBeforeReplaceRace: Boolean = false,
  simulateSpreadCloseOneLegReject: Boolean = false,
  simulateRateLimit429: Boolean = false
)

object ChaosConfig:
  given Encoder[ChaosConfig] = Encoder.instance { c =>
    Json.obj(
      "enforceTickSize"                 -> Json.fromBoolean(c.enforceTickSize),
      "enforceBuyingPower"              -> Json.fromBoolean(c.enforceBuyingPower),
      "autoFillCrossingOrders"          -> Json.fromBoolean(c.autoFillCrossingOrders),
      "autoFillMidTolerancePct"         -> Json.fromDoubleOrNull(c.autoFillMidTolerancePct),
      "simulateFillBeforeReplaceRace"   -> Json.fromBoolean(c.simulateFillBeforeReplaceRace),
      "simulateSpreadCloseOneLegReject" -> Json.fromBoolean(c.simulateSpreadCloseOneLegReject),
      "simulateRateLimit429"            -> Json.fromBoolean(c.simulateRateLimit429)
    )
  }

  given Decoder[ChaosConfig] = Decoder.instance { c =>
    for
      enforceTickSize                 <- c.downField("enforceTickSize").as[Boolean].orElse(Right(true))
      enforceBuyingPower              <- c.downField("enforceBuyingPower").as[Boolean].orElse(Right(true))
      autoFillCrossingOrders          <- c.downField("autoFillCrossingOrders").as[Boolean].orElse(Right(true))
      autoFillMidTolerancePct         <- c.downField("autoFillMidTolerancePct").as[Double].orElse(Right(0.015))
      simulateFillBeforeReplaceRace   <- c.downField("simulateFillBeforeReplaceRace").as[Boolean].orElse(Right(false))
      simulateSpreadCloseOneLegReject <- c.downField("simulateSpreadCloseOneLegReject").as[Boolean].orElse(Right(false))
      simulateRateLimit429            <- c.downField("simulateRateLimit429").as[Boolean].orElse(Right(false))
    yield ChaosConfig(
      enforceTickSize                 = enforceTickSize,
      enforceBuyingPower              = enforceBuyingPower,
      autoFillCrossingOrders          = autoFillCrossingOrders,
      autoFillMidTolerancePct         = autoFillMidTolerancePct,
      simulateFillBeforeReplaceRace   = simulateFillBeforeReplaceRace,
      simulateSpreadCloseOneLegReject = simulateSpreadCloseOneLegReject,
      simulateRateLimit429            = simulateRateLimit429
    )
  }

final case class MockOrderLeg(
  orderLegType: String,
  legId: Long,
  instruction: String,
  quantity: Double,
  symbol: String,
  assetType: String
):
  def toJson: Json =
    val qtyJson = if quantity == quantity.toLong.toDouble then Json.fromLong(quantity.toLong) else Json.fromDoubleOrNull(quantity)
    Json.obj(
      "orderLegType" -> Json.fromString(orderLegType),
      "legId"        -> Json.fromLong(legId),
      "instruction"  -> Json.fromString(instruction),
      "quantity"     -> qtyJson,
      "instrument"   -> Json.obj(
        "symbol"    -> Json.fromString(symbol),
        "assetType" -> Json.fromString(assetType)
      )
    )

final case class MockOrderRecord(
  orderId: Long,
  accountHash: String,
  accountNumber: String,
  orderType: String,
  session: String,
  duration: String,
  orderStrategyType: String,
  complexOrderStrategyType: Option[String],
  price: Option[Double],
  stopPrice: Option[Double],
  quantity: Double,
  filledQuantity: Double,
  status: String,
  enteredTime: String,
  closeTime: Option[String],
  replacedByOrderId: Option[Long],
  statusDescription: Option[String],
  legs: List[MockOrderLeg],
  tag: Option[String] = Some("API_QUANTHARVEST")
):
  def primarySymbol: String = legs.headOption.map(_.symbol).getOrElse("UNKNOWN")
  def primaryInstruction: String = legs.headOption.map(_.instruction).getOrElse("BUY")

  def toSchwabJson: Json =
    val fields = List(
      Some("orderId"              -> Json.fromLong(orderId)),
      Some("orderType"            -> Json.fromString(orderType)),
      Some("session"              -> Json.fromString(session)),
      Some("duration"             -> Json.fromString(duration)),
      Some("orderStrategyType"    -> Json.fromString(orderStrategyType)),
      complexOrderStrategyType.map(cs => "complexOrderStrategyType" -> Json.fromString(cs)),
      price.map(p => "price"      -> Json.fromDoubleOrNull(p)),
      stopPrice.map(sp => "stopPrice" -> Json.fromDoubleOrNull(sp)),
      Some("quantity"             -> Json.fromDoubleOrNull(quantity)),
      Some("filledQuantity"       -> Json.fromDoubleOrNull(filledQuantity)),
      Some("remainingQuantity"    -> Json.fromDoubleOrNull(math.max(0.0, quantity - filledQuantity))),
      Some("status"               -> Json.fromString(status)),
      Some("enteredTime"          -> Json.fromString(enteredTime)),
      closeTime.map(ct => "closeTime" -> Json.fromString(ct)),
      replacedByOrderId.map(rid => "replacedByOrderId" -> Json.fromLong(rid)),
      statusDescription.map(sd => "statusDescription" -> Json.fromString(sd)),
      Some("accountNumber"        -> Json.fromString(accountNumber)),
      Some("orderLegCollection"   -> Json.fromValues(legs.map(_.toJson))),
      tag.map(t => "tag"          -> Json.fromString(t))
    ).flatten
    Json.obj(fields*)

final case class MockPositionRecord(
  symbol: String,
  assetType: String,
  description: String,
  longQuantity: Double,
  shortQuantity: Double,
  averagePrice: Double,
  underlyingSymbol: Option[String],
  putCall: Option[String]
)

final case class MockAccountState(
  accountNumber: String,
  hashValue: String,
  nickName: String,
  accountType: String,
  initialCash: Double,
  cashBalance: Double,
  positions: Map[String, MockPositionRecord]
)

final case class ComputedQuote(
  symbol: String,
  bid: Double,
  ask: Double,
  mark: Double,
  last: Double,
  openPrice: Double,
  highPrice: Double,
  lowPrice: Double,
  closePrice: Double,
  netPercentChange: Double,
  delta: Double,
  gamma: Double,
  theta: Double,
  vega: Double,
  volatility: Double
):
  def toSchwabQuoteEntryJson: Json =
    Json.obj(
      "symbol" -> Json.fromString(symbol),
      "quote"  -> Json.obj(
        "bidPrice"         -> Json.fromDoubleOrNull(bid),
        "askPrice"         -> Json.fromDoubleOrNull(ask),
        "mark"             -> Json.fromDoubleOrNull(mark),
        "lastPrice"        -> Json.fromDoubleOrNull(last),
        "openPrice"        -> Json.fromDoubleOrNull(openPrice),
        "highPrice"        -> Json.fromDoubleOrNull(highPrice),
        "lowPrice"         -> Json.fromDoubleOrNull(lowPrice),
        "closePrice"       -> Json.fromDoubleOrNull(closePrice),
        "netPercentChange" -> Json.fromDoubleOrNull(netPercentChange),
        "delta"            -> Json.fromDoubleOrNull(delta),
        "gamma"            -> Json.fromDoubleOrNull(gamma),
        "theta"            -> Json.fromDoubleOrNull(theta),
        "vega"             -> Json.fromDoubleOrNull(vega),
        "volatility"       -> Json.fromDoubleOrNull(volatility * 100.0)
      )
    )

/**
 * Stateful Wire-Level Charles Schwab Exchange & Matching Engine.
 */
final class ExchangeMatchingEngine:

  private val EasternZone = ZoneId.of("America/New_York")
  private val OccDateFmt  = DateTimeFormatter.ofPattern("yyMMdd")
  private val nextOrderId = new AtomicLong(1009000000001L)
  private val nextActivityId = new AtomicLong(8001000001L)
  private val chaosRef    = new AtomicReference[ChaosConfig](ChaosConfig())

  private val underlyingPrices = new ConcurrentHashMap[String, Double]()
  private val accountsByHash   = new ConcurrentHashMap[String, MockAccountState]()
  private val ordersById       = new ConcurrentHashMap[Long, MockOrderRecord]()
  private val transactionsByAccount = new ConcurrentHashMap[String, Vector[Json]]()
  private val spreadRejectTriggeredOnce = new ConcurrentHashMap[String, java.lang.Boolean]()

  resetToDefaults()

  def getChaosConfig: ChaosConfig = chaosRef.get()
  def setChaosConfig(cfg: ChaosConfig): ChaosConfig =
    chaosRef.set(cfg)
    if !cfg.simulateSpreadCloseOneLegReject then spreadRejectTriggeredOnce.clear()
    cfg

  def resetToDefaults(): Unit =
    underlyingPrices.clear()
    Map(
      "SPY"  -> 585.00,
      "$SPX" -> 5850.00,
      "QQQ"  -> 502.50,
      "NVDA" -> 141.20,
      "AAPL" -> 231.50,
      "AMZN" -> 210.40,
      "META" -> 592.00,
      "MSFT" -> 428.00,
      "TSLA" -> 258.00,
      "GOOG" -> 182.50,
      "IWM"  -> 224.00,
      "AVGO" -> 178.00,
      "AMD"  -> 156.00,
      "ET"   -> 16.80,
      "KMI"  -> 24.50,
      "PCG"  -> 20.40
    ).foreach { case (k, v) => underlyingPrices.put(k, v) }

    accountsByHash.clear()
    ordersById.clear()
    transactionsByAccount.clear()
    spreadRejectTriggeredOnce.clear()

    val acc1 = MockAccountState(
      accountNumber = "84920171",
      hashValue     = "MOCKHASH84920171A1B2C3D4",
      nickName      = "Mock Primary Margin",
      accountType   = "MARGIN",
      initialCash   = 250000.0,
      cashBalance   = 250000.0,
      positions     = Map(
        "ET" -> MockPositionRecord(
          symbol           = "ET",
          assetType        = "EQUITY",
          description      = "ENERGY TRANSFER LP",
          longQuantity     = 1000.0,
          shortQuantity    = 0.0,
          averagePrice     = 15.20,
          underlyingSymbol = Some("ET"),
          putCall          = None
        )
      )
    )
    val acc2 = MockAccountState(
      accountNumber = "84920172",
      hashValue     = "MOCKHASH84920172E5F6G7H8",
      nickName      = "Mock Tactical Options",
      accountType   = "MARGIN",
      initialCash   = 100000.0,
      cashBalance   = 100000.0,
      positions     = Map.empty
    )
    accountsByHash.put(acc1.hashValue, acc1)
    accountsByHash.put(acc2.hashValue, acc2)

  def getUnderlyingPrices: Map[String, Double] =
    underlyingPrices.asScala.toMap

  def setUnderlyingPrice(symbol: String, price: Double): Unit =
    val clean = symbol.trim.toUpperCase
    if price > 0.0 then
      underlyingPrices.put(clean, price)
      stepMatchingCycle()

  def listAccountHashes: List[Json] =
    accountsByHash.values().asScala.toList.sortBy(_.accountNumber).map { a =>
      Json.obj(
        "accountNumber" -> Json.fromString(a.accountNumber),
        "hashValue"     -> Json.fromString(a.hashValue)
      )
    }

  def getUserPreferencesJson: Json =
    val accs = accountsByHash.values().asScala.toList.sortBy(_.accountNumber).zipWithIndex.map { case (a, idx) =>
      Json.obj(
        "accountNumber"      -> Json.fromString(a.accountNumber),
        "primaryAccount"     -> Json.fromBoolean(idx == 0),
        "type"               -> Json.fromString(a.accountType),
        "nickName"           -> Json.fromString(a.nickName),
        "accountColor"       -> Json.fromString(if idx == 0 then "Green" else "Blue"),
        "displayAcctId"      -> Json.fromString(a.accountNumber),
        "autoPositionEffect" -> Json.fromBoolean(true)
      )
    }
    Json.obj(
      "accounts" -> Json.fromValues(accs),
      "streamerInfo" -> Json.arr(
        Json.obj(
          "streamerSocketUrl"       -> Json.fromString("ws://localhost:8095/ws/streamer"),
          "schwabClientCustomerId" -> Json.fromString("MOCK_CUSTOMER"),
          "schwabClientCorrelId"   -> Json.fromString("MOCK_CORREL_ID"),
          "schwabClientChannel"    -> Json.fromString("N9"),
          "schwabClientFunctionId" -> Json.fromString("APIAPP")
        )
      )
    )

  private def resolveOrCreateAccount(accountHashOrNum: String): MockAccountState =
    val clean = accountHashOrNum.trim
    Option(accountsByHash.get(clean))
      .orElse(accountsByHash.values().asScala.find(a => a.accountNumber == clean || a.hashValue.equalsIgnoreCase(clean)))
      .getOrElse {
        val synthNum = if clean.forall(_.isDigit) && clean.length >= 4 then clean else s"8499${math.abs(clean.hashCode) % 10000}%04d"
        val created = MockAccountState(
          accountNumber = synthNum,
          hashValue     = clean,
          nickName      = s"Mock Account $synthNum",
          accountType   = "MARGIN",
          initialCash   = 100000.0,
          cashBalance   = 100000.0,
          positions     = Map.empty
        )
        accountsByHash.put(clean, created)
        created
      }

  def getAccountDetailsJson(accountHash: String): Json =
    val acc = resolveOrCreateAccount(accountHash)
    val posJsons = acc.positions.values.toList.sortBy(_.symbol).map { p =>
      val q = computeQuote(p.symbol)
      val mult = if p.assetType == "OPTION" then 100.0 else 1.0
      val netQty = if p.shortQuantity > 0.0 then -p.shortQuantity else p.longQuantity
      val absQty = math.abs(netQty)
      val mktVal = round2(netQty * q.mark * mult)
      val costVal = absQty * p.averagePrice * mult
      val pnl = round2(
        if netQty >= 0.0 then (q.mark - p.averagePrice) * absQty * mult
        else (p.averagePrice - q.mark) * absQty * mult
      )
      val pnlPct = if costVal > 0.0 then round2((pnl / costVal) * 100.0) else 0.0
      Json.obj(
        "longQuantity"                   -> Json.fromDoubleOrNull(p.longQuantity),
        "shortQuantity"                  -> Json.fromDoubleOrNull(p.shortQuantity),
        "averagePrice"                   -> Json.fromDoubleOrNull(p.averagePrice),
        "marketValue"                    -> Json.fromDoubleOrNull(mktVal),
        "currentDayProfitLoss"           -> Json.fromDoubleOrNull(pnl),
        "currentDayProfitLossPercentage" -> Json.fromDoubleOrNull(pnlPct),
        "instrument"                     -> Json.obj(
          "symbol"           -> Json.fromString(p.symbol),
          "assetType"        -> Json.fromString(p.assetType),
          "description"      -> Json.fromString(p.description),
          "underlyingSymbol" -> p.underlyingSymbol.fold(Json.Null)(Json.fromString),
          "putCall"          -> p.putCall.fold(Json.Null)(Json.fromString)
        )
      )
    }

    val totalPosMarketValue = acc.positions.values.map { p =>
      val q = computeQuote(p.symbol)
      val mult = if p.assetType == "OPTION" then 100.0 else 1.0
      val netQty = if p.shortQuantity > 0.0 then -p.shortQuantity else p.longQuantity
      netQty * q.mark * mult
    }.sum
    val liqVal = round2(acc.cashBalance + totalPosMarketValue)
    val dayPnl = round2(liqVal - acc.initialCash)
    val dayPnlPct = if acc.initialCash > 0.0 then round2((dayPnl / acc.initialCash) * 100.0) else 0.0

    Json.obj(
      "securitiesAccount" -> Json.obj(
        "accountNumber" -> Json.fromString(acc.accountNumber),
        "type"          -> Json.fromString(acc.accountType),
        "currentBalances" -> Json.obj(
          "liquidationValue"        -> Json.fromDoubleOrNull(liqVal),
          "cashBalance"             -> Json.fromDoubleOrNull(round2(acc.cashBalance)),
          "buyingPower"             -> Json.fromDoubleOrNull(round2(math.max(0.0, acc.cashBalance * 2.0))),
          "availableFunds"          -> Json.fromDoubleOrNull(round2(acc.cashBalance)),
          "dayProfitLoss"           -> Json.fromDoubleOrNull(dayPnl),
          "dayProfitLossPercentage" -> Json.fromDoubleOrNull(dayPnlPct)
        ),
        "initialBalances" -> Json.obj(
          "liquidationValue" -> Json.fromDoubleOrNull(acc.initialCash)
        ),
        "positions" -> Json.fromValues(posJsons)
      )
    )

  def listOrders(accountHashOpt: Option[String], statusFilterOpt: Option[String]): List[MockOrderRecord] =
    val targetAccOpt = accountHashOpt.map(resolveOrCreateAccount)
    ordersById.values().asScala.toList
      .filter { o =>
        targetAccOpt.forall(a => o.accountHash == a.hashValue || o.accountNumber == a.accountNumber) &&
        statusFilterOpt.forall(st => o.status.equalsIgnoreCase(st))
      }
      .sortBy(-_.orderId)

  def listTransactionsJson(accountHash: String): List[Json] =
    val acc = resolveOrCreateAccount(accountHash)
    Option(transactionsByAccount.get(acc.hashValue)).map(_.toList).getOrElse(Nil)

  /**
   * Places a new order (`POST /trader/v1/accounts/{accountHash}/orders`), validating Schwab wire rules
   * and immediately matching if `autoFillCrossingOrders` is enabled and the limit price crosses.
   */
  def placeOrder(accountHash: String, bodyJson: Json): Either[(Int, String), MockOrderRecord] =
    val chaos = chaosRef.get()
    if chaos.simulateRateLimit429 then
      Left((429, """{"message":"Too Many Requests - Simulated 429 rate limit from SchwabMock"}"""))
    else
      val acc = resolveOrCreateAccount(accountHash)
      val c = bodyJson.hcursor
      val orderType = c.downField("orderType").as[String].getOrElse("LIMIT").trim.toUpperCase
      val session   = c.downField("session").as[String].getOrElse("NORMAL").trim.toUpperCase
      val duration  = c.downField("duration").as[String].getOrElse("DAY").trim.toUpperCase
      val stratType = c.downField("orderStrategyType").as[String].getOrElse("SINGLE").trim.toUpperCase
      val complexOpt = c.downField("complexOrderStrategyType").as[String].toOption.map(_.trim.toUpperCase).filter(_.nonEmpty)
      val priceOpt  = c.downField("price").as[Double].toOption
      val stopOpt   = c.downField("stopPrice").as[Double].toOption

      val rawLegs = c.downField("orderLegCollection").focus.flatMap(_.asArray).getOrElse(Vector.empty)
      val legs: List[MockOrderLeg] = rawLegs.zipWithIndex.flatMap { case (lj, idx) =>
        val lc = lj.hcursor
        val inst = lc.downField("instrument")
        for
          instruction <- lc.downField("instruction").as[String].toOption
          qty         <- lc.downField("quantity").as[Double].toOption
          sym         <- inst.downField("symbol").as[String].orElse(lc.downField("symbol").as[String]).toOption
        yield
          val assetType = inst.downField("assetType").as[String].orElse(lc.downField("orderLegType").as[String]).getOrElse("OPTION").toUpperCase
          val legId = lc.downField("legId").as[Long].getOrElse((idx + 1).toLong)
          MockOrderLeg(assetType, legId, instruction.trim.toUpperCase, qty, sym.trim.toUpperCase, assetType)
      }.toList

      if legs.isEmpty then
        Left((400, """{"message":"Validation error: orderLegCollection must contain at least one leg"}"""))
      else
        validateOrderRules(acc, orderType, priceOpt, stopOpt, legs, chaos) match
          case Some((code, errMsg)) =>
            Left((code, Json.obj("message" -> Json.fromString(errMsg)).noSpaces))
          case None =>
            val oid = nextOrderId.getAndIncrement()
            val nowIso = DateTimeFormatter.ISO_INSTANT.format(Instant.now())
            val primaryQty = legs.head.quantity
            val initialRecord = MockOrderRecord(
              orderId                  = oid,
              accountHash              = acc.hashValue,
              accountNumber            = acc.accountNumber,
              orderType                = orderType,
              session                  = session,
              duration                 = duration,
              orderStrategyType        = stratType,
              complexOrderStrategyType = complexOpt,
              price                    = priceOpt,
              stopPrice                = stopOpt,
              quantity                 = primaryQty,
              filledQuantity           = 0.0,
              status                   = "WORKING",
              enteredTime              = nowIso,
              closeTime                = None,
              replacedByOrderId        = None,
              statusDescription        = None,
              legs                     = legs
            )
            ordersById.put(oid, initialRecord)

            if chaos.autoFillCrossingOrders && shouldAutoFill(initialRecord, chaos.autoFillMidTolerancePct) then
              Right(executeFill(initialRecord, None))
            else
              Right(initialRecord)

  /**
   * Replaces an existing working order (`PUT /trader/v1/accounts/{accountHash}/orders/{orderId}`).
   */
  def replaceOrder(accountHash: String, orderId: Long, bodyJson: Json): Either[(Int, String), MockOrderRecord] =
    val chaos = chaosRef.get()
    if chaos.simulateRateLimit429 then
      Left((429, """{"message":"Too Many Requests - Simulated 429 rate limit from SchwabMock"}"""))
    else
      Option(ordersById.get(orderId)) match
        case None =>
          Left((404, s"""{"message":"Order $orderId not found"}"""))
        case Some(existing) =>
          if chaos.simulateFillBeforeReplaceRace && existing.status == "WORKING" then
            // Simulate the classic real-world race condition: order fills on the exchange 50ms before PUT arrives!
            executeFill(existing, None)
            Left((400, s"""{"message":"Order #$orderId cannot be replaced because its current status is FILLED"}"""))
          else if existing.status != "WORKING" && existing.status != "QUEUED" && existing.status != "PENDING_ACTIVATION" then
            Left((400, s"""{"message":"Order #$orderId cannot be replaced in state ${existing.status}"}"""))
          else
            val acc = resolveOrCreateAccount(accountHash)
            val c = bodyJson.hcursor
            val newOrderType = c.downField("orderType").as[String].getOrElse(existing.orderType).trim.toUpperCase
            val newPriceOpt  = c.downField("price").as[Double].toOption
            val newStopOpt   = c.downField("stopPrice").as[Double].toOption

            validateOrderRules(acc, newOrderType, newPriceOpt, newStopOpt, existing.legs, chaos, isReplacement = true) match
              case Some((code, errMsg)) =>
                Left((code, Json.obj("message" -> Json.fromString(errMsg)).noSpaces))
              case None =>
                val newOrderId = nextOrderId.getAndIncrement()
                val nowIso = DateTimeFormatter.ISO_INSTANT.format(Instant.now())
                val canceledOld = existing.copy(
                  status            = "REPLACED",
                  closeTime         = Some(nowIso),
                  replacedByOrderId = Some(newOrderId)
                )
                ordersById.put(orderId, canceledOld)

                val replacement = existing.copy(
                  orderId           = newOrderId,
                  orderType         = newOrderType,
                  price             = newPriceOpt,
                  stopPrice         = newStopOpt,
                  status            = "WORKING",
                  enteredTime       = nowIso,
                  closeTime         = None,
                  replacedByOrderId = None
                )
                ordersById.put(newOrderId, replacement)

                if chaos.autoFillCrossingOrders && shouldAutoFill(replacement, chaos.autoFillMidTolerancePct) then
                  Right(executeFill(replacement, None))
                else
                  Right(replacement)

  def cancelOrder(accountHash: String, orderId: Long): Either[(Int, String), MockOrderRecord] =
    Option(ordersById.get(orderId)) match
      case None =>
        Left((404, s"""{"message":"Order $orderId not found"}"""))
      case Some(existing) =>
        if existing.status == "FILLED" then
          Left((400, s"""{"message":"Cannot cancel order #$orderId: order is already FILLED"}"""))
        else
          val nowIso = DateTimeFormatter.ISO_INSTANT.format(Instant.now())
          val canceled = existing.copy(status = "CANCELED", closeTime = Some(nowIso))
          ordersById.put(orderId, canceled)
          Right(canceled)

  def manualFillOrder(orderId: Long, fillPriceOverrideOpt: Option[Double] = None): Option[MockOrderRecord] =
    Option(ordersById.get(orderId)).map { ord =>
      if ord.status == "FILLED" then ord
      else executeFill(ord, fillPriceOverrideOpt)
    }

  def manualRejectOrder(orderId: Long, reason: String = "Rejected by Mock Exchange operator"): Option[MockOrderRecord] =
    Option(ordersById.get(orderId)).map { ord =>
      val nowIso = DateTimeFormatter.ISO_INSTANT.format(Instant.now())
      val rejected = ord.copy(status = "REJECTED", closeTime = Some(nowIso), statusDescription = Some(reason))
      ordersById.put(orderId, rejected)
      rejected
    }

  def stepMatchingCycle(): Int =
    val chaos = chaosRef.get()
    if !chaos.autoFillCrossingOrders then 0
    else
      val working = ordersById.values().asScala.filter(_.status == "WORKING").toList
      var filledCount = 0
      working.foreach { ord =>
        if shouldAutoFill(ord, chaos.autoFillMidTolerancePct) then
          executeFill(ord, None)
          filledCount += 1
      }
      filledCount

  private def validateOrderRules(
    acc: MockAccountState,
    orderType: String,
    priceOpt: Option[Double],
    stopOpt: Option[Double],
    legs: List[MockOrderLeg],
    chaos: ChaosConfig,
    isReplacement: Boolean = false
  ): Option[(Int, String)] =
    val checkPriceOpt = priceOpt.orElse(stopOpt)
    if (orderType == "LIMIT" || orderType == "NET_CREDIT" || orderType == "NET_DEBIT") && priceOpt.isEmpty then
      Some((400, s"Order type $orderType requires a 'price' field"))
    else if orderType == "STOP" && stopOpt.isEmpty then
      Some((400, "Order type STOP requires a 'stopPrice' field"))
    else if checkPriceOpt.exists(_ <= 0.0) then
      Some((400, s"Order price must be strictly positive (got ${checkPriceOpt.getOrElse(0.0)})"))
    else if chaos.enforceTickSize && checkPriceOpt.exists(p => math.abs(p * 100.0 - math.round(p * 100.0)) > 1e-4) then
      Some((400, s"Invalid sub-penny tick increment on price ${checkPriceOpt.get}: Schwab requires at most 2 decimal places"))
    else if !isReplacement && chaos.simulateSpreadCloseOneLegReject && legs.size >= 2 && legs.exists(_.instruction.contains("CLOSE")) &&
      spreadRejectTriggeredOnce.putIfAbsent(legs.map(_.symbol).mkString("/"), java.lang.Boolean.TRUE) == null
    then
      Some((400, "Simulated Schwab multi-leg spread close rejection (first attempt rejected to test fallback)"))
    else if chaos.enforceBuyingPower && legs.size == 1 && legs.head.instruction.startsWith("BUY_TO_OPEN") then
      val leg = legs.head
      val mult = if leg.assetType == "OPTION" then 100.0 else 1.0
      val estPrice = priceOpt.getOrElse(computeQuote(leg.symbol).ask)
      val estCost = leg.quantity * estPrice * mult
      val maxBp = acc.cashBalance * 2.0
      if estCost > maxBp then
        Some((400, f"Insufficient buying power for ${leg.instruction} ${leg.quantity.toInt}x ${leg.symbol}: required $$$estCost%.2f exceeds buying power $$$maxBp%.2f"))
      else None
    else None

  private def shouldAutoFill(ord: MockOrderRecord, tolerancePct: Double): Boolean =
    ord.orderType match
      case "MARKET" => true
      case "LIMIT" if ord.legs.size == 1 =>
        val leg = ord.legs.head
        val q = computeQuote(leg.symbol)
        val lim = ord.price.getOrElse(0.0)
        if leg.instruction.startsWith("BUY") then
          lim >= q.midWithTolerance(isBuy = true, tolerancePct)
        else
          lim <= q.midWithTolerance(isBuy = false, tolerancePct)
      case "NET_CREDIT" if ord.legs.size >= 2 =>
        val netMidCredit = ord.legs.map { l =>
          val q = computeQuote(l.symbol)
          if l.instruction.startsWith("SELL") then q.mark else -q.mark
        }.sum
        ord.price.forall(reqCredit => reqCredit <= netMidCredit * (1.0 + tolerancePct))
      case "NET_DEBIT" if ord.legs.size >= 2 =>
        val netMidDebit = ord.legs.map { l =>
          val q = computeQuote(l.symbol)
          if l.instruction.startsWith("BUY") then q.mark else -q.mark
        }.sum
        ord.price.forall(reqDebit => reqDebit >= netMidDebit * (1.0 - tolerancePct))
      case "STOP" if ord.legs.size == 1 =>
        val leg = ord.legs.head
        val q = computeQuote(leg.symbol)
        val stp = ord.stopPrice.getOrElse(0.0)
        if leg.instruction.startsWith("SELL") then q.last <= stp
        else q.last >= stp
      case _ => false

  extension (q: ComputedQuote)
    private def midWithTolerance(isBuy: Boolean, tolPct: Double): Double =
      if isBuy then q.mark * (1.0 - tolPct)
      else q.mark * (1.0 + tolPct)

  private def executeFill(ord: MockOrderRecord, fillPriceOverrideOpt: Option[Double]): MockOrderRecord =
    val nowIso = DateTimeFormatter.ISO_INSTANT.format(Instant.now())
    val acc = resolveOrCreateAccount(ord.accountHash)
    var updatedCash = acc.cashBalance
    var updatedPositions = acc.positions

    val effOrderFillPrice = fillPriceOverrideOpt
      .orElse(ord.price)
      .orElse(ord.stopPrice)
      .getOrElse(computeQuote(ord.primarySymbol).mark)

    ord.legs.foreach { leg =>
      val q = computeQuote(leg.symbol)
      val legPrice =
        if ord.legs.size == 1 then effOrderFillPrice
        else q.mark
      val mult = if leg.assetType == "OPTION" then 100.0 else 1.0
      val notional = leg.quantity * legPrice * mult
      val isBuy = leg.instruction.startsWith("BUY")
      updatedCash = if isBuy then updatedCash - notional else updatedCash + notional

      val parsedOpt = ExchangeMatchingEngine.parseOccSymbol(leg.symbol)
      val existingOpt = updatedPositions.get(leg.symbol)
      val nextPosOpt: Option[MockPositionRecord] = (existingOpt, leg.instruction) match
        case (Some(pos), inst) if inst.startsWith("BUY_TO_CLOSE") =>
          val remShort = math.max(0.0, pos.shortQuantity - leg.quantity)
          if remShort <= 1e-6 && pos.longQuantity <= 1e-6 then None
          else Some(pos.copy(shortQuantity = remShort))
        case (Some(pos), inst) if inst.startsWith("SELL_TO_CLOSE") =>
          val remLong = math.max(0.0, pos.longQuantity - leg.quantity)
          if remLong <= 1e-6 && pos.shortQuantity <= 1e-6 then None
          else Some(pos.copy(longQuantity = remLong))
        case (Some(pos), inst) if inst.startsWith("BUY") =>
          val totQty = pos.longQuantity + leg.quantity
          val newAvg = if totQty > 0.0 then ((pos.longQuantity * pos.averagePrice) + (leg.quantity * legPrice)) / totQty else legPrice
          Some(pos.copy(longQuantity = totQty, averagePrice = round2(newAvg)))
        case (Some(pos), _) =>
          val totQty = pos.shortQuantity + leg.quantity
          val newAvg = if totQty > 0.0 then ((pos.shortQuantity * pos.averagePrice) + (leg.quantity * legPrice)) / totQty else legPrice
          Some(pos.copy(shortQuantity = totQty, averagePrice = round2(newAvg)))
        case (None, inst) =>
          val isShort = inst.startsWith("SELL")
          Some(
            MockPositionRecord(
              symbol           = leg.symbol,
              assetType        = leg.assetType,
              description      = leg.symbol,
              longQuantity     = if isShort then 0.0 else leg.quantity,
              shortQuantity    = if isShort then leg.quantity else 0.0,
              averagePrice     = round2(legPrice),
              underlyingSymbol = parsedOpt.map(_._1).orElse(Some(leg.symbol)),
              putCall          = parsedOpt.map(t => if t._3 then "CALL" else "PUT")
            )
          )

      updatedPositions = nextPosOpt match
        case Some(np) => updatedPositions.updated(leg.symbol, np)
        case None     => updatedPositions - leg.symbol

      val actId = nextActivityId.getAndIncrement()
      val txJson = Json.obj(
        "activityId"  -> Json.fromLong(actId),
        "time"        -> Json.fromString(nowIso),
        "tradeDate"   -> Json.fromString(nowIso),
        "type"        -> Json.fromString("TRADE"),
        "status"      -> Json.fromString("VALID"),
        "netAmount"   -> Json.fromDoubleOrNull(round2(if isBuy then -notional else notional)),
        "transferItems" -> Json.arr(
          Json.obj(
            "instrument" -> Json.obj(
              "symbol"    -> Json.fromString(leg.symbol),
              "assetType" -> Json.fromString(leg.assetType)
            ),
            "amount" -> Json.fromDoubleOrNull(if isBuy then leg.quantity else -leg.quantity),
            "cost"   -> Json.fromDoubleOrNull(round2(if isBuy then -notional else notional)),
            "price"  -> Json.fromDoubleOrNull(round2(legPrice)),
            "positionEffect" -> Json.fromString(if leg.instruction.endsWith("CLOSE") then "CLOSING" else "OPENING")
          )
        )
      )
      transactionsByAccount.compute(acc.hashValue, (_, prev) => Option(prev).getOrElse(Vector.empty) :+ txJson)
    }

    accountsByHash.put(acc.hashValue, acc.copy(cashBalance = round2(updatedCash), positions = updatedPositions))
    val filledOrder = ord.copy(
      status         = "FILLED",
      filledQuantity = ord.quantity,
      price          = Some(round2(effOrderFillPrice)),
      closeTime      = Some(nowIso)
    )
    ordersById.put(ord.orderId, filledOrder)
    filledOrder

  def computeQuote(symbol: String): ComputedQuote =
    val clean = symbol.trim.toUpperCase
    ExchangeMatchingEngine.parseOccSymbol(clean) match
      case Some((root, yymmdd, isCall, strike)) =>
        val spot = Option(underlyingPrices.get(root)).getOrElse(100.0)
        val today = LocalDate.now(EasternZone)
        val expDate = Try(LocalDate.parse(yymmdd, OccDateFmt)).getOrElse(today.plusDays(7))
        val dteDays = math.max(0L, ChronoUnit.DAYS.between(today, expDate))
        val tYears  = math.max(1.0 / 365.0, dteDays.toDouble / 365.0)
        val iv      = 0.28
        val (theo, delta, gamma, theta, vega) = blackScholes(spot, strike, tYears, 0.045, iv, isCall)
        val mark = math.max(0.05, round2(theo))
        val halfSpread = math.max(0.02, round2(mark * 0.015))
        val bid = math.max(0.01, round2(mark - halfSpread))
        val ask = round2(mark + halfSpread)
        ComputedQuote(
          symbol           = symbol,
          bid              = bid,
          ask              = ask,
          mark             = mark,
          last             = mark,
          openPrice        = mark,
          highPrice        = round2(mark * 1.08),
          lowPrice         = math.max(0.01, round2(mark * 0.92)),
          closePrice       = mark,
          netPercentChange = 1.25,
          delta            = round4(delta),
          gamma            = round4(gamma),
          theta            = round4(theta),
          vega             = round4(vega),
          volatility       = iv
        )
      case None =>
        val spot = Option(underlyingPrices.get(clean)).getOrElse(100.0)
        val halfSpread = if spot >= 200.0 then 0.02 else 0.01
        val bid = round2(spot - halfSpread)
        val ask = round2(spot + halfSpread)
        ComputedQuote(
          symbol           = symbol,
          bid              = bid,
          ask              = ask,
          mark             = round2(spot),
          last             = round2(spot),
          openPrice        = round2(spot * 0.995),
          highPrice        = round2(spot * 1.008),
          lowPrice         = round2(spot * 0.991),
          closePrice       = round2(spot * 0.996),
          netPercentChange = 0.40,
          delta            = 1.0,
          gamma            = 0.0,
          theta            = 0.0,
          vega             = 0.0,
          volatility       = 0.22
        )

  def generateOptionChainJson(symbol: String, contractTypeOpt: Option[String]): Json =
    val cleanSym = symbol.trim.toUpperCase
    val spot = Option(underlyingPrices.get(cleanSym)).getOrElse(100.0)
    val step =
      if spot >= 400.0 then 5.0
      else if spot >= 100.0 then 2.5
      else 1.0
    val centerStrike = math.round(spot / step) * step
    val strikes = (-8 to 8).map(i => centerStrike + i * step).filter(_ > 0.0).toList
    val today = LocalDate.now(EasternZone)
    val dtes = List(0, 2, 7, 14, 30, 45, 180)

    def buildExpMap(isCall: Boolean): Json =
      val expEntries = dtes.map { dte =>
        val expDate = today.plusDays(dte.toLong)
        val expKey  = s"${expDate.toString}:$dte"
        val yymmdd  = expDate.format(OccDateFmt)
        val cpChar  = if isCall then "C" else "P"
        val strikeFields = strikes.map { k =>
          val strikeStr = f"$k%.1f"
          val occStrike = f"${math.round(k * 1000.0)}%08d"
          val paddedRoot = f"$cleanSym%-6s"
          val occSymbol = s"$paddedRoot$yymmdd$cpChar$occStrike"
          val q = computeQuote(occSymbol)
          strikeStr -> Json.arr(
            Json.obj(
              "putCall"          -> Json.fromString(if isCall then "CALL" else "PUT"),
              "symbol"           -> Json.fromString(occSymbol),
              "description"      -> Json.fromString(s"$cleanSym ${expDate.toString} $strikeStr ${if isCall then "Call" else "Put"}"),
              "bid"              -> Json.fromDoubleOrNull(q.bid),
              "ask"              -> Json.fromDoubleOrNull(q.ask),
              "last"             -> Json.fromDoubleOrNull(q.last),
              "mark"             -> Json.fromDoubleOrNull(q.mark),
              "totalVolume"      -> Json.fromInt(850),
              "openInterest"     -> Json.fromInt(2400),
              "volatility"       -> Json.fromDoubleOrNull(q.volatility * 100.0),
              "delta"            -> Json.fromDoubleOrNull(q.delta),
              "gamma"            -> Json.fromDoubleOrNull(q.gamma),
              "theta"            -> Json.fromDoubleOrNull(q.theta),
              "vega"             -> Json.fromDoubleOrNull(q.vega),
              "strikePrice"      -> Json.fromDoubleOrNull(k),
              "expirationDate"   -> Json.fromString(s"${expDate.toString}T20:00:00.000+00:00"),
              "daysToExpiration" -> Json.fromInt(dte),
              "multiplier"       -> Json.fromDoubleOrNull(100.0),
              "inTheMoney"       -> Json.fromBoolean(if isCall then spot > k else spot < k)
            )
          )
        }
        expKey -> Json.obj(strikeFields*)
      }
      Json.obj(expEntries*)

    val ct = contractTypeOpt.map(_.trim.toUpperCase).getOrElse("ALL")
    Json.obj(
      "symbol"          -> Json.fromString(cleanSym),
      "status"          -> Json.fromString("SUCCESS"),
      "underlyingPrice" -> Json.fromDoubleOrNull(spot),
      "volatility"      -> Json.fromDoubleOrNull(24.0),
      "underlying"      -> Json.obj(
        "symbol" -> Json.fromString(cleanSym),
        "mark"   -> Json.fromDoubleOrNull(spot),
        "last"   -> Json.fromDoubleOrNull(spot),
        "bid"    -> Json.fromDoubleOrNull(round2(spot - 0.02)),
        "ask"    -> Json.fromDoubleOrNull(round2(spot + 0.02))
      ),
      "callExpDateMap"  -> (if ct == "PUT" then Json.obj() else buildExpMap(isCall = true)),
      "putExpDateMap"   -> (if ct == "CALL" then Json.obj() else buildExpMap(isCall = false))
    )

  def generatePriceHistoryJson(symbol: String): Json =
    val clean = symbol.trim.toUpperCase
    val baseSpot = Option(underlyingPrices.get(clean)).getOrElse(100.0)
    val nowMs = System.currentTimeMillis()
    val candles = (0 until 60).map { i =>
      val ts = nowMs - (60 - i) * 60000L
      val wave = math.sin(i.toDouble / 6.0) * (baseSpot * 0.003)
      val c = round2(baseSpot + wave)
      val o = round2(c - 0.15)
      val h = round2(math.max(o, c) + 0.25)
      val l = round2(math.min(o, c) - 0.20)
      Json.obj(
        "open"     -> Json.fromDoubleOrNull(o),
        "high"     -> Json.fromDoubleOrNull(h),
        "low"      -> Json.fromDoubleOrNull(l),
        "close"    -> Json.fromDoubleOrNull(c),
        "volume"   -> Json.fromLong(45000L + (i * 350L)),
        "datetime" -> Json.fromLong(ts)
      )
    }
    Json.obj(
      "symbol"  -> Json.fromString(clean),
      "empty"   -> Json.fromBoolean(false),
      "candles" -> Json.fromValues(candles)
    )

  private def blackScholes(
    spot: Double,
    strike: Double,
    tYears: Double,
    r: Double,
    sigma: Double,
    isCall: Boolean
  ): (Double, Double, Double, Double, Double) =
    val sqrtT = math.sqrt(math.max(1e-6, tYears))
    val d1 = (math.log(spot / strike) + (r + 0.5 * sigma * sigma) * tYears) / (sigma * sqrtT)
    val d2 = d1 - sigma * sqrtT
    val nd1 = normCdf(d1)
    val nd2 = normCdf(d2)
    val npd1 = normPdf(d1)
    val disc = math.exp(-r * tYears)
    val price =
      if isCall then spot * nd1 - strike * disc * nd2
      else strike * disc * normCdf(-d2) - spot * normCdf(-d1)
    val delta = if isCall then nd1 else nd1 - 1.0
    val gamma = npd1 / (spot * sigma * sqrtT)
    val vega  = (spot * npd1 * sqrtT) / 100.0
    val theta = (-(spot * npd1 * sigma) / (2.0 * sqrtT)) / 365.0
    (math.max(0.05, price), delta, gamma, theta, vega)

  private def normPdf(x: Double): Double =
    math.exp(-0.5 * x * x) / math.sqrt(2.0 * math.Pi)

  private def normCdf(x: Double): Double =
    val a1 =  0.254829592
    val a2 = -0.284496736
    val a3 =  1.421413741
    val a4 = -1.453152027
    val a5 =  1.061405429
    val p  =  0.3275911
    val sign = if x < 0 then -1.0 else 1.0
    val absX = math.abs(x) / math.sqrt(2.0)
    val t = 1.0 / (1.0 + p * absX)
    val y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * math.exp(-absX * absX)
    0.5 * (1.0 + sign * y)

  private def round2(v: Double): Double =
    BigDecimal(v).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

  private def round4(v: Double): Double =
    BigDecimal(v).setScale(4, BigDecimal.RoundingMode.HALF_UP).toDouble

object ExchangeMatchingEngine:
  private val OccRegex = """^([A-Z0-9]+?)\s*(\d{6})([CP])(\d{8})$""".r

  def parseOccSymbol(sym: String): Option[(String, String, Boolean, Double)] =
    sym.trim.toUpperCase match
      case OccRegex(root, yymmdd, cp, strikeStr) =>
        Try(strikeStr.toDouble / 1000.0).toOption.map(strike => (root, yymmdd, cp == "C", strike))
      case _ => None
