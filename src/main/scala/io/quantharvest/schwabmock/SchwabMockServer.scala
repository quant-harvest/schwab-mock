package io.quantharvest.schwabmock

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.syntax.all.*
import com.comcast.ip4s.*
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*
import org.http4s.*
import org.http4s.dsl.io.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.`Content-Type`
import org.typelevel.ci.CIString

object SchwabMockServer:

  private val jsonHeader = Header.Raw(CIString("Content-Type"), "application/json")
  private val htmlHeader = Header.Raw(CIString("Content-Type"), "text/html; charset=utf-8")

  def routes(engine: ExchangeMatchingEngine): HttpRoutes[IO] =
    HttpRoutes.of[IO] {

      // =========================================================================
      // 1. OAuth 2.0 Token Endpoint
      // =========================================================================
      case POST -> Root / "v1" / "oauth" / "token" | POST -> Root / "oauth" / "token" =>
        val nowMs = System.currentTimeMillis()
        val resp = Json.obj(
          "access_token"                -> Json.fromString(s"MOCK_ACCESS_TOKEN_$nowMs"),
          "refresh_token"               -> Json.fromString(s"MOCK_REFRESH_TOKEN_$nowMs"),
          "token_type"                  -> Json.fromString("Bearer"),
          "expires_in"                  -> Json.fromLong(1800L),
          "authenticated_at_ms"         -> Json.fromLong(nowMs),
          "access_token_expires_at_ms"  -> Json.fromLong(nowMs + 1800000L),
          "refresh_token_expires_at_ms" -> Json.fromLong(nowMs + 7L * 86400000L)
        )
        Ok(resp.noSpaces, jsonHeader)

      // =========================================================================
      // 2. Trader v1 Account & Order Endpoints (mounted at /trader/v1 and /)
      // =========================================================================
      case GET -> Root / "trader" / "v1" / "accounts" / "accountNumbers" |
           GET -> Root / "accounts" / "accountNumbers" =>
        if engine.getChaosConfig.simulateRateLimit429 then
          TooManyRequests("""{"message":"Simulated 429 Too Many Requests"}""", jsonHeader)
        else
          Ok(Json.fromValues(engine.listAccountHashes).noSpaces, jsonHeader)

      case GET -> Root / "trader" / "v1" / "userPreference" |
           GET -> Root / "userPreference" =>
        Ok(engine.getUserPreferencesJson.noSpaces, jsonHeader)

      case GET -> Root / "trader" / "v1" / "accounts" / accountHash =>
        if engine.getChaosConfig.simulateRateLimit429 then
          TooManyRequests("""{"message":"Simulated 429 Too Many Requests"}""", jsonHeader)
        else
          Ok(engine.getAccountDetailsJson(accountHash).noSpaces, jsonHeader)

      case GET -> Root / "accounts" / accountHash =>
        if engine.getChaosConfig.simulateRateLimit429 then
          TooManyRequests("""{"message":"Simulated 429 Too Many Requests"}""", jsonHeader)
        else
          Ok(engine.getAccountDetailsJson(accountHash).noSpaces, jsonHeader)

      case req @ GET -> Root / "trader" / "v1" / "accounts" / accountHash / "orders" =>
        handleGetOrders(engine, Some(accountHash), req)

      case req @ GET -> Root / "accounts" / accountHash / "orders" =>
        handleGetOrders(engine, Some(accountHash), req)

      case req @ POST -> Root / "trader" / "v1" / "accounts" / accountHash / "orders" =>
        handlePlaceOrder(engine, accountHash, req)

      case req @ POST -> Root / "accounts" / accountHash / "orders" =>
        handlePlaceOrder(engine, accountHash, req)

      case req @ PUT -> Root / "trader" / "v1" / "accounts" / accountHash / "orders" / LongVar(orderId) =>
        handleReplaceOrder(engine, accountHash, orderId, req)

      case req @ PUT -> Root / "accounts" / accountHash / "orders" / LongVar(orderId) =>
        handleReplaceOrder(engine, accountHash, orderId, req)

      case DELETE -> Root / "trader" / "v1" / "accounts" / accountHash / "orders" / LongVar(orderId) =>
        handleCancelOrder(engine, accountHash, orderId)

      case DELETE -> Root / "accounts" / accountHash / "orders" / LongVar(orderId) =>
        handleCancelOrder(engine, accountHash, orderId)

      case GET -> Root / "trader" / "v1" / "accounts" / accountHash / "transactions" =>
        Ok(Json.fromValues(engine.listTransactionsJson(accountHash)).noSpaces, jsonHeader)

      case GET -> Root / "accounts" / accountHash / "transactions" =>
        Ok(Json.fromValues(engine.listTransactionsJson(accountHash)).noSpaces, jsonHeader)

      // =========================================================================
      // 3. Market Data v1 Endpoints (mounted at /marketdata/v1 and /)
      // =========================================================================
      case req @ GET -> Root / "marketdata" / "v1" / "chains" =>
        handleOptionChains(engine, req)

      case req @ GET -> Root / "chains" =>
        handleOptionChains(engine, req)

      case req @ GET -> Root / "marketdata" / "v1" / "quotes" =>
        handleQuotes(engine, req)

      case req @ GET -> Root / "quotes" =>
        handleQuotes(engine, req)

      case req @ GET -> Root / "marketdata" / "v1" / "pricehistory" =>
        handlePriceHistory(engine, req)

      case req @ GET -> Root / "marketdata" / "v1" / "marketdata" / "v1" / "pricehistory" =>
        handlePriceHistory(engine, req)

      case req @ GET -> Root / "pricehistory" =>
        handlePriceHistory(engine, req)

      // =========================================================================
      // 4. Mock Exchange Admin, Chaos & Dashboard Routes
      // =========================================================================
      case GET -> Root =>
        Ok(dashboardHtml, htmlHeader)

      case GET -> Root / "health" =>
        Ok("""{"status":"ok","service":"schwab-mock"}""", jsonHeader)

      case GET -> Root / "api" / "mock" / "state" =>
        val accounts = engine.listAccountHashes.flatMap { h =>
          h.hcursor.downField("hashValue").as[String].toOption.map(engine.getAccountDetailsJson)
        }
        val orders = engine.listOrders(None, None).map(_.toSchwabJson)
        val stateJson = Json.obj(
          "chaos"            -> engine.getChaosConfig.asJson,
          "underlyingPrices" -> engine.getUnderlyingPrices.asJson,
          "accounts"         -> Json.fromValues(accounts),
          "orders"           -> Json.fromValues(orders)
        )
        Ok(stateJson.noSpaces, jsonHeader)

      case req @ POST -> Root / "api" / "mock" / "chaos" =>
        req.as[String].flatMap { body =>
          io.circe.parser.decode[ChaosConfig](body) match
            case Right(cfg) =>
              val updated = engine.setChaosConfig(cfg)
              Ok(Json.obj("status" -> Json.fromString("ok"), "chaos" -> updated.asJson).noSpaces, jsonHeader)
            case Left(err) =>
              BadRequest(Json.obj("status" -> Json.fromString("error"), "error" -> Json.fromString(err.getMessage)).noSpaces, jsonHeader)
        }

      case req @ POST -> Root / "api" / "mock" / "prices" =>
        req.as[String].flatMap { body =>
          parse(body) match
            case Right(json) =>
              val c = json.hcursor
              for
                sym <- c.downField("symbol").as[String].toOption
                pr  <- c.downField("price").as[Double].toOption
              do engine.setUnderlyingPrice(sym, pr)
              Ok(Json.obj("status" -> Json.fromString("ok"), "underlyingPrices" -> engine.getUnderlyingPrices.asJson).noSpaces, jsonHeader)
            case Left(err) =>
              BadRequest(Json.obj("error" -> Json.fromString(err.message)).noSpaces, jsonHeader)
        }

      case req @ POST -> Root / "api" / "mock" / "orders" / LongVar(orderId) / "fill" =>
        req.as[String].flatMap { body =>
          val priceOverride = parse(body).toOption.flatMap(_.hcursor.downField("price").as[Double].toOption)
          engine.manualFillOrder(orderId, priceOverride) match
            case Some(filled) =>
              Ok(Json.obj("status" -> Json.fromString("ok"), "order" -> filled.toSchwabJson).noSpaces, jsonHeader)
            case None =>
              NotFound(Json.obj("error" -> Json.fromString(s"Order $orderId not found")).noSpaces, jsonHeader)
        }

      case req @ POST -> Root / "api" / "mock" / "orders" / LongVar(orderId) / "reject" =>
        req.as[String].flatMap { body =>
          val reason = parse(body).toOption.flatMap(_.hcursor.downField("reason").as[String].toOption).getOrElse("Rejected by Mock Exchange operator")
          engine.manualRejectOrder(orderId, reason) match
            case Some(rej) =>
              Ok(Json.obj("status" -> Json.fromString("ok"), "order" -> rej.toSchwabJson).noSpaces, jsonHeader)
            case None =>
              NotFound(Json.obj("error" -> Json.fromString(s"Order $orderId not found")).noSpaces, jsonHeader)
        }

      case POST -> Root / "api" / "mock" / "orders" / LongVar(orderId) / "cancel" =>
        engine.cancelOrder("ANY", orderId) match
          case Right(canc) =>
            Ok(Json.obj("status" -> Json.fromString("ok"), "order" -> canc.toSchwabJson).noSpaces, jsonHeader)
          case Left((code, msg)) =>
            Response[IO](Status.fromInt(code).getOrElse(Status.BadRequest))
              .withEntity(msg)
              .putHeaders(jsonHeader)
              .pure[IO]

      case POST -> Root / "api" / "mock" / "reset" =>
        engine.resetToDefaults()
        Ok("""{"status":"ok","message":"Mock Exchange reset to default accounts and prices"}""", jsonHeader)
    }

  private def handleGetOrders(engine: ExchangeMatchingEngine, accountHashOpt: Option[String], req: Request[IO]): IO[Response[IO]] =
    if engine.getChaosConfig.simulateRateLimit429 then
      TooManyRequests("""{"message":"Simulated 429 Too Many Requests"}""", jsonHeader)
    else
      val statusOpt = req.params.get("status").map(_.trim).filter(_.nonEmpty)
      val orders = engine.listOrders(accountHashOpt, statusOpt).map(_.toSchwabJson)
      Ok(Json.fromValues(orders).noSpaces, jsonHeader)

  private def handlePlaceOrder(engine: ExchangeMatchingEngine, accountHash: String, req: Request[IO]): IO[Response[IO]] =
    req.as[String].flatMap { bodyStr =>
      parse(bodyStr) match
        case Left(err) =>
          BadRequest(Json.obj("message" -> Json.fromString(s"Malformed order JSON: ${err.message}")).noSpaces, jsonHeader)
        case Right(json) =>
          engine.placeOrder(accountHash, json) match
            case Left((code, errBody)) =>
              Response[IO](Status.fromInt(code).getOrElse(Status.BadRequest))
                .withEntity(errBody)
                .putHeaders(jsonHeader)
                .pure[IO]
            case Right(orderRec) =>
              val locHeader = Header.Raw(CIString("Location"), s"/trader/v1/accounts/$accountHash/orders/${orderRec.orderId}")
              Created(orderRec.toSchwabJson.noSpaces, jsonHeader, locHeader)
    }

  private def handleReplaceOrder(engine: ExchangeMatchingEngine, accountHash: String, orderId: Long, req: Request[IO]): IO[Response[IO]] =
    req.as[String].flatMap { bodyStr =>
      parse(bodyStr) match
        case Left(err) =>
          BadRequest(Json.obj("message" -> Json.fromString(s"Malformed replacement JSON: ${err.message}")).noSpaces, jsonHeader)
        case Right(json) =>
          engine.replaceOrder(accountHash, orderId, json) match
            case Left((code, errBody)) =>
              Response[IO](Status.fromInt(code).getOrElse(Status.BadRequest))
                .withEntity(errBody)
                .putHeaders(jsonHeader)
                .pure[IO]
            case Right(newOrderRec) =>
              val locHeader = Header.Raw(CIString("Location"), s"/trader/v1/accounts/$accountHash/orders/${newOrderRec.orderId}")
              Ok(newOrderRec.toSchwabJson.noSpaces, jsonHeader, locHeader)
    }

  private def handleCancelOrder(engine: ExchangeMatchingEngine, accountHash: String, orderId: Long): IO[Response[IO]] =
    engine.cancelOrder(accountHash, orderId) match
      case Left((code, errBody)) =>
        Response[IO](Status.fromInt(code).getOrElse(Status.BadRequest))
          .withEntity(errBody)
          .putHeaders(jsonHeader)
          .pure[IO]
      case Right(_) =>
        Ok("", jsonHeader)

  private def handleOptionChains(engine: ExchangeMatchingEngine, req: Request[IO]): IO[Response[IO]] =
    if engine.getChaosConfig.simulateRateLimit429 then
      TooManyRequests("""{"message":"Simulated 429 Too Many Requests"}""", jsonHeader)
    else
      val sym = req.params.getOrElse("symbol", "SPY")
      val contractTypeOpt = req.params.get("contractType")
      Ok(engine.generateOptionChainJson(sym, contractTypeOpt).noSpaces, jsonHeader)

  private def handleQuotes(engine: ExchangeMatchingEngine, req: Request[IO]): IO[Response[IO]] =
    if engine.getChaosConfig.simulateRateLimit429 then
      TooManyRequests("""{"message":"Simulated 429 Too Many Requests"}""", jsonHeader)
    else
      val rawSyms = req.params.getOrElse("symbols", "SPY")
      val symbols = rawSyms.split(',').map(_.trim).filter(_.nonEmpty).toList
      val entries = symbols.map { s =>
        val q = engine.computeQuote(s)
        s -> q.toSchwabQuoteEntryJson
      }
      Ok(Json.obj(entries*).noSpaces, jsonHeader)

  private def handlePriceHistory(engine: ExchangeMatchingEngine, req: Request[IO]): IO[Response[IO]] =
    val sym = req.params.getOrElse("symbol", "SPY")
    Ok(engine.generatePriceHistoryJson(sym).noSpaces, jsonHeader)

  private val dashboardHtml: String =
    """<!DOCTYPE html>
      |<html lang="en">
      |<head>
      |  <meta charset="UTF-8">
      |  <title>🏛️ QuantHarvest — Schwab Mock Exchange & Chaos Console</title>
      |  <style>
      |    :root { --bg:#090d16; --card:#111827; --border:#1f2937; --text:#e2e8f0; --muted:#94a3b8; --accent:#38bdf8; --green:#22c55e; --red:#ef4444; --amber:#f59e0b; }
      |    body { margin:0; font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif; background:var(--bg); color:var(--text); padding:20px 28px; }
      |    h1 { margin:0; font-size:20px; display:flex; align-items:center; gap:10px; }
      |    .card { background:var(--card); border:1px solid var(--border); border-radius:10px; padding:16px 20px; margin-bottom:16px; }
      |    .grid { display:grid; grid-template-columns:1fr 1fr; gap:16px; }
      |    .btn { background:#1e293b; color:var(--text); border:1px solid #334155; border-radius:6px; padding:6px 12px; font-size:12px; font-weight:700; cursor:pointer; }
      |    .btn:hover { border-color:var(--accent); }
      |    .btn.green { background:rgba(34,197,94,0.15); color:#4ade80; border-color:rgba(34,197,94,0.4); }
      |    .btn.red { background:rgba(239,68,68,0.15); color:#f87171; border-color:rgba(239,68,68,0.4); }
      |    table { width:100%; border-collapse:collapse; font-size:12px; }
      |    th, td { padding:8px 10px; border-bottom:1px solid var(--border); text-align:left; }
      |    th { color:var(--muted); font-size:11px; text-transform:uppercase; }
      |    code { font-family:monospace; color:var(--accent); }
      |    .toggle-row { display:flex; justify-content:space-between; align-items:center; padding:6px 0; border-bottom:1px solid rgba(255,255,255,0.05); font-size:13px; }
      |  </style>
      |</head>
      |<body>
      |  <div style="display:flex; justify-content:space-between; align-items:center; margin-bottom:18px;">
      |    <div>
      |      <h1>🏛️ QuantHarvest — Charles Schwab Wire-Level Mock Exchange</h1>
      |      <div style="font-size:12px; color:var(--muted); margin-top:4px;">
      |        Trader v1: <code>/trader/v1/accounts/...</code> &nbsp;|&nbsp; Market Data v1: <code>/marketdata/v1/chains, /quotes</code> &nbsp;|&nbsp; OAuth2: <code>/v1/oauth/token</code>
      |      </div>
      |    </div>
      |    <div style="display:flex; gap:10px;">
      |      <button class="btn" onclick="refreshState()">🔄 Refresh</button>
      |      <button class="btn red" onclick="resetExchange()">🗑️ Reset Exchange State</button>
      |    </div>
      |  </div>
      |
      |  <div class="grid">
      |    <div class="card">
      |      <h3 style="margin:0 0 10px 0; font-size:14px;">⚡ Chaos & Wire-Validation Controls</h3>
      |      <div class="toggle-row"><label>Enforce Sub-Penny Tick Rejections (HTTP 400)</label><input type="checkbox" id="chkTick" onchange="saveChaos()"></div>
      |      <div class="toggle-row"><label>Enforce Buying Power Checks (HTTP 400)</label><input type="checkbox" id="chkBp" onchange="saveChaos()"></div>
      |      <div class="toggle-row"><label>Auto-Fill Crossing Limit & Spread Orders</label><input type="checkbox" id="chkAutoFill" onchange="saveChaos()"></div>
      |      <div class="toggle-row"><label style="color:#fbbf24;">🔥 Simulate Fill-Before-Replace Race (PUT returns 400 &amp; fills)</label><input type="checkbox" id="chkRace" onchange="saveChaos()"></div>
      |      <div class="toggle-row"><label style="color:#fbbf24;">🔥 Simulate 2-Leg Spread Close First-Attempt Reject (HTTP 400)</label><input type="checkbox" id="chkSpreadRej" onchange="saveChaos()"></div>
      |      <div class="toggle-row"><label style="color:#f87171;">🚨 Simulate Schwab HTTP 429 Rate-Limit Burst</label><input type="checkbox" id="chk429" onchange="saveChaos()"></div>
      |    </div>
      |
      |    <div class="card">
      |      <h3 style="margin:0 0 10px 0; font-size:14px;">📈 Live Underlying Spot Tape (Click to Override)</h3>
      |      <div id="pricesGrid" style="display:grid; grid-template-columns:repeat(4, 1fr); gap:8px;"></div>
      |    </div>
      |  </div>
      |
      |  <div class="card">
      |    <h3 style="margin:0 0 10px 0; font-size:14px;">📋 Exchange Order Book (WORKING / FILLED / REPLACED / REJECTED)</h3>
      |    <table>
      |      <thead>
      |        <tr><th>Order ID</th><th>Account</th><th>Status</th><th>Strategy / Type</th><th>Legs</th><th>Price / Stop</th><th>Entered</th><th style="text-align:right;">Manual Action</th></tr>
      |      </thead>
      |      <tbody id="ordersBody"></tbody>
      |    </table>
      |  </div>
      |
      |  <script>
      |    function refreshState() {
      |      fetch('/api/mock/state').then(r => r.json()).then(data => {
      |        const c = data.chaos || {};
      |        document.getElementById('chkTick').checked = !!c.enforceTickSize;
      |        document.getElementById('chkBp').checked = !!c.enforceBuyingPower;
      |        document.getElementById('chkAutoFill').checked = !!c.autoFillCrossingOrders;
      |        document.getElementById('chkRace').checked = !!c.simulateFillBeforeReplaceRace;
      |        document.getElementById('chkSpreadRej').checked = !!c.simulateSpreadCloseOneLegReject;
      |        document.getElementById('chk429').checked = !!c.simulateRateLimit429;
      |
      |        const prices = data.underlyingPrices || {};
      |        const pg = document.getElementById('pricesGrid');
      |        pg.innerHTML = Object.keys(prices).sort().map(sym =>
      |          '<div style="background:#0f172a; border:1px solid #1e293b; border-radius:6px; padding:6px 10px; cursor:pointer;" onclick="promptPrice(\'' + sym + '\',' + prices[sym] + ')">' +
      |            '<div style="font-size:11px; color:#94a3b8; font-weight:700;">' + sym + '</div>' +
      |            '<div style="font-size:14px; font-family:monospace; color:#38bdf8; font-weight:800;">$' + Number(prices[sym]).toFixed(2) + '</div>' +
      |          '</div>'
      |        ).join('');
      |
      |        const orders = data.orders || [];
      |        const ob = document.getElementById('ordersBody');
      |        if (orders.length === 0) {
      |          ob.innerHTML = '<tr><td colspan="8" style="text-align:center; color:#64748b; padding:16px;">No orders submitted yet. Point QuantHarvest SCHWAB_BASE_URL at this server to stream orders.</td></tr>';
      |        } else {
      |          ob.innerHTML = orders.map(o => {
      |            const legsStr = (o.orderLegCollection || []).map(l => l.instruction + ' ' + l.quantity + 'x ' + (l.instrument && l.instrument.symbol)).join(' / ');
      |            const prStr = o.price ? ('$' + Number(o.price).toFixed(2)) : (o.stopPrice ? ('STOP $' + Number(o.stopPrice).toFixed(2)) : 'MKT');
      |            const stColor = o.status === 'FILLED' ? '#4ade80' : (o.status === 'WORKING' ? '#38bdf8' : (o.status === 'REJECTED' ? '#f87171' : '#94a3b8'));
      |            const actions = o.status === 'WORKING'
      |              ? '<button class="btn green" onclick="orderAction(' + o.orderId + ',\'fill\')">✓ Fill</button> ' +
      |                '<button class="btn red" onclick="orderAction(' + o.orderId + ',\'reject\')">✕ Reject</button>'
      |              : '';
      |            return '<tr>' +
      |              '<td style="font-family:monospace;">#' + o.orderId + '</td>' +
      |              '<td style="font-family:monospace;">' + (o.accountNumber || '') + '</td>' +
      |              '<td><strong style="color:' + stColor + ';">' + o.status + '</strong></td>' +
      |              '<td>' + (o.complexOrderStrategyType || o.orderStrategyType) + ' · ' + o.orderType + '</td>' +
      |              '<td style="font-family:monospace;">' + legsStr + '</td>' +
      |              '<td style="font-family:monospace;">' + prStr + '</td>' +
      |              '<td style="color:#94a3b8; font-size:11px;">' + (o.enteredTime || '') + '</td>' +
      |              '<td style="text-align:right;">' + actions + '</td>' +
      |            '</tr>';
      |          }).join('');
      |        }
      |      });
      |    }
      |    function saveChaos() {
      |      const payload = {
      |        enforceTickSize: document.getElementById('chkTick').checked,
      |        enforceBuyingPower: document.getElementById('chkBp').checked,
      |        autoFillCrossingOrders: document.getElementById('chkAutoFill').checked,
      |        autoFillMidTolerancePct: 0.015,
      |        simulateFillBeforeReplaceRace: document.getElementById('chkRace').checked,
      |        simulateSpreadCloseOneLegReject: document.getElementById('chkSpreadRej').checked,
      |        simulateRateLimit429: document.getElementById('chk429').checked
      |      };
      |      fetch('/api/mock/chaos', { method:'POST', headers:{'Content-Type':'application/json'}, body:JSON.stringify(payload) }).then(() => refreshState());
      |    }
      |    function promptPrice(sym, cur) {
      |      const next = prompt('Set underlying spot price for ' + sym + ':', cur);
      |      if (next && !isNaN(parseFloat(next))) {
      |        fetch('/api/mock/prices', { method:'POST', headers:{'Content-Type':'application/json'}, body:JSON.stringify({symbol:sym, price:parseFloat(next)}) }).then(() => refreshState());
      |      }
      |    }
      |    function orderAction(oid, act) {
      |      fetch('/api/mock/orders/' + oid + '/' + act, { method:'POST', headers:{'Content-Type':'application/json'}, body:'{}' }).then(() => refreshState());
      |    }
      |    function resetExchange() {
      |      if (!confirm('Reset all mock exchange accounts, positions, and orders to defaults?')) return;
      |      fetch('/api/mock/reset', { method:'POST' }).then(() => refreshState());
      |    }
      |    refreshState();
      |    setInterval(refreshState, 3000);
      |  </script>
      |</body>
      |</html>""".stripMargin

object SchwabMockApp extends IOApp:
  override def run(args: List[String]): IO[ExitCode] =
    val portNum = sys.env.get("PORT").flatMap(_.toIntOption).getOrElse(8095)
    val engine  = new ExchangeMatchingEngine()
    val serverRes: Resource[IO, org.http4s.server.Server] =
      EmberServerBuilder
        .default[IO]
        .withHost(host"0.0.0.0")
        .withPort(Port.fromInt(portNum).getOrElse(port"8095"))
        .withHttpApp(SchwabMockServer.routes(engine).orNotFound)
        .build

    serverRes.use { _ =>
      IO.println(s"🏛️ [SchwabMock] Wire-Level Charles Schwab Mock Exchange listening on http://localhost:$portNum") >>
        IO.never
    }.as(ExitCode.Success)
