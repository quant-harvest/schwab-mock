# 🏛️ QuantHarvest Schwab Mock Exchange (`schwab-mock`)

Wire-level Charles Schwab REST & Market Data Mock Exchange for QuantHarvest (`app`).

Unlike in-memory paper trading (`PaperTradingSchwabClient`), `schwab-mock` runs as an independent HTTP microservice implementing the real Charles Schwab Trader v1 (`/trader/v1`), Market Data v1 (`/marketdata/v1`), and OAuth2 (`/v1/oauth/token`) wire contracts. This allows QuantHarvest's unmodified `LiveSchwabClient`, `OrderRunner` cancel/replace loop, `Location` header parser, and rate-limit pacer to be tested end-to-end over HTTP—including injectable chaos modes for production failure scenarios.

## 🚀 Quick Start

```bash
# Run the Mock Exchange on Port 8095 (default)
sbt run

# Run the unit test suite
sbt test
```

Open the **Mock Exchange & Chaos Console** at **[http://localhost:8095](http://localhost:8095)**.

## 🔌 Connecting QuantHarvest (`app`) to `schwab-mock`

```bash
SCHWAB_BASE_URL=http://localhost:8095/trader/v1 \
SCHWAB_MARKET_DATA_BASE_URL=http://localhost:8095/marketdata/v1 \
SCHWAB_TOKEN_URL=http://localhost:8095/v1/oauth/token \
DISABLE_TLS=true sbt run
```
