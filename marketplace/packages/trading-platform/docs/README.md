# Trading Platform

**High-performance trading platform with RDF ontology, order book management, and FIX protocol support**

## Features

- **RDF Ontology (300+ lines)**: Complete trading domain model
- **SPARQL Templates (15+ queries)**: Order book queries, market data, FIX messages
- **Rust Matching Engine**: <1ms latency, 100K orders/sec throughput
- **TypeScript WebSocket Client**: Real-time market data and order management
- **Python Backtesting**: Strategy testing with technical indicators

## Quick Start

### Rust - Matching Engine

```rust
let mut engine = MatchingEngine::new();
engine.add_instrument("AAPL".to_string());

let order = Order {
    order_id: "ORD-001".to_string(),
    symbol: "AAPL".to_string(),
    side: Side::Buy,
    order_type: OrderType::Limit,
    quantity: Decimal::new(100, 0),
    price: Some(Decimal::new(15000, 2)),
    // ...
};

let fills = engine.submit_order(order)?;
```

### TypeScript - WebSocket Client

```typescript
const client = new TradingClient(wsUrl, apiKey);
await client.connect();

client.on('quote', (quote) => {
  console.log(`${quote.symbol}: Bid ${quote.bidPrice} Ask ${quote.askPrice}`);
});

client.subscribeMarketData('AAPL', 'QUOTE');
```

### Python - Backtesting

```python
engine = BacktestEngine(initial_capital=Decimal('100000'))
results = engine.run_backtest(data, simple_ma_crossover_strategy)

print(f"Total Return: {results['total_return_pct']:.2f}%")
print(f"Sharpe Ratio: {results['sharpe_ratio']:.2f}")
```

## Performance

- Matching latency: <1ms
- Throughput: 100K orders/sec
- WebSocket latency: <10ms

## License

MIT
