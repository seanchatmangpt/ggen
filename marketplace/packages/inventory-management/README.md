# Inventory Management Package

Production-ready inventory and warehouse management system with RDF ontology, SPARQL queries, and code templates.

## Overview

Comprehensive inventory tracking solution with:
- **Stock Management**: Real-time tracking across multiple locations
- **Valuation Methods**: FIFO, LIFO, weighted average
- **Lot Tracking**: Batch numbers, expiration dates, serial numbers
- **Reorder Automation**: Safety stock and reorder point calculation
- **Multi-Channel**: Allocate inventory across sales channels

## Features

- ✅ RDF ontology (290+ lines) defining inventory concepts
- ✅ 12 SPARQL queries for stock operations
- ✅ Rust inventory engine with FIFO/LIFO (high-performance)
- ✅ TypeScript/React warehouse dashboard
- ✅ Python demand forecasting with ML
- ✅ Chicago TDD tests with real RDF/SPARQL

## Quick Start

### 1. Track Stock Movement

```bash
ggen template render inventory-management/templates/rust/inventory_engine.rs \
  --output src/inventory/engine.rs
```

### 2. Query Stock Levels

```bash
ggen query execute inventory-management/sparql/queries.rq \
  --graph inventory.ttl \
  --query "Extract Stock Levels by Location"
```

### 3. Forecast Demand

```bash
python inventory-management/templates/python/demand_forecasting.py
```

## Architecture

### Valuation Methods

| Method | Description | Accounting | Use Case |
|--------|-------------|------------|----------|
| **FIFO** | First-In-First-Out | Oldest cost first | Standard retail |
| **LIFO** | Last-In-First-Out | Newest cost first | Inflation hedging |
| **Weighted Avg** | Average all costs | Smoothed cost | Manufacturing |
| **Specific ID** | Track individual items | Actual cost | High-value goods |

### Location Hierarchy

```
Warehouse
  └─ Zone (Receiving, Picking, Shipping)
      └─ Bin (Shelf-A-01, Shelf-A-02)
```

## RDF Ontology Classes

- `inv:Product` - Physical or digital item
- `inv:StockLevel` - Quantity at location
- `inv:StockMovement` - Transaction (receive/sell/transfer)
- `inv:ReorderPoint` - Trigger for replenishment
- `inv:Lot` - Batch with expiration date
- `inv:ValuationMethod` - FIFO/LIFO/WeightedAverage
- `inv:Channel` - Sales channel (online/retail/marketplace)
- `inv:Warehouse` - Storage facility

## SPARQL Query Examples

### Stock Levels by Location
```sparql
PREFIX inv: <http://example.org/inventory#>
SELECT ?productName ?locationName ?quantityAvailable WHERE {
    ?product inv:productName ?productName ;
             inv:hasStockLevel ?stock .
    ?stock inv:atLocation ?location ;
           inv:quantityAvailable ?quantityAvailable .
    ?location inv:locationName ?locationName .
}
```

### Products Needing Reorder
```sparql
PREFIX inv: <http://example.org/inventory#>
SELECT ?productId ?currentStock ?reorderLevel WHERE {
    ?product inv:productId ?productId ;
             inv:hasStockLevel ?stock ;
             inv:hasReorderPoint ?reorder .
    ?stock inv:quantityAvailable ?currentStock .
    ?reorder inv:reorderLevel ?reorderLevel .
    FILTER(?currentStock <= ?reorderLevel)
}
```

## Code Examples

### Rust: FIFO Inventory Engine
```rust
let mut engine = InventoryEngine::new();

// Receive stock
engine.receive_inventory("WIDGET-A", "WAREHOUSE-1", 100, 10.0);

// Sell stock (calculates COGS using FIFO)
let cogs = engine.sell_inventory("WIDGET-A", "WAREHOUSE-1", 30).unwrap();

// Check reorder needed
if engine.check_reorder_needed("WIDGET-A", "WAREHOUSE-1", 20) {
    println!("Reorder needed!");
}
```

### TypeScript: Warehouse Dashboard
```tsx
import { StockLevelsDashboard } from './warehouse-ui';

function App() {
  return <StockLevelsDashboard />;
}
```

### Python: Demand Forecasting
```python
from demand_forecasting import DemandForecaster

forecaster = DemandForecaster()
forecast = forecaster.forecast_demand('WIDGET-PRO', forecast_days=30)
safety_stock = forecaster.calculate_safety_stock('WIDGET-PRO', lead_time_days=7)
```

## Testing

Run Chicago TDD tests:
```bash
cargo test --package inventory-management
```

## License

MIT
