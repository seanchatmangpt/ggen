# Supply Chain Management

Comprehensive supply chain optimization system covering procurement, logistics, warehouse management, and demand planning.

## Features

- **Supplier Management**: Vendor performance tracking and contract management
- **Procurement**: Purchase requisitions, RFQs, and purchase orders
- **Shipment Tracking**: Real-time logistics and carrier management
- **Warehouse Operations**: Bin management, picking, and packing
- **Demand Forecasting**: ML-powered demand prediction
- **Inventory Optimization**: EOQ calculation and reorder point management
- **Performance Analytics**: Supplier and carrier performance metrics

## Quick Start

```bash
ggen marketplace install supply-chain-management
ggen new my-supply-chain --template supply-chain-management
```

## Components

### Economic Order Quantity (EOQ)

```rust
let optimizer = SupplyChainOptimizer::new();
let eoq = optimizer.calculate_eoq(
    annual_demand: 12000,
    ordering_cost: Decimal::from(50),
    holding_cost: Decimal::from(2)
);
```

### Purchase Recommendations

```rust
let recommendations = optimizer.generate_purchase_recommendations();
for rec in recommendations {
    println!("Order {} units of {}", rec.recommended_quantity, rec.item_code);
}
```

## SPARQL Queries

- Purchase recommendations based on reorder points
- Supplier performance analysis
- Shipment tracking and delays
- Warehouse utilization metrics
- Demand forecasting
- ABC inventory classification

## License

MIT
