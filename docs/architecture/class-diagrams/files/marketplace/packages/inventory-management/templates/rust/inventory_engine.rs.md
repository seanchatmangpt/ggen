# `marketplace/packages/inventory-management/templates/rust/inventory_engine.rs`

Source SHA-256: `4a940d3f6240f73ecfcd8ce7b4ee34c947bb44f5c78f3680109385994b7fede5`

```mermaid
classDiagram
    class enum_ValuationMethod {
      <<enum>>
    }
    class struct_StockMovement {
      <<struct>>
      +"movement_id: String"
      +"product_id: String"
      +"quantity: i32"
      +"unit_cost: f64"
      +"location_id: String"
      +"timestamp: DateTime~Utc~"
      +"movement_type: MovementType"
    }
    class enum_MovementType {
      <<enum>>
    }
    class struct_StockLevel {
      <<struct>>
      +"product_id: String"
      +"location_id: String"
      +"quantity_on_hand: i32"
      +"quantity_reserved: i32"
      +"valuation_method: ValuationMethod"
      +"fifo_layers: VecDeque~FIFOLayer~"
    }
    class struct_FIFOLayer {
      <<struct>>
      +"quantity: i32"
      +"unit_cost: f64"
      +"received_at: DateTime~Utc~"
    }
    class struct_InventoryEngine {
      <<struct>>
      +"stock_levels: HashMap~(String"
      +"movements: Vec~StockMovement~"
    }
    class mod_tests {
      <<mod>>
    }
    note "InventoryEngine"
    note "StockLevel"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `std::collections::{HashMap, VecDeque}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
