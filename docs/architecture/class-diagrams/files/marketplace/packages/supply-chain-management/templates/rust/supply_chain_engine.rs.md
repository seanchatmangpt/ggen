# `marketplace/packages/supply-chain-management/templates/rust/supply_chain_engine.rs`

Source SHA-256: `2d620a88459241c763ec136cae623c1aad23d6f92041f7f1b696958cff006823`

```mermaid
classDiagram
    class struct_InventoryItem {
      <<struct>>
      +"item_code: String"
      +"quantity_on_hand: i32"
      +"safety_stock: i32"
      +"reorder_point: i32"
      +"economic_order_quantity: i32"
      +"lead_time_days: i32"
    }
    class struct_SupplyChainOptimizer {
      <<struct>>
      +"items: Vec~InventoryItem~"
    }
    class struct_PurchaseRecommendation {
      <<struct>>
      +"item_code: String"
      +"recommended_quantity: i32"
      +"current_stock: i32"
      +"reorder_point: i32"
    }
    class mod_tests {
      <<mod>>
    }
    note "InventoryItem"
    note "SupplyChainOptimizer"
```

## Dependencies

- `rust_decimal::Decimal`
- `serde::{Deserialize, Serialize}`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
