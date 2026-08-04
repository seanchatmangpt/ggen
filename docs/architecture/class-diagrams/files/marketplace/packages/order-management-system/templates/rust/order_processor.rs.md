# `marketplace/packages/order-management-system/templates/rust/order_processor.rs`

Source SHA-256: `c25dd25b1baa3ee0126e2ecd487d8d1a707a01572693c94203038719609b7255`

```mermaid
classDiagram
    class enum_OrderStatus {
      <<enum>>
    }
    class struct_Order {
      <<struct>>
      +"order_number: String"
      +"customer_id: String"
      +"status: OrderStatus"
      +"items: Vec~OrderItem~"
      +"subtotal: f64"
      +"tax: f64"
      +"shipping_cost: f64"
      +"discount: f64"
      +"total: f64"
      +"currency: String"
      +"order_date: DateTime~Utc~"
      +"payment: Option~Payment~"
      +"shipment: Option~Shipment~"
    }
    class struct_OrderItem {
      <<struct>>
      +"product_id: String"
      +"sku: String"
      +"quantity: u32"
      +"unit_price: f64"
      +"line_total: f64"
    }
    class struct_Payment {
      <<struct>>
      +"transaction_id: String"
      +"method: String"
      +"amount: f64"
      +"status: String"
    }
    class struct_Shipment {
      <<struct>>
      +"carrier: String"
      +"tracking_number: String"
      +"estimated_delivery: DateTime~Utc~"
      +"actual_delivery: Option~DateTime~Utc~~"
    }
    class struct_OrderProcessor {
      <<struct>>
      +"valid_transitions: HashMap~OrderStatus"
    }
    class struct_FulfillmentEngine {
      <<struct>>
    }
    class mod_tests {
      <<mod>>
    }
    note "FulfillmentEngine"
    note "OrderProcessor"
```

## Dependencies

- `chrono::{DateTime, Utc}`
- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
