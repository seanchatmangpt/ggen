# `marketplace/packages/order-management-system/tests/chicago_tdd/order_processor_tests.rs`

Source SHA-256: `499a1745b8cbb2cf175b4ebc3aacb850612e43e10a37411ea5302a6183b9de87`

```mermaid
classDiagram
    class mod_mocks {
      <<mod>>
    }
    class mod_order_lifecycle_tests {
      <<mod>>
    }
    class mod_order_calculation_tests {
      <<mod>>
    }
    class mod_payment_processing_tests {
      <<mod>>
    }
    class mod_fulfillment_tests {
      <<mod>>
    }
    class mod_return_refund_tests {
      <<mod>>
    }
    class mod_analytics_tests {
      <<mod>>
    }
    class fn_create_test_order {
      <<fn>>
    }
```

## Dependencies

- `chrono::Utc`
- `mocks::*`
- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
