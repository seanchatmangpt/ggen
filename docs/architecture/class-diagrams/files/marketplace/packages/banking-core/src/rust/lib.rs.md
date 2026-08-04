# `marketplace/packages/banking-core/src/rust/lib.rs`

Source SHA-256: `826fbf9b1483a6a7139fb9e4f146853b3018c3e244638787b751a64e81f764de`

```mermaid
classDiagram
    class struct_Account {
      <<struct>>
      +"account_number: String"
      +"account_type: AccountType"
      +"balance: f64"
      +"available_balance: f64"
      +"status: AccountStatus"
    }
    class enum_AccountType {
      <<enum>>
    }
    class enum_AccountStatus {
      <<enum>>
    }
    class struct_Transaction {
      <<struct>>
      +"id: String"
      +"account_number: String"
      +"transaction_type: TransactionType"
      +"amount: f64"
      +"description: String"
    }
    class enum_TransactionType {
      <<enum>>
    }
    class struct_InterestCalculator {
      <<struct>>
    }
    class struct_Ledger {
      <<struct>>
      +"entries: Vec~LedgerEntry~"
    }
    class struct_LedgerEntry {
      <<struct>>
      +"debit_account: String"
      +"credit_account: String"
      +"amount: f64"
    }
    note "InterestCalculator"
    note "Ledger"
```

## Dependencies

- `serde::{Deserialize, Serialize}`
- `std::collections::HashMap`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
