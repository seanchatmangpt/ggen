# Enterprise ERP Core

Comprehensive accounting and financial management system with double-entry bookkeeping, multi-currency support, and advanced financial reporting.

## Features

- **Double-Entry Accounting**: Validated journal entries with automatic balancing
- **Chart of Accounts**: Hierarchical account structure with custom types
- **Accounts Payable/Receivable**: Invoice tracking and aging reports
- **Purchase & Sales Orders**: Full procurement and sales workflows
- **Inventory Management**: FIFO, LIFO, and weighted average costing
- **Fixed Assets**: Depreciation schedules and asset tracking
- **Multi-Currency**: Exchange rates and currency gain/loss tracking
- **Financial Statements**: P&L, Balance Sheet, Cash Flow
- **Period Close**: Automated period close and audit controls
- **Audit Trail**: Complete transaction history and compliance

## Quick Start

### 1. Install Package

```bash
ggen marketplace install enterprise-erp-core
```

### 2. Generate Project

```bash
ggen new my-erp --template enterprise-erp-core
```

### 3. Load RDF Ontology

```rust
use ggen::rdf::load_ontology;

let ontology = load_ontology("ontology/erp.ttl")?;
```

### 4. Run SPARQL Queries

```rust
let trial_balance = query_template(
    "queries.rq",
    "trial_balance",
    params! {
        "start_date" => "2025-01-01",
        "end_date" => "2025-01-31"
    }
)?;
```

## Components

### Rust: Accounting Engine

High-performance double-entry accounting with validation:

```rust
use accounting_engine::{AccountingEngine, Account, AccountType, JournalEntry};

let mut engine = AccountingEngine::new();

// Add accounts
engine.add_account(Account {
    number: "1000".to_string(),
    name: "Cash".to_string(),
    account_type: AccountType::Asset,
    is_active: true,
})?;

// Post journal entry
engine.post_entry(JournalEntry {
    entry_number: "JE001".to_string(),
    description: "Initial investment".to_string(),
    lines: vec![
        JournalLine { account: "1000", debit: 10000, credit: 0 },
        JournalLine { account: "3000", debit: 0, credit: 10000 },
    ],
})?;

// Generate trial balance
let tb = engine.generate_trial_balance(start_date, end_date);
```

### TypeScript: ERP Dashboard

React-based financial reporting dashboard:

```typescript
import { ERPDashboard } from './erp_dashboard';

<ERPDashboard
  dateRange={{ startDate: '2025-01-01', endDate: '2025-01-31' }}
  reports={['income', 'balance', 'aging', 'cashflow']}
/>
```

### Python: Financial Analytics

Advanced analysis and forecasting:

```python
from financial_analytics import FinancialAnalytics

analytics = FinancialAnalytics()

# Calculate financial ratios
ratios = analytics.calculate_financial_ratios(
    total_assets=500000,
    total_liabilities=200000,
    revenue=400000,
    net_income=50000
)

# Forecast revenue
forecasts = analytics.forecast_revenue(historical_data, periods_ahead=12)

# Variance analysis
variances = analytics.variance_analysis(budget_data, actual_data)
```

## SPARQL Queries

### Trial Balance

```sparql
SELECT ?accountNumber ?accountName ?debitTotal ?creditTotal ?balance
WHERE {
  ?account a erp:Account ;
           erp:accountNumber ?accountNumber ;
           erp:accountName ?accountName .
  # ... (see queries.rq for full query)
}
```

### Accounts Receivable Aging

```sparql
SELECT ?customerName ?invoiceNumber ?amountDue ?agingBucket
WHERE {
  ?invoice a erp:Invoice ;
           erp:amountDue ?amountDue ;
           erp:invoiceCustomer ?customer .
  # ... (see queries.rq for full query)
}
```

## Accounting Principles

### Double-Entry System

Every transaction must balance:
- **Debits = Credits** (enforced by validation)
- Asset/Expense accounts increase with debits
- Liability/Equity/Revenue accounts increase with credits

### Account Types

1. **Assets**: Resources owned (Cash, A/R, Inventory, Fixed Assets)
2. **Liabilities**: Obligations owed (A/P, Notes Payable, Long-term Debt)
3. **Equity**: Owner's stake (Common Stock, Retained Earnings)
4. **Revenue**: Income earned (Sales, Service Revenue)
5. **Expenses**: Costs incurred (COGS, Salaries, Rent)

### Period Close

End-of-period checklist:
1. Reconcile all bank accounts
2. Post adjusting entries (accruals, deferrals)
3. Calculate depreciation
4. Generate financial statements
5. Close period (prevent backdated entries)
6. Archive audit trail

## Integration

### Tax Systems

Connect to tax calculation engines:
```rust
let tax_amount = calculate_sales_tax(invoice, tax_jurisdiction)?;
```

### Payroll Integration

Sync salary expenses and payroll liabilities:
```rust
sync_payroll_entries(payroll_system, accounting_engine)?;
```

### Banking Integration

Import bank transactions for reconciliation:
```rust
import_bank_transactions(bank_feed, "1000")?;
```

## Testing

Run Chicago TDD tests:

```bash
cargo test --package enterprise-erp-core
```

Tests cover:
- Double-entry validation
- Trial balance generation
- Financial statement accuracy
- Multi-currency handling
- Audit trail creation

## License

MIT License - see LICENSE file
