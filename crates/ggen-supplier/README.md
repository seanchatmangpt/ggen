# ggen-supplier

Supplier quality scoring and rate limiting system for ggen v6.0.0.

## Features

- **Quality Scoring**: Calculate defect rates across four dimensions
  - Incomplete packets
  - Churn
  - Urgency inflation
  - Coordination dumping

- **Risk Assessment**: 5-level risk classification
  - Excellent (0% defects)
  - Low (0-5%)
  - Medium (5-10%)
  - High (10-20%)
  - Critical (>20%)

- **Rate Limiting**: Auto rate-limit suppliers exceeding 20% defect threshold
  - Configurable violation limits (default: 3 strikes)
  - Configurable cooldown periods (default: 24 hours)
  - Persistent state tracking

- **History Tracking**: Track supplier performance over time
  - Trend analysis (improving/stable/worsening)
  - Average defect rate calculation
  - Supplier comparison
  - Time-based filtering

## Architecture

```
crates/ggen-supplier/
├── Cargo.toml
├── src/
│   ├── lib.rs       # Core types: Supplier, QualityMetrics, Result
│   ├── scorer.rs    # Quality scoring and risk assessment
│   ├── limiter.rs   # Rate limiting with violation tracking
│   └── history.rs   # Historical tracking and trend analysis
└── tests/
    └── integration_test.rs  # End-to-end workflow tests
```

## Usage

```rust
use ggen_supplier::{
    Supplier, QualityMetrics,
    scorer::QualityScorer,
    limiter::RateLimiter,
    history::HistoryTracker,
};

// Create supplier and metrics
let supplier = Supplier::new("s1", "Acme Corp");
let metrics = QualityMetrics::new(5, 3, 2, 1, 100)?;

// Score quality
let assessment = QualityScorer::assess_supplier(&metrics)?;
println!("Risk: {:?}, Defect Rate: {:.2}%",
    assessment.risk_level,
    assessment.score.defect_rate);

// Check rate limits
let mut limiter = RateLimiter::with_default_config();
let status = limiter.check_supplier(&supplier, &metrics)?;
if status.is_limited {
    eprintln!("Supplier is rate limited until {:?}", status.limited_until);
}

// Track history
let mut tracker = HistoryTracker::new();
tracker.record(&supplier, metrics)?;
let trend = tracker.get_history("s1")
    .and_then(|h| h.trend_analysis(4));
```

## Testing

All 65 tests pass:
- 59 unit tests (lib, scorer, limiter, history)
- 6 integration tests (end-to-end workflows)

```bash
cd crates/ggen-supplier
cargo test
```

## Design Principles

- **Type-First**: Compiler enforces invariants (e.g., defects ≤ deliveries)
- **Zero-Unwrap**: All operations return `Result<T, SupplierError>`
- **Immutable by Default**: State changes explicit via `&mut`
- **Chicago TDD**: Real dependencies, state-based verification
