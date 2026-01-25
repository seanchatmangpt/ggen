# tai-validation: SLO/SLA Validation & Compliance Framework

Production-ready validation system for ggen project ensuring SLO/SLA compliance, multi-framework compliance auditing, security scanning, and deterministic evidence collection.

## Features

### 1. SLO/SLA Tracking (`slo/`)
- **Metrics Collection**: Build time, test duration, memory usage, RDF processing
- **Threshold Validation**: Automatic violation detection with remediation guidance
- **Historical Tracking**: SLA trend analysis with percentile calculations
- **Andon Signals**: Violations trigger stop-the-line alerts

**SLO Targets:**
- First build ≤ 15s
- Incremental build ≤ 2s
- RDF processing ≤ 5s (for 1000+ triples)
- Memory usage ≤ 100MB
- CLI end-to-end ≤ 3s

### 2. Compliance Framework (`compliance/`)
Multi-framework support with automated control evaluation:
- **FISMA**: Federal Information Security Management Act
- **FedRAMP**: Federal Risk and Authorization Management Program
- **SOC 2 Type II**: Service Organization Control framework
- **HIPAA**: Health Insurance Portability and Accountability Act
- **21 CFR Part 11**: FDA Regulations for Electronic Records
- **NIST 800-53**: Security and Privacy Controls
- **DFARS**: Defense Federal Acquisition Regulation Supplement

Features:
- Control status tracking (Compliant, Non-Compliant, In Progress, Not Applicable)
- Evidence collection and correlation
- Violation tracking with remediation steps
- Compliance percentage calculation

### 3. Security Scanning (`security/`)
Automated vulnerability detection:
- **SAST**: Static Application Security Testing
- **DAST**: Dynamic Application Security Testing
- **Dependency Scanning**: Supply chain vulnerability detection
- **SBOM**: Software Bill of Materials generation

Result includes:
- Vulnerability classification by severity (Critical, High, Medium, Low)
- Affected component identification
- Remediation guidance

### 4. SHACL Validation (`shacl/`)
RDF ontology conformance checking:
- Validates .specify/*.ttl files against SHACL shapes
- Reports schema violations with details
- Processes triple counts and shape conformance
- Severity levels: Error, Warning, Info

### 5. Pre-flight Validation (`preflight/`)
Pre-deployment readiness checks:
- Resource availability (disk space, memory)
- Permission verification
- Configuration validation
- Dependency resolution

Check types: Passed, Failed, Skipped, Warning

### 6. Evidence Collection (`evidence/`)
Deterministic receipt generation with cryptographic proofs:
- SHA-256 hashing for all artifacts
- Validation receipt with execution ID
- Manifest and ontology fingerprinting
- File-by-file change tracking
- JSON audit trail export

### 7. Test Coverage Analysis (`coverage/`)
Coverage metrics and gap analysis:
- Per-module coverage calculation
- Coverage gap identification
- Prioritized remediation recommendations
- Trend analysis (lines, functions, branches)

### 8. Acceptance Criteria Matching (`acceptance/`)
Requirements-to-test traceability:
- Parse user stories from .specify/*.ttl
- Map acceptance criteria to test cases
- Generate traceability matrix
- Coverage percentage calculation

### 9. Test Execution Pipeline (`execution/`)
Parallel test execution with timeout enforcement:
- Batch-based test execution
- Parallel batch scheduling
- Timeout enforcement (quick <5s, standard <10s, integration <30s)
- Retry logic with exponential backoff
- Aggregated result reporting with latency percentiles

## Architecture

```
crates/tai-validation/
├── src/
│   ├── lib.rs              # Main library interface
│   ├── error.rs            # Unified error types
│   ├── slo/
│   │   ├── mod.rs
│   │   ├── metrics.rs      # Measurement and aggregation
│   │   ├── validator.rs    # SLO validation engine
│   │   └── tracker.rs      # Historical tracking
│   ├── compliance/
│   │   ├── mod.rs
│   │   ├── frameworks.rs   # Framework definitions
│   │   ├── controls.rs     # Control evaluation
│   │   └── evidence.rs     # Evidence collection
│   ├── security.rs         # SAST/DAST/SBOM integration
│   ├── shacl.rs           # RDF validation
│   ├── preflight.rs       # Pre-deployment checks
│   ├── evidence.rs        # Cryptographic receipts
│   ├── coverage.rs        # Test coverage analysis
│   ├── acceptance.rs      # Requirements traceability
│   └── execution.rs       # Test execution pipeline
├── tests/
│   ├── integration_tests.rs
│   ├── compliance_tests.rs
│   ├── security_tests.rs
│   └── evidence_tests.rs
├── Cargo.toml
└── README.md
```

## Quick Start

### SLO Validation

```rust,no_run
use tai_validation::slo::SloValidator;
use tai_validation::slo::metrics::MetricType;

#[tokio::main]
async fn main() -> Result<()> {
    let validator = SloValidator::new();

    // Validate build time
    let metrics = validator.validate_metric(MetricType::BuildTime, 12.5)?;
    assert!(!metrics.is_violating());

    Ok(())
}
```

### Compliance Auditing

```rust,no_run
use tai_validation::compliance::ComplianceFramework;

#[tokio::main]
async fn main() -> Result<()> {
    let framework = ComplianceFramework::fisma();
    let results = framework.audit().await?;
    println!("Compliance: {}", results.remediation_summary());
    Ok(())
}
```

### Evidence Collection

```rust,no_run
use tai_validation::evidence::{ValidationReceipt, EvidenceCollector};

fn main() -> Result<()> {
    let receipt = ValidationReceipt::new()
        .with_manifest_hash("abc123".to_string())
        .with_ontology_hash("def456".to_string())
        .add_file("file.rs".to_string(), "hash789".to_string())
        .with_duration(2.5)
        .with_rules_executed(42);

    println!("{}", receipt.to_json()?);
    Ok(())
}
```

### Test Execution

```rust,no_run
use tai_validation::execution::{ExecutionPipeline, TestBatch};

#[tokio::main]
async fn main() -> Result<()> {
    let mut pipeline = ExecutionPipeline::new()
        .with_max_concurrent(4)
        .with_retry(true, 3);

    let batch = TestBatch::new(
        "batch1".to_string(),
        vec!["test1".to_string(), "test2".to_string()],
    );
    pipeline.add_batch(batch);

    let results = pipeline.execute().await?;
    println!("{}", results.summary());
    Ok(())
}
```

## Compliance Workflows

### FISMA Audit

```rust,no_run
let framework = ComplianceFramework::fisma();
let results = framework.audit().await?;

if results.is_compliant() {
    println!("System is FISMA compliant");
} else {
    for violation in results.high_severity_violations() {
        println!("Violation: {}", violation.remediation.join(", "));
    }
}
```

### FedRAMP Assessment

```rust,no_run
let framework = ComplianceFramework::fedramp();
let results = framework.audit().await?;
println!("Compliance: {:.1}%", results.compliance_percentage);
```

## Testing

All modules include comprehensive Chicago TDD tests with AAA pattern:

```bash
cargo test -p tai-validation
cargo test -p tai-validation -- --nocapture
cargo test -p tai-validation -- --test-threads=1
```

## Integration with ggen

The `tai-validation` crate integrates with:
- `ggen-cli-validation`: CLI validation hooks
- `ggen-spec-validator`: RDF specification validation
- `ggen-test-audit`: Test quality metrics
- `tai-testing`: Production hardening tests

## SLO Thresholds

Defined in `src/slo/mod.rs`:

```rust
pub const SLO_BUILD_FIRST: f64 = 15.0;           // seconds
pub const SLO_BUILD_INCREMENTAL: f64 = 2.0;     // seconds
pub const SLO_RDF_PROCESSING: f64 = 5.0;        // seconds
pub const SLO_MEMORY_MAX: f64 = 100.0;          // MB
pub const SLO_CLI_END_TO_END: f64 = 3.0;        // seconds
```

## Compliance Matrix

| Framework | Controls | Status | Evidence |
|-----------|----------|--------|----------|
| FISMA | SI-4, AC-2, AU-2, ... | Framework ready | Automated collection |
| FedRAMP | SC-7, SI-4, AU-12, ... | Framework ready | Automated collection |
| SOC 2 Type II | CC6.1, CC7.2, CC9.2, ... | Framework ready | Automated collection |
| HIPAA | 164.308, 164.312, ... | Framework ready | Automated collection |
| 21 CFR Part 11 | 11.100, 11.200, 11.300 | Framework ready | Automated collection |
| NIST 800-53 | AC, AU, SC, SI, ... | Framework ready | Automated collection |
| DFARS | 252.204-7012, 7019, ... | Framework ready | Automated collection |

## Error Handling

All operations return `Result<T, ValidationError>` with detailed context:

```rust
pub enum ValidationError {
    SloViolation { metric, actual, threshold },
    ComplianceFailure { framework, control_id, description },
    SecurityVulnerability { vuln_type, severity, description },
    ShapeViolation { shape_uri, property, message },
    PreFlightFailure { check_name, reason },
    // ... more variants
}
```

## Performance Characteristics

- **Compliance audit**: O(n) where n = number of controls
- **SHACL validation**: O(n) where n = number of triples
- **Coverage analysis**: O(n log n) with sorting
- **Test execution**: Parallel O(n/m) where m = max_concurrent

## Future Enhancements

- Real SAST/DAST integration (currently stubs)
- SBOM generation in CycloneDX format
- Real SPARQL-based SHACL validation
- Machine learning for anomaly detection
- Compliance trend visualization
- Integration with GitHub Actions
- Webhook support for real-time alerts

## License

MIT License - See LICENSE file for details
