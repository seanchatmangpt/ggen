# ln_ctrl Golden Test Templates

Tera templates for generating deterministic golden examples and expected outputs for λn execution traces.

## Templates

### 1. scenario.json.tera
**Purpose**: Generate test scenario definitions with parameters and expected outcomes.

**Key Features**:
- Scenario metadata (ID, name, type, description)
- Test parameters (workflow ID, initial expression, budget constraints)
- Execution context (environment, bindings)
- Expected outcomes (completion status, final value, chain validity)
- Normalization rules for deterministic comparison

**Usage**:
```rust
let context = tera::Context::from_serialize(json!({
    "scenario_name": "complex_beta_eta_reduction",
    "initial_expression": "(λx.λy.x) 5 (λz.z)",
    "expected_steps": 5,
    "budget_steps": 10000,
}));
```

### 2. expected_receipts.json.tera
**Purpose**: Generate expected sequence of receipts for execution traces.

**Key Features**:
- Full receipt trace with all fields (operation, redex, frontier, effects, budget)
- Placeholder substitution for non-deterministic values (timestamps, hashes, signatures)
- Chain summary statistics
- Structural comparison support

**Normalization Placeholders**:
- `{{TIMESTAMP}}` - ISO 8601 timestamp fields
- `{{HASH}}` - SHA-256 hash values
- `{{SIGNATURE}}` - Ed25519 signatures
- `{{UUID}}` - UUIDs

**Usage**:
```rust
let receipts = vec![
    json!({
        "operation": "reduce",
        "redex_type": "beta",
        "redex_expression": "(λx.x+1) 5",
        "frontier_terms": ["5+1"],
        "step_index": 0,
    }),
    // ... more receipts
];

let context = tera::Context::from_serialize(json!({
    "receipts": receipts,
    "workflow_id": "550e8400-e29b-41d4-a716-446655440000",
}));
```

### 3. expected_verdict.json.tera
**Purpose**: Generate expected validation verdicts for positive test cases.

**Key Features**:
- Overall validity flag
- Detailed validation categories (chain integrity, execution semantics, completeness)
- Validation results (passed/failed/warnings)
- Error and warning arrays
- Chain metrics
- Cryptographic verification status

**Usage**:
```rust
let context = tera::Context::from_serialize(json!({
    "valid": true,
    "causal_chain_valid": true,
    "hash_chain_valid": true,
    "total_checks": 11,
    "passed_checks": 11,
    "chain_length": 3,
}));
```

### 4. expected_divergence.json.tera
**Purpose**: Generate expected divergence reports for negative test cases.

**Key Features**:
- Divergence detection metadata
- Validation failure details
- Chain break analysis
- Cryptographic issues
- Semantic issues (reduction rule violations, frontier inconsistencies)
- Budget violations
- Test assertions for negative cases

**Usage**:
```rust
let context = tera::Context::from_serialize(json!({
    "divergence_detected": true,
    "divergence_type": "hash_chain_break",
    "detected_at_step": 2,
    "validation_failures": vec![
        json!({
            "failure_type": "hash_mismatch",
            "receipt_index": 2,
            "field_path": "hash_chain",
        }),
    ],
}));
```

## Normalization Rules

All templates follow these normalization rules for deterministic comparison:

### Timestamp Normalization
Replace actual timestamps with `{{TIMESTAMP}}` placeholder:
```json
"timestamp": "{{TIMESTAMP}}"
```

### Hash Normalization
Replace SHA-256 hashes with `{{HASH}}` placeholder:
```json
"hash_chain": "{{HASH}}",
"causal_parent": "{{HASH}}",
"frontier_hash": "{{HASH}}"
```

### Signature Normalization
Replace Ed25519 signatures with `{{SIGNATURE}}` placeholder:
```json
"signature": "{{SIGNATURE}}"
```

### UUID Normalization
Replace UUIDs with `{{UUID}}` placeholder (or use fixed UUIDs in test data):
```json
"workflow_id": "{{UUID}}"
```

### Line Ending Normalization
- All golden files use LF line endings
- Cross-platform consistency enforced via `.gitattributes`

## Comparison Modes

### Structural Comparison
Used for receipt traces - compares structure and values while ignoring non-deterministic fields:
```json
"normalization_rules": {
    "comparison_mode": "structural",
    "ignore_fields": ["timestamp", "signature", "public_key"]
}
```

### Semantic Comparison
Used for divergence reports - focuses on semantic meaning rather than exact values:
```json
"normalization_rules": {
    "comparison_mode": "semantic",
    "normalize_hash_values": true
}
```

## Test Workflow

### 1. Create Scenario
```bash
# Define test scenario
ggen wizard ln_ctrl scenario \
    --name "beta_reduction_test" \
    --expression "(λx.x+1) 5" \
    --output scenarios/beta_test.json
```

### 2. Generate Expected Receipts
```bash
# Run execution and capture receipts
ggen wizard ln_ctrl execute \
    --scenario scenarios/beta_test.json \
    --output receipts/beta_test_receipts.json
```

### 3. Generate Expected Verdict
```bash
# Validate and generate verdict
ggen wizard ln_ctrl validate \
    --receipts receipts/beta_test_receipts.json \
    --output verdicts/beta_test_verdict.json
```

### 4. Compare with Golden
```rust
use ggen_e2e::golden::GoldenFile;

let golden = GoldenFile::load(golden_dir, Path::new("beta_test_receipts.json"))?;
let actual = fs::read_to_string("actual_receipts.json")?;

// Compare with normalization
match golden.compare(&actual) {
    Ok(_) => println!("✅ Test passed"),
    Err(mismatch) => {
        eprintln!("❌ Test failed:\n{}", mismatch.display());
    }
}
```

## Schema Validation

All templates generate JSON that validates against their respective schemas:

- `scenario.json.tera` → Self-contained schema
- `expected_receipts.json.tera` → `../schemas/receipt.schema.json.tera`
- `expected_verdict.json.tera` → Verdict schema (self-contained)
- `expected_divergence.json.tera` → Divergence schema (self-contained)

## Deterministic Output

All templates are designed to produce deterministic output:

1. **Fixed timestamps**: Use default values or placeholders
2. **Placeholder substitution**: Replace non-deterministic values with constants
3. **Sorted arrays**: Maintain consistent ordering
4. **Stable defaults**: All default values are deterministic
5. **No random generation**: No random UUIDs or values in defaults

## Examples

See test fixtures in:
- `/home/user/ggen/tests/e2e/golden/ln_ctrl/` - Golden files
- `/home/user/ggen/tests/e2e/fixtures/ln_ctrl/` - Test scenarios
- `/home/user/ggen/crates/ggen-cli/tests/ln_ctrl/` - Unit tests

## References

- [Receipt Contract Ontology](../../ontologies/ln_ctrl_receipts.ttl)
- [Receipt Schema](../schemas/receipt.schema.json.tera)
- [Golden Test Framework](../../../../../../crates/ggen-e2e/src/golden.rs)
- [E2E Testing Guide](../../../../../../tests/e2e/golden/README.md)
