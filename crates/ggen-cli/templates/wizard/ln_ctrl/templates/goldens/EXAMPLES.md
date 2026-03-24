# Golden Template Usage Examples

Complete examples demonstrating how to use ln_ctrl golden templates for test scenario generation.

## Example 1: Basic Beta Reduction (Positive Test)

### Step 1: Create Scenario

```rust
use serde_json::json;
use tera::{Context, Tera};

let mut tera = Tera::new("templates/goldens/*.tera").unwrap();
let mut context = Context::new();

// Define test scenario
context.insert("scenario_name", "beta_reduction_simple");
context.insert("scenario_type", "positive");
context.insert("description", "Simple beta reduction: (λx.x+1) 5 → 6");
context.insert("initial_expression", "(λx.x+1) 5");
context.insert("expected_steps", &3);
context.insert("final_value", "6");
context.insert("budget_steps", &10000);

let scenario = tera.render("scenario.json.tera", &context).unwrap();
fs::write("scenarios/beta_simple.json", scenario).unwrap();
```

**Generated Output** (`scenarios/beta_simple.json`):
```json
{
  "scenario_id": "550e8400-e29b-41d4-a716-446655440000",
  "scenario_name": "beta_reduction_simple",
  "scenario_type": "positive",
  "description": "Simple beta reduction: (λx.x+1) 5 → 6",
  "test_parameters": {
    "workflow_id": "550e8400-e29b-41d4-a716-446655440000",
    "initial_expression": "(λx.x+1) 5",
    "expected_steps": 3,
    "budget": {
      "budget_steps": 10000,
      "budget_memory": 104857600,
      "budget_time": 30000
    }
  },
  "expected_outcomes": {
    "should_complete": true,
    "final_value": "6",
    "total_receipts": 3
  }
}
```

### Step 2: Generate Expected Receipts

```rust
let receipts = vec![
    json!({
        "operation": "reduce",
        "redex_type": "beta",
        "redex_expression": "(λx.x+1) 5",
        "frontier_terms": ["5+1"],
        "step_index": 0,
        "budget_steps": 9999,
    }),
    json!({
        "operation": "evaluate",
        "redex_type": "delta",
        "redex_expression": "5+1",
        "frontier_terms": ["6"],
        "step_index": 1,
        "budget_steps": 9998,
    }),
    json!({
        "operation": "normalize",
        "redex_type": "eta",
        "redex_expression": "6",
        "frontier_terms": ["6"],
        "step_index": 2,
        "budget_steps": 9997,
    }),
];

let mut context = Context::new();
context.insert("receipts", &receipts);
context.insert("workflow_id", "550e8400-e29b-41d4-a716-446655440000");
context.insert("chain_valid", &true);

let expected_receipts = tera.render("expected_receipts.json.tera", &context).unwrap();
fs::write("goldens/beta_simple_receipts.json", expected_receipts).unwrap();
```

### Step 3: Generate Expected Verdict

```rust
let mut context = Context::new();
context.insert("scenario_id", "550e8400-e29b-41d4-a716-446655440000");
context.insert("valid", &true);
context.insert("causal_chain_valid", &true);
context.insert("hash_chain_valid", &true);
context.insert("signatures_valid", &true);
context.insert("total_checks", &11);
context.insert("passed_checks", &11);
context.insert("failed_checks", &0);
context.insert("chain_length", &3);
context.insert("total_reductions", &3);

let verdict = tera.render("expected_verdict.json.tera", &context).unwrap();
fs::write("goldens/beta_simple_verdict.json", verdict).unwrap();
```

## Example 2: Hash Chain Break (Negative Test)

### Scenario: Detect Hash Chain Integrity Violation

```rust
// Step 1: Create divergence scenario
let mut context = Context::new();
context.insert("scenario_name", "hash_chain_break");
context.insert("scenario_type", "negative");
context.insert("description", "Detect hash chain break at step 2");
context.insert("tags", &vec!["negative-test", "hash-integrity", "security"]);

let scenario = tera.render("scenario.json.tera", &context).unwrap();
fs::write("scenarios/hash_break.json", scenario).unwrap();

// Step 2: Generate receipts with intentional break
let receipts = vec![
    json!({
        "operation": "reduce",
        "redex_type": "beta",
        "redex_expression": "(λx.x) 42",
        "frontier_terms": ["42"],
        "step_index": 0,
    }),
    json!({
        "operation": "evaluate",
        "redex_type": "delta",
        "redex_expression": "42",
        "frontier_terms": ["42"],
        "step_index": 1,
    }),
    json!({
        "operation": "normalize",
        "redex_type": "eta",
        "redex_expression": "42",
        "frontier_terms": ["42"],
        "step_index": 2,
        // Step 3: This receipt will have invalid hash_chain
    }),
];

let mut context = Context::new();
context.insert("receipts", &receipts);
context.insert("chain_valid", &false);

let receipts_json = tera.render("expected_receipts.json.tera", &context).unwrap();

// Step 3: Generate expected divergence report
let mut context = Context::new();
context.insert("divergence_detected", &true);
context.insert("divergence_type", "hash_chain_break");
context.insert("detected_at_step", &2);
context.insert("severity", "critical");
context.insert("primary_issue", "Hash chain break detected at step 2");
context.insert("issue_category", "cryptographic_integrity");

let validation_failures = vec![json!({
    "failure_type": "hash_mismatch",
    "failure_message": "Expected hash does not match computed hash",
    "receipt_index": 2,
    "field_path": "hash_chain",
    "severity": "error",
})];
context.insert("validation_failures", &validation_failures);

let chain_breaks = vec![json!({
    "break_location": 2,
    "break_type": "hash_chain",
    "parent_receipt": 1,
    "child_receipt": 2,
    "description": "Child receipt hash_chain does not match parent's hash",
})];
context.insert("chain_breaks", &chain_breaks);

context.insert("assert_divergence_detected", &true);
context.insert("assert_error_count", &1);
context.insert("assert_chain_invalid", &true);

let divergence = tera.render("expected_divergence.json.tera", &context).unwrap();
fs::write("goldens/hash_break_divergence.json", divergence).unwrap();
```

## Example 3: Budget Exceeded (Negative Test)

```rust
// Scenario with budget violation
let mut context = Context::new();
context.insert("scenario_name", "budget_exceeded_steps");
context.insert("scenario_type", "negative");
context.insert("description", "Execution exceeds step budget");
context.insert("budget_steps", &5); // Low budget
context.insert("expected_steps", &10); // More steps than budget
context.insert("budget_exceeded", &true);

let scenario = tera.render("scenario.json.tera", &context).unwrap();

// Expected divergence for budget violation
let mut context = Context::new();
context.insert("divergence_detected", &true);
context.insert("divergence_type", "budget_exceeded");
context.insert("detected_at_step", &5);
context.insert("severity", "error");

let budget_violations = vec![json!({
    "violation_type": "steps_exceeded",
    "receipt_index": 5,
    "budget_limit": 5,
    "actual_consumption": 6,
    "overflow_amount": 1,
})];
context.insert("budget_violations", &budget_violations);

let divergence = tera.render("expected_divergence.json.tera", &context).unwrap();
```

## Example 4: Complex Execution with Effects

```rust
// Scenario with side effects
let receipts = vec![
    json!({
        "operation": "reduce",
        "redex_type": "beta",
        "redex_expression": "(λx.log(x); x+1) 5",
        "frontier_terms": ["log(5); 5+1"],
        "step_index": 0,
        "effects": [
            {
                "effect_type": "logging",
                "effect_data": "{\"level\":\"info\",\"message\":\"Value: 5\"}",
                "effect_result": "logged",
            }
        ],
    }),
    json!({
        "operation": "evaluate",
        "redex_type": "delta",
        "redex_expression": "5+1",
        "frontier_terms": ["6"],
        "step_index": 1,
        "effects": [],
    }),
];

let mut context = Context::new();
context.insert("receipts", &receipts);
context.insert("enable_effects", &true);

let receipts_json = tera.render("expected_receipts.json.tera", &context).unwrap();

// Verdict should verify effects ordering
let mut context = Context::new();
context.insert("valid", &true);
context.insert("effects_ordered", &true);
context.insert("total_effects", &1);

let verdict = tera.render("expected_verdict.json.tera", &context).unwrap();
```

## Example 5: Test Harness Integration

```rust
use ggen_e2e::golden::GoldenFile;
use std::path::Path;

/// Complete test workflow
#[test]
fn test_beta_reduction_golden() -> Result<(), Box<dyn std::error::Error>> {
    // 1. Load scenario
    let scenario: Value = serde_json::from_str(
        &fs::read_to_string("scenarios/beta_simple.json")?
    )?;

    // 2. Execute λn interpreter with scenario
    let actual_receipts = execute_ln_ctrl(&scenario)?;

    // 3. Load golden receipts
    let golden = GoldenFile::load(
        Path::new("goldens"),
        Path::new("beta_simple_receipts.json")
    )?;

    // 4. Normalize and compare
    let normalized_actual = normalize_receipts(&actual_receipts)?;

    match golden.compare(&normalized_actual) {
        Ok(_) => {
            println!("✅ Receipts match golden file");

            // 5. Validate receipts
            let verdict = validate_receipts(&actual_receipts)?;

            // 6. Load expected verdict
            let golden_verdict = GoldenFile::load(
                Path::new("goldens"),
                Path::new("beta_simple_verdict.json")
            )?;

            // 7. Compare verdict
            golden_verdict.compare(&serde_json::to_string_pretty(&verdict)?)?;

            println!("✅ Verdict matches golden file");
            Ok(())
        }
        Err(mismatch) => {
            eprintln!("❌ Receipt mismatch:\n{}", mismatch.display());

            // Optionally update golden if UPDATE_GOLDEN=1
            if env::var("UPDATE_GOLDEN").is_ok() {
                fs::write("goldens/beta_simple_receipts.json", &normalized_actual)?;
                println!("⚠️  Updated golden file");
            }

            Err("Golden file mismatch".into())
        }
    }
}

/// Normalize receipts for deterministic comparison
fn normalize_receipts(receipts: &Value) -> Result<String, Box<dyn std::error::Error>> {
    let mut normalized = receipts.clone();

    // Replace non-deterministic fields with placeholders
    if let Some(receipt_array) = normalized["receipts"].as_array_mut() {
        for receipt in receipt_array {
            receipt["timestamp"] = json!("{{TIMESTAMP}}");
            receipt["hash_chain"] = json!("{{HASH}}");
            receipt["signature"] = json!("{{SIGNATURE}}");
            receipt["public_key"] = json!("{{HASH}}");

            if let Some(parent) = receipt.get_mut("causal_parent") {
                if !parent.is_null() {
                    *parent = json!("{{HASH}}");
                }
            }

            if let Some(frontier) = receipt.get_mut("frontier_after") {
                frontier["frontier_hash"] = json!("{{HASH}}");
            }
        }
    }

    Ok(serde_json::to_string_pretty(&normalized)?)
}
```

## Example 6: Parametric Test Generation

```rust
/// Generate multiple test scenarios from parameters
fn generate_test_suite() -> Result<(), Box<dyn std::error::Error>> {
    let test_cases = vec![
        ("identity", "(λx.x) 42", 1, "42"),
        ("simple_add", "(λx.x+1) 5", 2, "6"),
        ("compose", "(λf.λg.λx.f(g(x))) (λy.y+1) (λz.z*2) 5", 4, "11"),
        ("church_two", "(λf.λx.f(f(x))) (λy.y+1) 0", 3, "2"),
    ];

    for (name, expr, steps, result) in test_cases {
        // Generate scenario
        let mut context = Context::new();
        context.insert("scenario_name", name);
        context.insert("initial_expression", expr);
        context.insert("expected_steps", &steps);
        context.insert("final_value", result);

        let scenario = tera.render("scenario.json.tera", &context)?;
        fs::write(format!("scenarios/{}.json", name), scenario)?;

        println!("✅ Generated scenario: {}", name);
    }

    Ok(())
}
```

## Normalization Reference

### Fields to Normalize

| Field | Placeholder | Reason |
|-------|-------------|--------|
| `timestamp` | `{{TIMESTAMP}}` | Changes on every execution |
| `hash_chain` | `{{HASH}}` | Cryptographic hash (256-bit) |
| `causal_parent` | `{{HASH}}` | Dependent on parent hash |
| `frontier_hash` | `{{HASH}}` | Hash of frontier state |
| `signature` | `{{SIGNATURE}}` | Ed25519 signature (512-bit) |
| `public_key` | `{{HASH}}` | Public key for verification |

### Fields to Preserve

- `operation` - Reduction operation type
- `redex_type` - Type of redex (beta, eta, delta, etc.)
- `redex_expression` - The expression being reduced
- `frontier_terms` - Terms at execution frontier
- `step_index` - Sequential step number
- `budget_*` - Resource budget values
- `chain_length` - Number of receipts in chain

## Best Practices

1. **Use Descriptive Names**: Scenario names should describe what is being tested
2. **Document Expected Behavior**: Include clear descriptions in scenario metadata
3. **Test Both Paths**: Create positive (valid) and negative (divergence) test cases
4. **Parametric Generation**: Use loops to generate multiple similar scenarios
5. **Version Control Golden Files**: Commit golden files to track changes over time
6. **Review Diffs**: Always review golden file changes before committing
7. **Deterministic Defaults**: Use fixed values in templates, not random generation
8. **Normalize Before Compare**: Always normalize non-deterministic fields
9. **Update Carefully**: Only update golden files when behavior intentionally changes
10. **Document Assumptions**: Add comments explaining why certain values are expected
