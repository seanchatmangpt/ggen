# FMEA Module - Failure Mode and Effects Analysis

Systematic risk assessment for RDF ontology systems with automated SOD scoring and RPN calculation.

## Quick Start

```rust
use ggen_core::fmea::*;
use ggen_core::Graph;

// 1. Create analyzers
let schema_analyzer = SchemaChangeAnalyzer::new();
let query_analyzer = QueryPathAnalyzer::new();
let template_analyzer = TemplateRiskAnalyzer::new();

// 2. Analyze schema changes
let changes = schema_analyzer.analyze_changes(&old_schema, &new_schema)?;
let failure_modes = schema_analyzer.changes_to_failure_modes(&changes);

// 3. Check RPN and take action
for fm in failure_modes {
    if fm.rpn.requires_immediate_action() {
        eprintln!("CRITICAL: {} (RPN: {})", fm.description, fm.rpn.value());
    }
}

// 4. Proactive prevention
let mut prevention = FailurePreventionEngine::new();
let report = prevention.validate_schema_changes(&old_schema, &new_schema)?;

if report.is_blocked() {
    return Err(anyhow!("Validation failed: {}", report.summary()));
}
```

## Module Structure

- **`types.rs`** - Core FMEA types (Severity, Occurrence, Detection, RPN, FailureMode)
- **`scoring.rs`** - SOD scoring and RPN calculation
- **`schema_analyzer.rs`** - RDF schema change analysis
- **`query_analyzer.rs`** - SPARQL query path risk analysis
- **`template_analyzer.rs`** - Template transformation risk analysis
- **`prevention.rs`** - Proactive failure prevention engine

## Key Features

### Automated SOD Scoring

No manual calculation required - metrics are automatically scored:

```rust
let scorer = SodScorer::new();

let impact = ImpactMetrics {
    breaking_change: true,
    affected_systems: 5,
    user_impact: UserImpact::Significant,
    ..Default::default()
};

let sod = scorer.calculate_sod(&impact, &occurrence, &detection);
// Returns: Severity, Occurrence, Detection, and calculated RPN
```

### Schema Change Detection

Automatically detect breaking changes:

```rust
let analyzer = SchemaChangeAnalyzer::new();
let changes = analyzer.analyze_changes(&old, &new)?;

for change in changes {
    match change.change_type {
        SchemaChangeType::PropertyRemoved { property_uri, .. } => {
            println!("Breaking change: Property removed {}", property_uri);
        }
        SchemaChangeType::TypeChanged { old_type, new_type, .. } => {
            println!("Type changed: {} → {}", old_type, new_type);
        }
        _ => {}
    }
}
```

### Query Complexity Analysis

Analyze SPARQL queries for performance risks:

```rust
let analyzer = QueryPathAnalyzer::new();
let risk = analyzer.analyze_query(query, &graph)?;

println!("Complexity: {:.2}", risk.complexity_score);
println!("RPN: {}", risk.rpn.value());

if risk.complexity_score > 0.8 {
    println!("WARNING: High complexity query");
}
```

### Template Risk Assessment

Detect template transformation risks:

```rust
let analyzer = TemplateRiskAnalyzer::new();
let risk = analyzer.analyze_template(&template)?;

for dep in &risk.dependencies {
    if dep.required && dep.default_value.is_none() {
        println!("Missing required variable: {}", dep.variable_name);
    }
}
```

### Proactive Prevention

Block high-risk operations before execution:

```rust
let mut engine = FailurePreventionEngine::new();

// Add custom rule
engine.add_rule(PreventionRule {
    name: "Block critical changes".to_string(),
    description: "Prevent any change with RPN > 300".to_string(),
    rule_type: RuleType::RpnThreshold {
        threshold: 300,
        action: PreventionAction::Block("RPN too high".to_string()),
    },
});

// Validate before applying
let report = engine.validate_schema_changes(&old, &new)?;
if report.is_blocked() {
    // Don't apply the change
    return Err(anyhow!("Blocked: {}", report.summary()));
}
```

## Examples

### Complete FMEA Analysis

```rust
use ggen_core::fmea::*;

fn analyze_migration(old: &Graph, new: &Graph) -> Result<FmeaReport> {
    // Create analysis
    let mut analysis = FmeaAnalysis::new(
        "Schema Migration v1→v2".to_string(),
        "Full risk assessment".to_string(),
    );

    // Analyze schema changes
    let schema_analyzer = SchemaChangeAnalyzer::new();
    let changes = schema_analyzer.analyze_changes(old, new)?;
    let failure_modes = schema_analyzer.changes_to_failure_modes(&changes);

    for fm in failure_modes {
        analysis.add_failure_mode(fm);
    }

    // Generate report
    let report = analysis.generate_report();
    println!("Total modes: {}", report.total_modes);
    println!("Critical: {}", report.critical_count);
    println!("Max RPN: {}", report.max_rpn);

    Ok(report)
}
```

### CI/CD Integration

```rust
#[test]
fn test_no_critical_risks_in_pr() -> Result<()> {
    let old_schema = load_schema("main")?;
    let new_schema = load_schema("feature-branch")?;

    let analyzer = SchemaChangeAnalyzer::new();
    let changes = analyzer.analyze_changes(&old_schema, &new_schema)?;
    let fms = analyzer.changes_to_failure_modes(&changes);

    // Fail test if any critical risks
    for fm in fms {
        assert!(
            fm.rpn.value() < 500,
            "Critical risk detected: {} (RPN: {})",
            fm.description,
            fm.rpn.value()
        );
    }

    Ok(())
}
```

## Testing

Run FMEA module tests:

```bash
cargo test --package ggen-core --lib fmea
```

## Further Reading

- See `/docs/FMEA_ONTOLOGY_ANALYSIS.md` for complete documentation
- See `tests/` for integration examples
- See individual module docs for detailed API documentation
