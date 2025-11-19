# FMEA-Driven Ontology Failure Mode Analysis

Comprehensive FMEA (Failure Mode and Effects Analysis) system for RDF ontology changes, SPARQL query paths, and semantic code generation pipelines.

## Overview

The FMEA Ontology Analysis system provides systematic risk assessment for:

1. **RDF Schema Changes** - Breaking change detection and impact analysis
2. **SPARQL Query Paths** - Query complexity and performance risk assessment
3. **Template Transformations** - Template change risk with automated RPN calculation
4. **Proactive Failure Prevention** - Real-time validation and mitigation

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   FMEA Ontology System                       │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────┐  ┌──────────────┐  ┌─────────────────┐  │
│  │   Schema     │  │    Query     │  │   Template      │  │
│  │   Analyzer   │  │   Analyzer   │  │   Analyzer      │  │
│  └──────┬───────┘  └──────┬───────┘  └────────┬────────┘  │
│         │                  │                    │           │
│         └──────────────────┴────────────────────┘           │
│                            │                                │
│                   ┌────────▼─────────┐                      │
│                   │   SOD Scorer     │                      │
│                   │ (Automated RPN)  │                      │
│                   └────────┬─────────┘                      │
│                            │                                │
│                   ┌────────▼─────────┐                      │
│                   │    Prevention    │                      │
│                   │     Engine       │                      │
│                   └──────────────────┘                      │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## Core Concepts

### SOD Scoring (Severity-Occurrence-Detection)

Each failure mode is scored on three dimensions:

#### Severity (1-10)
- **1-2**: Minimal impact - Minor inconvenience
- **3-4**: Minor impact - Slight degradation, workaround available
- **5-6**: Moderate impact - Noticeable degradation, requires fix
- **7-8**: Major impact - Significant functional loss
- **9-10**: Critical impact - System failure, data loss, or security breach

#### Occurrence (1-10)
- **1-2**: Remote - Failure unlikely (<1 in 10,000)
- **3-4**: Low - Occasional failure (1 in 1,000)
- **5-6**: Moderate - Frequent failure (1 in 100)
- **7-8**: High - Repeated failure (1 in 10)
- **9-10**: Very High - Failure almost certain (>1 in 2)

#### Detection (1-10)
*Lower is better - indicates likelihood of detecting before harm*

- **1-2**: Almost Certain - Automated tests always detect
- **3-4**: High - Automated tests likely to detect
- **5-6**: Moderate - Manual review or testing may detect
- **7-8**: Low - Detection difficult, requires expert review
- **9-10**: Cannot Detect - No known method before deployment

### Risk Priority Number (RPN)

**RPN = Severity × Occurrence × Detection**

- **Range**: 1-1000
- **Low Risk**: 1-50
- **Medium Risk**: 51-100
- **High Risk**: 101-200
- **Very High Risk**: 201-500
- **Critical Risk**: 501-1000

**Action Thresholds**:
- RPN > 200: **Immediate action required**
- RPN > 100: **Should be addressed**
- RPN ≤ 100: **Monitor**

## Components

### 1. Schema Change Analyzer

Analyzes RDF/OWL ontology changes for breaking changes and impact.

```rust
use ggen_core::fmea::{SchemaChangeAnalyzer, SchemaChange};
use ggen_core::Graph;

let analyzer = SchemaChangeAnalyzer::new();

// Analyze changes between schemas
let changes = analyzer.analyze_changes(&old_schema, &new_schema)?;

// Convert to failure modes with SOD scoring
let failure_modes = analyzer.changes_to_failure_modes(&changes);

for fm in failure_modes {
    println!("Failure Mode: {}", fm.description);
    println!("RPN: {} (S:{} × O:{} × D:{})",
        fm.rpn.value(),
        fm.severity.value(),
        fm.occurrence.value(),
        fm.detection.value()
    );

    if fm.rpn.requires_immediate_action() {
        println!("⚠️  CRITICAL - Immediate action required!");
    }
}
```

**Detected Change Types**:
- Class removed/renamed
- Property removed/renamed/type changed
- Cardinality constraint changes
- Hierarchy modifications
- SHACL constraint changes
- Namespace changes

**Example Output**:
```
Failure Mode: Property removed: ex:userName from ex:Person
RPN: 360 (S:9 × O:5 × D:8)
⚠️  CRITICAL - Immediate action required!

Effects:
- Downstream queries will fail
- Templates referencing this property will break
- Generated code compilation errors

Mitigations:
[Planned] Implement schema versioning and migration path (reduces S by 3)
[Planned] Add automated breaking change detection in CI (reduces D by 5)
```

### 2. Query Path Analyzer

Analyzes SPARQL queries for complexity and performance risks with SOD scoring.

```rust
use ggen_core::fmea::QueryPathAnalyzer;

let analyzer = QueryPathAnalyzer::new();

let query = r#"
    PREFIX ex: <http://example.org/>
    SELECT ?person ?name
    WHERE {
        ?person ex:name ?name .
        OPTIONAL { ?person ex:age ?age }
        OPTIONAL { ?person ex:email ?email }
        FILTER (?age > 18)
    }
"#;

let risk = analyzer.analyze_query(query, &graph)?;

println!("Query Complexity: {:.2}", risk.complexity_score);
println!("RPN: {}", risk.rpn.value());

for risk_desc in &risk.risks {
    println!("⚠️  {}", risk_desc);
}

// Convert to failure mode
let failure_mode = analyzer.query_risk_to_failure_mode(&risk);
```

**Analysis Factors**:
- Query type (SELECT, CONSTRUCT, ASK, DESCRIBE)
- Number of OPTIONAL patterns
- UNION branches
- Subqueries
- Aggregation functions
- FILTER clauses
- Estimated complexity score (0.0-1.0)

**Risk Detection**:
```
Query Complexity: 0.85
RPN: 280 (S:7 × O:5 × D:8)

Risks:
⚠️  High query complexity may impact performance
⚠️  Multiple subqueries may cause timeout
⚠️  Many OPTIONAL clauses can lead to cartesian products

Mitigations:
[Planned] Optimize query by reducing subqueries (reduces S by 3)
[Planned] Add query timeout and resource limits (reduces D by 2)
[Planned] Implement query result caching (reduces O by 4)
```

### 3. Template Risk Analyzer

Analyzes template transformations with automated RPN calculation.

```rust
use ggen_core::fmea::TemplateRiskAnalyzer;
use ggen_core::Template;

let analyzer = TemplateRiskAnalyzer::new();

// Analyze template risks
let risk = analyzer.analyze_template(&template)?;

println!("Template: {}", risk.template_name);
println!("Complexity: {:.2}", risk.complexity_score);
println!("RPN: {}", risk.rpn.value());

// Check variable dependencies
for dep in &risk.dependencies {
    if dep.required && dep.default_value.is_none() {
        println!("⚠️  Missing required variable: {}", dep.variable_name);
    }
}

// Convert to failure mode
let failure_mode = analyzer.template_risk_to_failure_mode(&risk);
```

**Analyzed Elements**:
- Template complexity (lines, nesting, constructs)
- Variable dependencies
- Include directives
- Loop constructs
- Conditional logic
- Circular dependencies

**Complexity Factors**:
- Template size (lines of code)
- Number of includes (×0.1)
- Number of loops (×0.15)
- Number of conditionals (×0.1)
- Nesting depth (×0.1)

**Example Analysis**:
```
Template: user_profile.tmpl
Complexity: 0.72
RPN: 210 (S:7 × O:6 × D:5)

Variable Dependencies:
✓ username (required, has default)
⚠️  email (required, no default)
✓ avatar_url (optional)

Risks:
⚠️  High template complexity may make maintenance difficult
⚠️  Missing required variable: email

Mitigations:
[Planned] Refactor template to reduce complexity (reduces S by 2)
[Planned] Add comprehensive template tests (reduces D by 4)
[Planned] Implement template validation in CI (reduces D by 3)
```

### 4. Failure Prevention Engine

Proactive validation and mitigation before execution.

```rust
use ggen_core::fmea::FailurePreventionEngine;

let mut engine = FailurePreventionEngine::new();

// Validate schema changes
let report = engine.validate_schema_changes(&old_schema, &new_schema)?;

if report.is_blocked() {
    println!("❌ Schema changes blocked:");
    for blocker in &report.blockers {
        println!("  - {}", blocker);
    }
    return Err(anyhow!("Schema validation failed"));
}

if report.has_warnings() {
    println!("⚠️  Warnings:");
    for warning in &report.warnings {
        println!("  - {}", warning);
    }
}

// Validate query before execution
let query_report = engine.validate_query(query, &graph)?;
if !query_report.is_blocked() {
    let results = graph.query(query)?; // Safe to execute
}

// Validate template before rendering
let template_report = engine.validate_template(&template)?;
if !template_report.is_blocked() {
    let output = pipeline.render(&template)?; // Safe to render
}

// Get prevention statistics
let stats = engine.get_statistics();
println!("Block rate: {:.1}%", stats.block_rate() * 100.0);
```

**Prevention Rules**:

1. **RPN Threshold** - Block operations with RPN > 500
2. **Complexity Threshold** - Warn when complexity > 0.8
3. **Breaking Change Detection** - Block breaking schema changes without migration
4. **Query Timeout** - Prevent queries likely to timeout
5. **Template Validation** - Block templates with circular dependencies

**Prevention Report**:
```
Schema Change Validation
Status: ❌ BLOCKED
Timestamp: 2025-11-19T10:30:00Z

Blockers:
- High-risk schema change: Property removed (RPN: 360)
- No migration path provided for breaking change

Warnings:
- 5 downstream queries affected
- 3 templates may require updates
```

## Complete FMEA Workflow

### Step 1: Analyze Changes

```rust
use ggen_core::fmea::*;

// Create analyzers
let schema_analyzer = SchemaChangeAnalyzer::new();
let query_analyzer = QueryPathAnalyzer::new();
let template_analyzer = TemplateRiskAnalyzer::new();

// Analyze schema changes
let schema_changes = schema_analyzer.analyze_changes(&old, &new)?;
let schema_fms = schema_analyzer.changes_to_failure_modes(&schema_changes);

// Analyze queries
let query_risk = query_analyzer.analyze_query(query, &graph)?;
let query_fm = query_analyzer.query_risk_to_failure_mode(&query_risk);

// Analyze templates
let template_risk = template_analyzer.analyze_template(&template)?;
let template_fm = template_analyzer.template_risk_to_failure_mode(&template_risk);
```

### Step 2: Build FMEA Analysis

```rust
let mut analysis = FmeaAnalysis::new(
    "Ontology v3.0 → v4.0 Migration".to_string(),
    "Risk assessment for major version upgrade".to_string(),
);

// Add all failure modes
for fm in schema_fms {
    analysis.add_failure_mode(fm);
}
analysis.add_failure_mode(query_fm);
analysis.add_failure_mode(template_fm);

// Sort by risk priority
let sorted = analysis.sorted_by_rpn();

// Get critical items
let critical = analysis.critical_modes();
println!("Critical failure modes: {}", critical.len());

for fm in critical {
    println!("  [RPN: {}] {}", fm.rpn.value(), fm.description);
}
```

### Step 3: Generate FMEA Report

```rust
let report = analysis.generate_report();

println!("FMEA Report: {}", report.metadata.name);
println!("Total Modes: {}", report.total_modes);
println!("Critical: {}", report.critical_count);
println!("Max RPN: {}", report.max_rpn);
println!("Avg RPN: {:.1}", report.avg_rpn);

println!("\nTop Risks:");
for (i, risk) in report.top_risks.iter().enumerate() {
    println!("{}. {}", i + 1, risk);
}

println!("\nBy Category:");
for (category, count) in &report.modes_by_category {
    println!("  {}: {}", category, count);
}
```

### Step 4: Apply Prevention

```rust
let mut prevention = FailurePreventionEngine::new();

// Pre-deployment validation
let validation = prevention.validate_schema_changes(&old, &new)?;

if validation.is_blocked() {
    // Halt deployment
    eprintln!("Deployment blocked: {}", validation.summary());
    return Err(anyhow!("FMEA validation failed"));
}

if validation.has_warnings() {
    // Log warnings but proceed
    warn!("Deployment warnings: {}", validation.summary());
}

// Proceed with deployment
deploy_changes()?;
```

## Integration with Existing Systems

### With Lifecycle Validation

```rust
use ggen_core::{ReadinessTracker, ReadinessCategory};
use ggen_core::fmea::FailurePreventionEngine;

let mut tracker = ReadinessTracker::new();
let mut prevention = FailurePreventionEngine::new();

// Add FMEA validation as readiness check
let report = prevention.validate_schema_changes(&old, &new)?;

if report.is_blocked() {
    tracker.require(
        ReadinessCategory::Security,
        "FMEA validation",
        "Critical risks detected",
    )?;
} else {
    tracker.mark_ready(ReadinessCategory::Security, "FMEA validation")?;
}
```

### With CI/CD Pipeline

```yaml
# .github/workflows/fmea-validation.yml
name: FMEA Validation

on:
  pull_request:
    paths:
      - 'ontologies/**'
      - 'templates/**'

jobs:
  fmea-analysis:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3

      - name: Run FMEA Analysis
        run: |
          cargo run --bin fmea-analyze \
            --old-schema ontologies/v3.ttl \
            --new-schema ontologies/v4.ttl \
            --threshold 200

      - name: Check for Critical Risks
        run: |
          if [ $? -ne 0 ]; then
            echo "❌ FMEA validation failed - critical risks detected"
            exit 1
          fi
```

## Automated RPN Calculation Examples

### Schema Change: Property Removed

```rust
// Input: Property ex:userName removed from ex:Person class

// Automated scoring:
// Severity: 9 (breaking change, affects downstream systems)
// Occurrence: 5 (properties removed occasionally during refactoring)
// Detection: 8 (difficult to detect without comprehensive tests)

// RPN = 9 × 5 × 8 = 360 (CRITICAL)

// Automated mitigations:
// 1. Schema versioning (reduces S by 3)
// 2. Breaking change detection in CI (reduces D by 5)
// 3. Audit affected queries (reduces O by 2)

// Revised RPN = 6 × 3 × 3 = 54 (LOW)
```

### Query: High Complexity

```rust
// Input: Query with 3 subqueries, 5 OPTIONALs, 2 UNIONs

// Complexity score: 0.85

// Automated scoring:
// Severity: 7 (performance impact, possible timeouts)
// Occurrence: 6 (complex queries written moderately often)
// Detection: 5 (can be detected with performance testing)

// RPN = 7 × 6 × 5 = 210 (VERY HIGH)

// Automated mitigations:
// 1. Query optimization (reduces S by 3)
// 2. Query timeout limits (reduces D by 2)
// 3. Result caching (reduces O by 4)

// Revised RPN = 4 × 2 × 3 = 24 (LOW)
```

### Template: High Complexity with Missing Variables

```rust
// Input: Template with nesting depth 4, 3 loops, missing required variable

// Complexity score: 0.72

// Automated scoring:
// Severity: 7 (maintenance issues, runtime failures)
// Occurrence: 6 (complex templates common in large projects)
// Detection: 5 (can be caught with template validation)

// RPN = 7 × 6 × 5 = 210 (VERY HIGH)

// Automated mitigations:
// 1. Refactor to reduce complexity (reduces S by 2)
// 2. Comprehensive template tests (reduces D by 4)
// 3. CI validation (reduces D by 3)

// Revised RPN = 5 × 6 × 1 = 30 (LOW)
```

## Best Practices

### 1. Continuous Risk Assessment

```rust
// Run FMEA analysis on every schema change
#[test]
fn test_schema_changes_meet_fmea_standards() {
    let analyzer = SchemaChangeAnalyzer::new();
    let changes = analyzer.analyze_changes(&old, &new).unwrap();
    let fms = analyzer.changes_to_failure_modes(&changes);

    // Ensure no critical risks
    for fm in fms {
        assert!(fm.rpn.value() < 500, "Critical risk detected: {}", fm.description);
    }
}
```

### 2. Preventive Validation

```rust
// Validate before deployment
fn deploy_schema(new_schema: &Graph) -> Result<()> {
    let mut prevention = FailurePreventionEngine::new();
    let current = load_current_schema()?;

    let report = prevention.validate_schema_changes(&current, new_schema)?;

    if report.is_blocked() {
        return Err(anyhow!("FMEA validation failed: {}", report.summary()));
    }

    // Safe to deploy
    apply_schema(new_schema)
}
```

### 3. Track and Learn

```rust
// Maintain FMEA history for learning
let stats = prevention.get_statistics();

println!("Prevention Metrics:");
println!("  Total validations: {}",
    stats.total_schema_validations +
    stats.total_query_validations +
    stats.total_template_validations
);
println!("  Block rate: {:.1}%", stats.block_rate() * 100.0);
println!("  Most common issues: ...");
```

## Performance Considerations

### Caching
- Schema analysis results are cached
- Query complexity scores cached by query hash
- Template risk scores cached by template hash

### Async Support
All analyzers support async operation:

```rust
let risk = analyzer.analyze_query_async(query, &graph).await?;
```

### Batch Analysis
Analyze multiple items in parallel:

```rust
use futures::future::join_all;

let futures: Vec<_> = queries.iter()
    .map(|q| analyzer.analyze_query_async(q, &graph))
    .collect();

let risks = join_all(futures).await;
```

## Summary

The FMEA Ontology Analysis system provides:

✅ **Systematic Risk Assessment** - SOD scoring for all changes
✅ **Automated RPN Calculation** - No manual calculation required
✅ **Proactive Prevention** - Block high-risk changes before deployment
✅ **Comprehensive Coverage** - Schema, queries, and templates
✅ **Integration Ready** - Works with existing validation systems
✅ **CI/CD Compatible** - Automated validation in pipelines
✅ **Learning System** - Tracks metrics for continuous improvement

**Key Metrics**:
- Risk Priority Number (RPN): 1-1000
- Action threshold: RPN > 200
- Typical reduction: 80-90% with mitigations
- Block rate: <5% for well-maintained systems
