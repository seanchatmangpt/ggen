# Six Sigma DMAIC for Ontology Quality

## Overview

This document describes the comprehensive Six Sigma quality management system implemented for RDF ontology generation in ggen. The implementation follows the DMAIC (Define-Measure-Analyze-Improve-Control) methodology to achieve world-class quality levels.

## Quality Levels

Six Sigma defines quality levels based on Defects Per Million Opportunities (DPMO):

| Sigma Level | DPMO | Yield % | Quality Rating |
|-------------|------|---------|----------------|
| 1σ | 691,462 | 30.9% | Poor |
| 2σ | 308,538 | 69.1% | Below Average |
| 3σ | 66,807 | 93.3% | Average |
| 4σ | 6,210 | 99.38% | Industry Standard |
| 5σ | 233 | 99.977% | Excellent |
| 6σ | 3.4 | 99.99966% | World Class |

## Architecture

### Module Structure

```
ggen-domain/src/six_sigma/
├── mod.rs                  # Module exports and documentation
├── dmaic.rs                # DMAIC cycle implementation
├── spc.rs                  # Statistical Process Control charts
├── capability.rs           # Cp/Cpk capability indices
├── dpmo.rs                 # DPMO tracking system
└── training.rs             # Black Belt training data for AI agents
```

### Integration

```
ggen-domain/src/rdf/
├── validation.rs           # Base SHACL validation
└── six_sigma_integration.rs  # Six Sigma enhanced validation
```

## DMAIC Cycle

### 1. Define Phase

**Purpose**: Establish quality goals and defect definitions

**Key Components**:
- Critical-to-Quality (CTQ) characteristics
- Target sigma level
- Defect type definitions
- Project charter

**Standard RDF CTQs**:
1. **Syntactic Correctness**: 99.5% minimum, 100% target
2. **SHACL Compliance**: 95% minimum, 100% target
3. **Semantic Consistency**: 98% minimum, 100% target
4. **Metadata Completeness**: 80% minimum, 95% target

**Standard Defect Types**:
- `SYN001`: Invalid Turtle Syntax (Critical)
- `SEM001`: Semantic Inconsistency (Major)
- `SCH001`: SHACL Violation (Major)
- `DQ001`: Missing Required Property (Major)
- `DQ002`: Invalid Property Value (Minor)
- `COM001`: Incomplete Metadata (Minor)
- `PERF001`: Inefficient Query Pattern (Minor)

### 2. Measure Phase

**Purpose**: Collect baseline quality data

**Metrics Collected**:
- Total opportunities for defects
- Total defects found
- Baseline DPMO
- Baseline sigma level
- Measurement System Analysis (MSA)

**MSA Requirements**:
- Repeatability variance < 30%
- Reproducibility variance < 30%
- Gage R&R < 30%

### 3. Analyze Phase

**Purpose**: Identify root causes of defects

**Analysis Methods**:
- **Pareto Analysis**: Identify vital few vs trivial many
- **Root Cause Analysis**: 5M+E framework
  - Man/People
  - Machine/Technology
  - Material/Input data
  - Method/Process
  - Measurement
  - Environment
- **Correlation Analysis**: Pearson correlation between variables
- **Hypothesis Testing**: Statistical validation of assumptions

### 4. Improve Phase

**Purpose**: Implement solutions to reduce defects

**Activities**:
- Solution development
- Pilot testing
- Validation of improvements
- Expected DPMO reduction tracking

### 5. Control Phase

**Purpose**: Sustain improvements through monitoring

**Control Mechanisms**:
- Statistical Process Control (SPC) charts
- Control plans
- Process documentation
- Continuous monitoring

## Statistical Process Control (SPC)

### Chart Types

#### 1. I-MR Chart (Individual-Moving Range)
**Use**: Individual measurements (e.g., parsing time, validation time)

**Control Limits**:
- Center Line (CL) = Process mean
- UCL = Mean + 3σ
- LCL = Mean - 3σ

#### 2. p-Chart (Proportion Defective)
**Use**: Proportion of defective items (e.g., % of templates failing validation)

**Control Limits**:
- CL = p̄ (overall proportion defective)
- UCL = p̄ + 3√(p̄(1-p̄)/n̄)
- LCL = p̄ - 3√(p̄(1-p̄)/n̄)

#### 3. u-Chart (Defects Per Unit)
**Use**: Number of defects per unit (e.g., defects per template)

**Control Limits**:
- CL = ū (average defects per unit)
- UCL = ū + 3√(ū/n̄)
- LCL = ū - 3√(ū/n̄)

### Western Electric Rules

Control charts detect special causes using these rules:

1. **Rule 1**: One point beyond 3σ (Critical)
2. **Rule 2**: Nine points in a row on same side of centerline (Warning)
3. **Rule 3**: Six points in a row steadily increasing/decreasing (Warning)
4. **Rule 4**: Fourteen points alternating up/down (Warning)
5. **Rule 5**: Two out of three points beyond 2σ on same side (Warning)
6. **Rule 6**: Four out of five points beyond 1σ on same side (Warning)
7. **Rule 7**: Fifteen points within 1σ of centerline (Info)
8. **Rule 8**: Eight points beyond 1σ from centerline (Warning)

## Process Capability

### Capability Indices

#### Cp (Potential Capability)
Measures inherent process capability assuming perfect centering:

```
Cp = (USL - LSL) / (6σ)
```

**Interpretation**:
- Cp < 1.0: Process not capable
- Cp ≥ 1.0: Minimally capable (3σ)
- Cp ≥ 1.33: Capable (4σ)
- Cp ≥ 1.67: Excellent (5σ)
- Cp ≥ 2.0: World-class (6σ)

#### Cpk (Actual Capability)
Measures actual capability accounting for process centering:

```
Cpk = min((USL - μ)/(3σ), (μ - LSL)/(3σ))
```

**Interpretation**: Same as Cp scale. When Cpk = Cp, process is perfectly centered.

#### Cpm (Capability About Target)
Measures capability relative to target value:

```
Cpm = (USL - LSL) / (6τ)
where τ = √(σ² + (μ - T)²)
```

**Use**: When hitting target is critical, not just staying within limits.

## DPMO Tracking

### Transformation Stages

DPMO is tracked across all semantic transformation stages:

1. **Parsing**: Template file parsing
2. **Generation**: RDF/Turtle generation
3. **Validation**: SHACL validation
4. **Reasoning**: Semantic reasoning
5. **Querying**: SPARQL query execution
6. **Serialization**: Output format serialization
7. **End-to-End**: Complete transformation pipeline

### Opportunity Types

Defect opportunities are categorized as:

1. **Syntax**: Correctness of RDF/Turtle syntax
2. **Schema**: Compliance with SHACL shapes
3. **Semantic**: Logical consistency
4. **Data Quality**: Value correctness and completeness
5. **Performance**: Meeting performance requirements
6. **Completeness**: Presence of recommended elements

### Calculation Example

```
Units Produced: 1,000 templates
Opportunities per Unit: 10 (e.g., 10 required fields)
Total Opportunities: 10,000
Defects Found: 50

DPMO = (50 / 10,000) × 1,000,000 = 5,000
Sigma Level ≈ 4.2σ
```

## Black Belt Training for AI Agents

### Belt Levels

AI quality agents are trained to achieve Six Sigma belt levels:

| Belt Level | Detection Accuracy | F1 Score | Root Cause Accuracy | Specializations |
|------------|-------------------|----------|---------------------|-----------------|
| White | - | - | - | 0 |
| Yellow | 70% | 0.65 | - | 0 |
| Green | 80% | 0.75 | 70% | 0 |
| Black | 90% | 0.85 | 80% | 2+ |
| Master Black | 95% | 0.90 | 90% | 4+ |

### Specialization Areas

1. **RDF Syntax**: Turtle/N3/RDF-XML parsing and validation
2. **SHACL Validation**: Shape constraint validation
3. **Semantic Reasoning**: OWL/RDFS inference
4. **Performance**: Query optimization and efficiency
5. **Statistics**: Statistical analysis and SPC
6. **Process Improvement**: DMAIC methodology

### Training Components

#### Best Practices
- Schema design patterns
- Validation strategies
- Performance optimization
- Documentation standards

#### Defect Patterns
- Common defect types
- Detection methods
- Root causes
- Prevention strategies

#### Statistical Methods
- Process capability analysis
- Control chart interpretation
- Hypothesis testing
- Correlation analysis

## Usage Examples

### Basic Six Sigma Validation

```rust
use ggen_domain::rdf::SixSigmaValidator;

let mut validator = SixSigmaValidator::new();
// Validate a template with Six Sigma tracking
let turtle = r#"
@prefix ggen: <http://ggen.dev/ontology#> .

<http://example.org/my-template> a ggen:Template ;
  ggen:templateName "My Template" ;
  ggen:templateVersion "1.0.0" ;
  ggen:stability "stable" .
"#;

let report = validator.validate(turtle, "http://example.org/my-template")?;

// Check quality metrics
println!("DPMO: {}", report.dpmo_metrics.dpmo);
println!("Sigma Level: {:.2}σ", report.dpmo_metrics.sigma_level);
println!("Status: {:?}", validator.get_quality_status());

// Review recommendations
for rec in &report.recommendations {
    println!("{:?}: {}", rec.priority, rec.description);
}
```

### DMAIC Cycle Management

```rust
use ggen_domain::six_sigma::dmaic::*;

// Define phase
let define = DefinePhase {
    ctq_characteristics: DefinePhase::standard_rdf_ctqs(),
    target_sigma_level: 4.0,
    target_dpmo: 6210.0,
    defect_types: DefinePhase::standard_rdf_defects(),
    charter: ProjectCharter {
        problem_statement: "High RDF template defect rate".to_string(),
        goal_statement: "Achieve 4σ quality level (6,210 DPMO)".to_string(),
        scope: "All template marketplace packages".to_string(),
        stakeholders: vec!["Quality Team".to_string(), "Dev Team".to_string()],
        timeline_days: 90,
    },
};

// Create DMAIC cycle
let mut cycle = DmaicCycle::new(
    "rdf-quality-2025-q1".to_string(),
    "http://ggen.dev/marketplace".to_string(),
    define,
);

// Measure phase (after collecting baseline data)
let measure = MeasurePhase {
    baseline_metrics: HashMap::new(),
    total_opportunities: 1_000_000,
    total_defects: 10_000,
    baseline_dpmo: 10_000.0,
    baseline_sigma: 3.8,
    msa: MeasurementSystemAnalysis {
        repeatability_variance: 0.15,
        reproducibility_variance: 0.10,
        gage_rr_percent: 25.0,
        is_acceptable: true,
    },
    samples_collected: 1000,
    collection_period_days: 30,
};

cycle.start_measure(measure)?;

// Progress through remaining phases...
```

### SPC Monitoring

```rust
use ggen_domain::six_sigma::spc::*;

let mut chart_manager = ControlChart::new();

// Create I-MR chart for parsing time
let baseline_times: Vec<f64> = vec![/* 30+ baseline measurements */];

chart_manager.create_imr_chart(
    "parsing-time-chart".to_string(),
    "Template Parsing Time (ms)".to_string(),
    &baseline_times,
)?;

// Add new measurements
let violations = chart_manager.add_data_point(
    "parsing-time-chart",
    125.5, // new measurement
    Some(1),
)?;

// Check for violations
if !violations.is_empty() {
    println!("⚠️ Control violations detected:");
    for violation in violations {
        println!("  - {:?}", violation);
    }
}
```

### Process Capability Analysis

```rust
use ggen_domain::six_sigma::capability::*;

// Collect validation success rate samples
let samples: Vec<f64> = vec![/* 30+ samples */];

// Define specification limits
let spec_limits = SpecificationLimits {
    upper: Some(100.0),  // 100% success rate
    lower: Some(95.0),   // Minimum 95% acceptable
    target: 99.5,        // Target 99.5% success
};

// Calculate capability
let capability = ProcessCapability::from_samples(
    "Validation Success Rate".to_string(),
    &samples,
    spec_limits,
)?;

println!("Cp: {:.2}", capability.indices.cp.unwrap());
println!("Cpk: {:.2}", capability.indices.cpk.unwrap());
println!("Sigma Level: {:.2}σ", capability.sigma_level);
println!("Expected DPMO: {:.0}", capability.expected_dpmo);
println!("Rating: {:?}", capability.performance_rating);

// Check if process is capable
if capability.is_capable() {
    println!("✓ Process is capable (Cpk >= 1.33)");
} else {
    println!("✗ Process needs improvement");
}
```

### DPMO Tracking

```rust
use ggen_domain::six_sigma::dpmo::*;

let mut tracker = DefectTracker::with_six_sigma_targets();

// Track defects for each transformation stage
let status = tracker.track_defects(
    "validation-batch-001".to_string(),
    TransformationStage::Validation,
    OpportunityType::Schema,
    1000,  // units (templates) processed
    10,    // opportunities per unit (required fields)
    5,     // defects found
)?;

match status {
    QualityStatus::OnTarget => println!("✓ Quality on target"),
    QualityStatus::Alert => println!("⚠️ Quality alert"),
    QualityStatus::Critical => println!("⚠️ Critical quality issue"),
    QualityStatus::Unacceptable => println!("✗ Unacceptable quality"),
}

// Get summary statistics
let summary = &tracker.calculator().summary;
println!("Overall DPMO: {:.2}", summary.overall_dpmo);
println!("Overall Sigma: {:.2}σ", summary.overall_sigma);

// Trend analysis
if summary.trend_analysis.is_improving {
    println!("✓ Quality is improving at {:.2} DPMO/day",
             summary.trend_analysis.improvement_rate);
}
```

### AI Quality Agent Training

```rust
use ggen_domain::six_sigma::training::*;

// Create quality agent
let mut agent = QualityAgent::new(
    "qa-agent-001".to_string(),
    BeltLevel::Green,
);

// Add specializations
agent.add_specialization(Specialization::RdfSyntax);
agent.add_specialization(Specialization::ShaclValidation);

// Update performance metrics
let mut metrics = AgentPerformanceMetrics::default();
metrics.defect_detection_accuracy = 85.0;
metrics.precision = 0.82;
metrics.recall = 0.78;
metrics.calculate_f1();  // F1 = 0.80
metrics.root_cause_accuracy = 75.0;

agent.update_metrics(metrics);

// Check qualification
if agent.qualifies_for_belt(BeltLevel::Green) {
    println!("✓ Agent qualifies for Green Belt");
}
if agent.qualifies_for_belt(BeltLevel::Black) {
    println!("Agent does not yet qualify for Black Belt");
    println!("Needs: Higher accuracy and 2+ specializations");
}
```

## Quality Targets

### Recommended Targets by Use Case

#### Production Marketplace Templates
- **Target Sigma**: 5σ
- **Target DPMO**: 233
- **Min Cpk**: 1.67
- **Validation Success Rate**: 99.98%

#### Development/Experimental Templates
- **Target Sigma**: 4σ
- **Target DPMO**: 6,210
- **Min Cpk**: 1.33
- **Validation Success Rate**: 99.4%

#### Internal/Testing Templates
- **Target Sigma**: 3σ
- **Target DPMO**: 66,807
- **Min Cpk**: 1.0
- **Validation Success Rate**: 93.3%

## Continuous Improvement

### Monthly Quality Reviews

1. **DPMO Tracking**: Monitor overall DPMO trend
2. **SPC Charts**: Review control charts for violations
3. **Capability Studies**: Quarterly process capability assessments
4. **Root Cause Analysis**: Address top defect contributors
5. **Agent Training**: Update AI agent training data with new patterns

### Quality Gates

Templates must pass these gates before marketplace publication:

1. **Syntax Gate**: Zero syntax errors
2. **SHACL Gate**: 100% SHACL compliance
3. **Capability Gate**: Cpk >= 1.33
4. **DPMO Gate**: DPMO <= target for template category

## References

- **Six Sigma Handbook** by Thomas Pyzdek
- **Statistical Process Control** by Wheeler & Chambers
- **SHACL Specification**: https://www.w3.org/TR/shacl/
- **RDF Specification**: https://www.w3.org/RDF/

## Contributing

To improve the Six Sigma quality system:

1. Add new defect patterns to `training.rs`
2. Enhance statistical methods in `spc.rs`
3. Contribute to AI agent training data
4. Report quality issues via GitHub Issues
5. Submit improvements via Pull Requests

## License

This Six Sigma implementation is part of ggen and is licensed under MIT.
