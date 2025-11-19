# Workload Leveling System (Mura + Heijunka)

## Overview

The ggen workload leveling system implements Toyota Production System principles to optimize template generation workflows. It provides smooth workload distribution, predictable cadence, and variability reduction across polyglot code generation targets.

## Core Principles

### Mura (Unevenness) Reduction

**Mura** refers to unevenness or irregularity in production. The Mura leveling module eliminates workload imbalances through:

- **Balanced Distribution**: Even workload across language targets (Rust, TypeScript, JavaScript, Python, Go)
- **Batch Size Leveling**: Standardized batch sizes with minimal variance
- **Variability Tracking**: Real-time monitoring of generation latency fluctuations
- **Coefficient of Variation (CV)**: Statistical measure of workload balance (target: CV < 0.3)

### Heijunka (Level Scheduling)

**Heijunka** means production leveling or smoothing. The Heijunka scheduler provides:

- **Takt Time Management**: Fixed time boxes for batch processing
- **Predictable Cadence**: Consistent rhythm for CI/CD pipelines
- **Schedule Adherence**: Tracking on-time batch completion
- **Standardized Work Sequences**: Repeatable RDF transformation workflows

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Leveled Generator                         │
│  ┌────────────────┐          ┌─────────────────────┐        │
│  │ Template       │          │ Workload Leveler    │        │
│  │ Discovery      │──────────▶ (Mura)              │        │
│  └────────────────┘          │                     │        │
│           │                  │ • Target Distribution│        │
│           │                  │ • Batch Leveling     │        │
│           ▼                  │ • CV Tracking        │        │
│  ┌────────────────┐          └─────────────────────┘        │
│  │ Target         │                      │                  │
│  │ Distribution   │◀─────────────────────┘                  │
│  └────────────────┘                                          │
│           │                                                  │
│           ▼                                                  │
│  ┌────────────────┐          ┌─────────────────────┐        │
│  │ Heijunka       │          │ Batch Processing    │        │
│  │ Scheduler      │──────────▶                     │        │
│  │                │          │ • RDF Operations     │        │
│  │ • Batch Sched. │          │ • SPARQL Queries     │        │
│  │ • Takt Time    │          │ • Template Render    │        │
│  │ • Time Boxing  │          │ • File Generation    │        │
│  └────────────────┘          └─────────────────────┘        │
│                                                              │
│  ┌─────────────────────────────────────────────────┐        │
│  │         Metrics & Reporting                      │        │
│  │  • Workload Imbalance (CV)                       │        │
│  │  • Schedule Adherence                            │        │
│  │  • Throughput (templates/sec)                    │        │
│  │  • Latency Variability                           │        │
│  └─────────────────────────────────────────────────┘        │
└─────────────────────────────────────────────────────────────┘
```

## Components

### 1. Mura Leveling Module

**Location**: `crates/ggen-core/src/lifecycle/mura_leveling.rs`

**Key Types**:
- `WorkloadLeveler` - Main workload distribution coordinator
- `MuraConfig` - Configuration for leveling parameters
- `TargetWorkload` - Metrics for individual language targets
- `BatchMetrics` - Per-batch performance tracking

**Features**:
```rust
use ggen_core::lifecycle::{WorkloadLeveler, MuraConfig, TargetWorkload};

let config = MuraConfig {
    target_batch_size: 10,
    max_batch_variance: 0.2,  // 20% variance allowed
    target_cv: 0.3,           // Target CV of 0.3
    auto_adjust_batches: true,
    min_templates_per_target: 3,
};

let mut leveler = WorkloadLeveler::new(config);
leveler.start();

// Distribute templates across targets
let targets = vec!["rust".to_string(), "typescript".to_string()];
let distribution = leveler.distribute_templates(items, &targets);

// Check balance
let imbalance = leveler.calculate_imbalance();
let is_balanced = leveler.is_balanced(); // CV < 0.3

leveler.report(); // Generate detailed report
```

**Metrics**:
- **Coefficient of Variation (CV)**: Relative variability measure
  - CV < 0.3: Balanced workload ✅
  - CV 0.3-0.5: Acceptable variability 👍
  - CV > 0.5: High imbalance ⚠️
- **Throughput**: Templates processed per second
- **Batch Size Variance**: Deviation from target batch sizes

### 2. Heijunka Scheduler Module

**Location**: `crates/ggen-core/src/lifecycle/heijunka_scheduler.rs`

**Key Types**:
- `HeijunkaScheduler<T>` - Level scheduling coordinator
- `HeijunkaConfig` - Takt time and batch size configuration
- `ScheduledBatch<T>` - Time-boxed batch with metrics
- `StandardWorkSequence` - Repeatable RDF transformation workflows

**Features**:
```rust
use ggen_core::lifecycle::{HeijunkaScheduler, HeijunkaConfig};
use std::time::Duration;

// Fast CI/CD configuration
let config = HeijunkaConfig::fast_cicd();
// Or heavy ontology processing
let config = HeijunkaConfig::heavy_ontology();

let mut scheduler = HeijunkaScheduler::new(config);

// Schedule batches with level loading
scheduler.schedule_batches(items);

// Process batches with takt time adherence
while let Some(mut batch) = scheduler.next_batch() {
    // Process batch items...
    scheduler.complete_batch(batch);
}

// Check schedule adherence
let adherence = scheduler.schedule_adherence(); // 0.0 = perfect
scheduler.report(); // Generate scheduling report
```

**Configurations**:

| Configuration     | Takt Time | Batch Size | Use Case                    |
|------------------|-----------|------------|------------------------------|
| `default()`      | 10s       | 5-20       | Standard template generation |
| `fast_cicd()`    | 5s        | 3-15       | CI/CD pipelines              |
| `heavy_ontology()`| 30s      | 10-50      | Large RDF processing         |

**Standard Work Sequences**:
```rust
use ggen_core::lifecycle::{StandardWorkSequence, WorkStep};

let mut sequence = StandardWorkSequence::new("RDF Transform");

sequence.add_step(WorkStep::new("Parse RDF", Duration::from_millis(100))
    .with_description("Load and parse RDF/Turtle files"));
sequence.add_step(WorkStep::new("Execute SPARQL", Duration::from_millis(200))
    .with_description("Run SPARQL queries on graph"));
sequence.add_step(WorkStep::new("Render Template", Duration::from_millis(150))
    .with_description("Render template with results"));

let result = sequence.execute()?;
assert!(result.met_target());
```

### 3. Leveled Generator

**Location**: `crates/ggen-core/src/leveled_generator.rs`

**Key Types**:
- `LeveledGenerator` - Enhanced streaming generator with leveling
- `LeveledGeneratorConfig` - Combined Mura + Heijunka configuration
- `LeveledGenerationResult` - Comprehensive generation metrics
- `TemplateItem` - Template with target and RDF metadata

**Usage**:
```rust
use ggen_core::{LeveledGenerator, LeveledGeneratorConfig};
use tera::Context;
use std::path::PathBuf;

let config = LeveledGeneratorConfig::fast_cicd();

let mut generator = LeveledGenerator::new(
    config,
    PathBuf::from("templates"),
    PathBuf::from("output"),
)?;

let mut vars = Context::new();
vars.insert("project_name", "MyApp");

let result = generator.generate_all(&vars)?;

// Print comprehensive reports
result.report();
generator.report();

println!("Throughput: {:.2} templates/sec", result.throughput());
println!("Balance: {}", if result.is_balanced { "✅" } else { "⚠️" });
```

## Integration with CI/CD Pipelines

### Predictable Cadence

The Heijunka scheduler ensures predictable execution times for CI/CD workflows:

```yaml
# .github/workflows/generate.yml
name: Template Generation

on: [push, pull_request]

jobs:
  generate:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Generate with Heijunka Leveling
        run: |
          cargo run --release -- generate \
            --config heijunka-fast-cicd \
            --templates ./templates \
            --output ./generated

      - name: Validate Generation Metrics
        run: |
          # Check for balanced workload (CV < 0.3)
          # Verify schedule adherence
          # Ensure throughput targets met
```

### Performance Targets

| Metric                | Target   | Measurement               |
|----------------------|----------|---------------------------|
| Template Throughput  | >10/sec  | Templates per second      |
| Workload Imbalance   | CV < 0.3 | Coefficient of Variation  |
| Schedule Adherence   | < 0.1    | Variance from takt time   |
| Batch Variance       | < 20%    | Batch size deviation      |

## RDF Batch Processing

### Standardized Transformation Sequences

```rust
use ggen_core::lifecycle::StandardWorkSequence;

// Define standard RDF transformation workflow
let sequence = StandardWorkSequence::new("Ontology to Code")
    .add_step(WorkStep::new("Load Ontology", Duration::from_secs(1)))
    .add_step(WorkStep::new("Validate Schema", Duration::from_millis(500)))
    .add_step(WorkStep::new("Execute Queries", Duration::from_secs(2)))
    .add_step(WorkStep::new("Transform Results", Duration::from_secs(1)))
    .add_step(WorkStep::new("Generate Code", Duration::from_secs(2)));

let result = sequence.execute()?;

if result.met_target() {
    println!("✅ Transformation completed on time");
} else {
    println!("⚠️  Exceeded target by {:.2}s",
        result.total_duration.as_secs_f64() -
        result.target_duration.as_secs_f64());
}
```

### Batch Size Optimization

The system automatically calculates optimal batch sizes based on:
- Total number of RDF operations
- Target batch size from configuration
- Min/max batch size constraints
- Heijunka leveling principles (minimize variance)

```rust
let batch_sizes = leveler.calculate_optimal_batch_sizes(100);
// Result: [10, 10, 10, 10, 10, 10, 10, 10, 10, 10]
// Perfect leveling with zero variance
```

## Variability Reduction Strategies

### 1. Pre-warming Caches

```rust
let config = LeveledGeneratorConfig {
    enable_cache_prewarming: true,
    ..Default::default()
};
```

Pre-warming eliminates cold-start latency by loading frequently-used templates into cache before batch processing.

### 2. Standardized Work Sequences

Using `StandardWorkSequence` ensures consistent execution patterns for RDF transformations, reducing latency variability.

### 3. Takt Time Management

Fixed takt times create predictable rhythm:
- **Fast CI/CD**: 5s takt time for quick iteration
- **Standard**: 10s takt time for balanced processing
- **Heavy Ontology**: 30s takt time for complex RDF

### 4. Batch Size Leveling

Heijunka leveling distributes items evenly across batches:
- 23 items with target batch size 5 → [5, 5, 5, 4, 4]
- Maximum variance: 1 item
- Ensures smooth, predictable processing

## Monitoring and Metrics

### Workload Imbalance Report

```
📊 Mura Leveling Report
═══════════════════════════════════════════════════════════
📈 Overall Metrics:
  Total Templates:       100
  Total RDF Operations:  45
  Total SPARQL Queries:  78
  Workload Imbalance:    0.245 (CV)
  Status:                ✅ Balanced

🎯 Target Distribution:
  ✅ rust         │ Templates:   35 │ Avg:  245.32ms │ CV: 0.187
  ✅ typescript   │ Templates:   33 │ Avg:  238.14ms │ CV: 0.203
  ✅ javascript   │ Templates:   32 │ Avg:  241.89ms │ CV: 0.195

📦 Batch Processing:
  Total Batches:         10
  Avg Batch Size:        10.0
  Batch Size Variance:   5.2%
  Batch Balance:         ✅ Leveled
```

### Heijunka Schedule Report

```
📅 Heijunka Scheduling Report
═══════════════════════════════════════════════════════════
⚙️  Configuration:
  Takt Time:             10.00s
  Batch Size Range:      5-20
  Batches per Cycle:     5
  Strict Timeboxing:     ✅ Enabled

📊 Execution Metrics:
  Total Batches:         10
  Cycle Duration:        102.34s
  Expected Duration:     100.00s
  Cycle Efficiency:      97.7%
  Schedule Adherence:    0.076 - ✅ Excellent
  On-Time Rate:          90.0% (9/10)
```

## Best Practices

### 1. Choose Appropriate Configuration

- **CI/CD Pipelines**: Use `fast_cicd()` preset
- **Heavy RDF Processing**: Use `heavy_ontology()` preset
- **Custom**: Tune based on workload characteristics

### 2. Monitor Metrics

Track key indicators:
- Workload imbalance (CV < 0.3 target)
- Schedule adherence (< 0.1 excellent)
- Throughput trends over time
- Batch processing consistency

### 3. Optimize Batch Sizes

Balance between:
- **Larger batches**: Better throughput, higher latency variance
- **Smaller batches**: More predictable, lower throughput
- **Sweet