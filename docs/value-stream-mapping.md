# Value Stream Mapping for Semantic Code Generation

## Overview

GGen now includes comprehensive Value Stream Mapping (VSM) capabilities to visualize and optimize the entire semantic code generation pipeline from ontology design through deployment. This feature provides end-to-end visibility, lead time tracking, bottleneck detection, and efficiency analysis with moonshot 2028 targets.

## Features

### 1. **End-to-End Pipeline Visualization**

Track the complete flow through nine distinct stages:

1. **Ontology Design** - Semantic modeling and RDF schema definition
2. **Template Creation** - Template authoring with RDF integration
3. **Data Binding** - Linking RDF data to template variables via SPARQL
4. **Template Processing** - Tera template rendering with frontmatter
5. **Graph Querying** - SPARQL query execution against RDF store
6. **Code Generation** - Final code artifact generation
7. **Validation** - Quality checks and governance approvals
8. **Testing** - Automated test execution
9. **Deployment** - Code deployment to target environment

### 2. **Comprehensive Metrics**

#### Timing Metrics
- **Lead Time**: Total elapsed time from stage entry to exit
- **Process Time**: Active processing time (value-added work)
- **Wait Time**: Idle time waiting for resources or approvals
- **Cycle Time**: Average time between stage completions
- **Touch Time**: Actual hands-on work time

#### Efficiency Ratios
- **Process Efficiency**: Ratio of process time to lead time (0.0 to 1.0)
- **Value-Added Ratio**: Proportion of time adding customer value
- **Utilization**: Actual throughput divided by capacity
- **First-Pass Yield**: Proportion passing without rework

#### Quality Metrics
- **Defect Rate**: Defects per thousand opportunities
- **Rework Rate**: Proportion requiring rework
- **Compliance Rate**: Meeting compliance rules

### 3. **Bottleneck Detection**

Automatically identifies and analyzes five types of bottlenecks:

- **Capacity Constraints**: Limited processing capacity
- **Quality Issues**: Defects causing rework
- **Approval Delays**: Slow governance processes
- **Dependency Waits**: Waiting on external dependencies
- **Process Inefficiency**: Low value-added ratios

Each bottleneck includes:
- Severity level (Critical, High, Medium, Low)
- Impact on lead time (milliseconds)
- Delay percentage of total value stream
- Improvement potential
- Actionable recommendations

### 4. **Swim Lane Analysis**

Tracks eight stakeholder roles with touchpoint analysis:

- **Ontology Architect**: Semantic model design
- **Template Author**: Template creation and maintenance
- **Data Engineer**: RDF data and SPARQL management
- **Developer**: Code consumption and feedback
- **Quality Assurance**: Validation and quality gates
- **DevOps Engineer**: Deployment and operations
- **AI System**: Automated generation and governance
- **Governance Board**: Approval authority

Metrics tracked per swim lane:
- Total time spent
- Stage count
- Touchpoint count (handoffs, reviews, approvals)
- Utilization ratio
- Overload detection

### 5. **Future State Mapping**

#### Moonshot 2028 Targets

Three transformational goals with strategic initiatives:

**1. 10x Lead Time Reduction**
- Current baseline: 1 hour (3,600,000 ms)
- Target: 6 minutes (360,000 ms)
- Improvement factor: 10x
- Initiatives:
  - Parallel stage execution
  - AI-powered automated approvals
  - Real-time SPARQL optimization

**2. 100% Automation Rate**
- Current baseline: 30% manual touchpoints
- Target: 0% manual touchpoints
- Initiatives:
  - Autonomous governance agents
  - Self-healing pipeline recovery
  - Predictive quality gates

**3. 99.9% Quality Target**
- Current baseline: 85% first-pass yield
- Target: 99.9% first-pass yield
- Initiatives:
  - AI-powered code validation
  - Continuous learning from defects
  - Proactive anomaly detection

## CLI Commands

### Analyze Complete Value Stream

```bash
ggen vsm analyze --stream-name "my-pipeline"
```

Output:
```json
{
  "stream_name": "my-pipeline",
  "total_lead_time_ms": 75000.0,
  "process_efficiency": 0.573,
  "efficiency_grade": "D",
  "stage_count": 8,
  "bottleneck_count": 3,
  "critical_bottleneck_count": 2,
  "improvement_potential_ms": 24000.0,
  "improvement_potential_pct": 32.0,
  "recommendations_count": 7
}
```

### Detect Bottlenecks

```bash
ggen vsm bottlenecks --stream-name "my-pipeline" --top-n 5
```

Output shows the top bottlenecks ranked by impact with improvement recommendations.

### Calculate Efficiency Ratios

```bash
ggen vsm efficiency --stream-name "my-pipeline"
```

Output:
```json
{
  "overall_efficiency": 0.573,
  "efficiency_grade": "D",
  "value_added_ratio": 0.573,
  "best_stage": "Testing",
  "worst_stage": "Validation",
  "stage_efficiencies": [
    {
      "stage": "Ontology Design",
      "efficiency": 0.625,
      "grade": "D"
    },
    ...
  ]
}
```

### Analyze Swim Lanes

```bash
ggen vsm swimlanes --stream-name "my-pipeline"
```

Shows stakeholder utilization, handoff counts, and overload detection.

### View Moonshot Targets

```bash
ggen vsm moonshot
```

Displays 2028 moonshot targets with current baselines and improvement factors.

### Generate Visualization Data

```bash
ggen vsm visualize --stream-name "my-pipeline" --format json
```

Generates complete visualization data including:
- Lead time charts (stacked: process + wait time)
- Efficiency radar charts
- Bottleneck impact charts
- Swim lane diagrams
- Process flow diagrams

## Programmatic API

### Creating a Value Stream

```rust
use ggen_domain::vsm::{ValueStream, Stage, StageType};
use std::time::Duration;

let mut stream = ValueStream::new("semantic-generation");
stream.start();

// Add stages
let mut stage = Stage::new(StageType::OntologyDesign);
stage.start();
// ... do work ...
stage.record_process_time(Duration::from_millis(5000));
stage.end();

stream.add_stage(stage);
stream.end();
```

### Analyzing the Stream

```rust
use ggen_domain::vsm::AnalysisReport;

let report = AnalysisReport::generate(&stream, None);

println!("Total lead time: {:.2}ms", report.overall_metrics.total_lead_time_ms);
println!("Efficiency: {:.1}%", report.efficiency.overall_efficiency * 100.0);
println!("Bottlenecks: {}", report.bottleneck_summary.total_bottlenecks);
```

### Detecting Bottlenecks

```rust
stream.analyze(); // Automatically detects bottlenecks

for bottleneck in stream.bottlenecks.top_bottlenecks(5) {
    println!("{}: {} impact", bottleneck.stage, bottleneck.severity);
    for recommendation in &bottleneck.recommendations {
        println!("  → {}", recommendation);
    }
}
```

### Moonshot Targets

```rust
use ggen_domain::vsm::FutureState;

let moonshot = FutureState::moonshot_2028();

for target in &moonshot.moonshot_targets {
    println!(
        "{}: {:.2}{} → {:.2}{} ({:.1}x improvement)",
        target.name,
        target.current_baseline,
        target.unit,
        target.target_value,
        target.unit,
        target.improvement_factor
    );
}
```

## RDF Ontology

VSM data is stored using a comprehensive RDF ontology at:
`http://ggen.dev/ontology/vsm#`

Key classes:
- `vsm:ValueStream` - Complete end-to-end flow
- `vsm:Stage` - Discrete pipeline step
- `vsm:Transition` - Flow between stages
- `vsm:Bottleneck` - Performance constraint
- `vsm:SwimLane` - Stakeholder role
- `vsm:Touchpoint` - Stakeholder interaction
- `vsm:FutureState` - Target improvement state
- `vsm:MoonshotTarget` - Transformational 2028 goal

Example SPARQL query:

```sparql
PREFIX vsm: <http://ggen.dev/ontology/vsm#>

SELECT ?stage ?leadTime ?efficiency
WHERE {
  ?stage a vsm:Stage ;
         vsm:leadTime ?leadTime ;
         vsm:processEfficiency ?efficiency .
  FILTER (?efficiency < 0.7)
}
ORDER BY ?efficiency
```

## Visualization Examples

### Lead Time Waterfall Chart

Shows breakdown of time spent in each stage with color coding:
- Green: High efficiency (>70%)
- Orange: Medium efficiency (50-70%)
- Red: Low efficiency (<50%)

### Efficiency Radar Chart

Multi-axis visualization showing process efficiency across all stages, making it easy to spot weak points.

### Bottleneck Impact Chart

Horizontal bar chart showing bottlenecks ranked by delay percentage, with color-coded severity levels.

### Swim Lane Diagram

Visualizes stakeholder interactions, handoffs, and workload distribution across the value stream.

### Process Flow Diagram

Node-and-edge graph showing stage progression with bottlenecks highlighted in red.

## Best Practices

### 1. Regular Analysis

Run VSM analysis after every pipeline execution to track trends:

```bash
ggen vsm analyze --stream-name "production" >> vsm-history.jsonl
```

### 2. Focus on Critical Path

Use the critical path analysis to focus optimization efforts:

```rust
let critical_stages = stream.critical_path();
for stage in critical_stages {
    println!("Critical: {} ({:.2}ms)", stage.stage_type, stage.lead_time().unwrap().as_millis());
}
```

### 3. Prioritize Bottlenecks

Address bottlenecks by priority score:

```rust
for bottleneck in stream.bottlenecks.by_priority().iter().take(3) {
    println!("Priority {:.2}: {}", bottleneck.priority_score(), bottleneck.summary());
}
```

### 4. Monitor Swim Lane Balance

Watch for overloaded stakeholders:

```rust
for role in stream.swim_lanes.overloaded_stakeholders() {
    eprintln!("WARNING: {} is overloaded (>85% utilization)", role);
}
```

### 5. Track Moonshot Progress

Regularly compare current performance against moonshot targets:

```rust
let current_metrics = calculate_current_metrics(&stream);
let progress = future_state.overall_progress(&current_metrics);
println!("Moonshot progress: {:.1}%", progress * 100.0);
```

## Integration Points

### Pipeline Integration

Automatically track VSM metrics in your template processing pipeline:

```rust
use ggen_core::pipeline::Pipeline;
use ggen_domain::vsm::{ValueStream, Stage, StageType};

let mut vsm = ValueStream::new("template-pipeline");

// Before template processing
let mut stage = Stage::new(StageType::TemplateProcessing);
stage.start();

// Run pipeline
let result = pipeline.process(template)?;

// After template processing
stage.end();
vsm.add_stage(stage);
```

### CI/CD Integration

Export VSM metrics for CI/CD monitoring:

```bash
#!/bin/bash
# In your CI/CD pipeline
ggen vsm analyze --stream-name "$CI_PIPELINE_ID" > vsm-report.json

# Fail build if efficiency drops below threshold
efficiency=$(jq '.process_efficiency' vsm-report.json)
if (( $(echo "$efficiency < 0.6" | bc -l) )); then
  echo "ERROR: Process efficiency below 60%: $efficiency"
  exit 1
fi
```

### Dashboard Integration

Use the visualization API to feed real-time dashboards:

```rust
let viz = VisualizationData::from_value_stream(&stream);
let json = serde_json::to_string_pretty(&viz)?;
// Send to dashboard API
```

## Performance Considerations

- VSM tracking adds minimal overhead (<5% per stage)
- Metrics are calculated incrementally
- Bottleneck detection runs once at stream completion
- RDF storage is optional (in-memory by default)
- Visualization data is generated on demand

## Future Enhancements

- [ ] Real-time streaming metrics
- [ ] Machine learning for bottleneck prediction
- [ ] Automated optimization recommendations
- [ ] Historical trend analysis
- [ ] Multi-stream comparison
- [ ] Integration with OpenTelemetry
- [ ] Custom dashboard templates
- [ ] Slack/Teams notifications for bottlenecks

## References

- [Value Stream Mapping Guide](https://www.lean.org/lexicon-terms/value-stream-mapping/)
- [Process Efficiency Metrics](https://en.wikipedia.org/wiki/Process_performance_index)
- [Theory of Constraints](https://en.wikipedia.org/wiki/Theory_of_constraints)
- [Lean Software Development](https://en.wikipedia.org/wiki/Lean_software_development)

## License

MIT License - See LICENSE file for details

## Contributing

Contributions welcome! Please see CONTRIBUTING.md for guidelines.
