# Kanban Pull System for Ontology Evolution

The ggen Kanban Pull System provides a demand-driven workflow for managing ontology evolution, template modifications, and code generation. It prevents overproduction by ensuring work is only started when there is explicit demand, while enforcing work-in-progress (WIP) limits to maintain flow efficiency.

## Overview

The Kanban system implements a pull-based workflow with six stages:

1. **Backlog** - Pending ontology changes awaiting prioritization
2. **Analysis** - RDF schema impact analysis
3. **Transformation** - Semantic transformation execution
4. **Validation** - Generated code and schema validation
5. **Generation** - Final code generation
6. **Done** - Completed and deployed changes

## Key Features

### 1. Work-in-Progress (WIP) Limits

WIP limits prevent overproduction and maintain flow efficiency:

- **Backlog**: 100 cards (default)
- **Analysis**: 5 concurrent analyses
- **Transformation**: 3 concurrent transformations
- **Validation**: 5 concurrent validations
- **Generation**: 2 concurrent generations
- **Done**: Unlimited

```bash
# Initialize board with custom WIP limits
ggen kanban init --max-concurrent 3
```

### 2. Flow Efficiency Metrics

Track semantic transformation efficiency:

- **Lead Time**: End-to-end time from backlog to done
- **Cycle Time**: Time spent in each stage
- **Throughput**: Items completed per hour
- **Flow Efficiency**: Ratio of value-added time to total time
- **WIP**: Current work in progress across all stages

```bash
# View flow metrics
ggen kanban metrics
```

### 3. Cumulative Flow Diagrams

Visual workflow tracking showing:

- WIP trends over time
- Bottleneck identification
- Stage utilization patterns
- Flow debt analysis

```bash
# Generate cumulative flow diagram
ggen kanban diagram --csv flow-data.csv --json flow-data.json
```

### 4. RDF Schema Change Tracking

Workflow cards track:

- Class additions/removals/modifications
- Property changes
- Relationship updates
- Cardinality constraints
- SPARQL query templates
- Namespace modifications

```bash
# Create card for RDF change
ggen kanban create "Add User class" "Add User class to ontology" --priority high

# Show card details
ggen kanban show <card-id>
```

### 5. Demand-Driven Generation

Pull system prevents overproduction:

- Explicit demand signals
- Schema change detection
- Dependency pulls
- Template usage tracking
- Scheduled updates

```bash
# Signal demand for generation
ggen kanban signal "Add authentication feature" --priority critical --signal-type request

# Pull work into next stage
ggen kanban pull --stage analysis
```

## CLI Commands

### Initialize Kanban Board

```bash
ggen kanban init [OPTIONS]
```

**Options:**
- `--max-concurrent <N>` - Maximum concurrent template modifications (default: 3)
- `--board-path <PATH>` - Path to save board state (default: .ggen/kanban.json)

**Example:**
```bash
ggen kanban init --max-concurrent 5
```

### View Board Status

```bash
ggen kanban status [OPTIONS]
```

**Options:**
- `--board-path <PATH>` - Path to board state

**Output:**
```json
{
  "total_wip": 8,
  "backlog": 5,
  "analysis": 2,
  "transformation": 1,
  "validation": 0,
  "generation": 0,
  "done": 15,
  "bottlenecks": [
    {
      "stage": "transformation",
      "utilization": 1.0
    }
  ]
}
```

### List Workflow Cards

```bash
ggen kanban list [OPTIONS]
```

**Options:**
- `--stage <STAGE>` - Filter by stage (backlog, analysis, transformation, validation, generation, done)
- `--status <STATUS>` - Filter by status (active, blocked, completed, abandoned)
- `--cards-path <PATH>` - Path to cards file

**Example:**
```bash
ggen kanban list --stage transformation --status active
```

### Show Card Details

```bash
ggen kanban show <CARD_ID> [OPTIONS]
```

**Example:**
```bash
ggen kanban show abc-123-def
```

### Create Workflow Card

```bash
ggen kanban create <TITLE> <DESCRIPTION> [OPTIONS]
```

**Options:**
- `--priority <PRIORITY>` - Priority level: low, medium, high, critical (default: medium)
- `--cards-path <PATH>` - Path to cards file
- `--board-path <PATH>` - Path to board state

**Example:**
```bash
ggen kanban create \
  "Add Product class" \
  "Add Product class with price and inventory properties" \
  --priority high
```

### Pull Card from Previous Stage

```bash
ggen kanban pull --stage <STAGE> [OPTIONS]
```

**Options:**
- `--stage <STAGE>` - Stage to pull into
- `--board-path <PATH>` - Path to board state

**Example:**
```bash
# Pull next card from backlog into analysis
ggen kanban pull --stage analysis
```

### Calculate Flow Metrics

```bash
ggen kanban metrics [OPTIONS]
```

**Options:**
- `--board-path <PATH>` - Path to board state
- `--cards-path <PATH>` - Path to cards file

**Output:**
```json
{
  "lead_time_avg": 3600.0,
  "throughput": 2.5,
  "wip": 8,
  "flow_efficiency": 0.65,
  "completed_count": 25,
  "blocked_count": 1,
  "bottlenecks": [
    {
      "stage": "transformation",
      "severity": "High",
      "reason": "High variance in cycle time (P95: 500.0s, Avg: 100.0s)",
      "recommendation": "Investigate outliers and standardize process"
    }
  ]
}
```

### Generate Cumulative Flow Diagram

```bash
ggen kanban diagram [OPTIONS]
```

**Options:**
- `--csv <PATH>` - Export data as CSV
- `--json <PATH>` - Export data as JSON
- `--diagram-path <PATH>` - Path to flow diagram data

**Example:**
```bash
ggen kanban diagram --csv cfd.csv --json cfd.json
```

### Signal Demand for Generation

```bash
ggen kanban signal <DESCRIPTION> [OPTIONS]
```

**Options:**
- `--priority <PRIORITY>` - Priority: low, medium, high, critical (default: medium)
- `--signal-type <TYPE>` - Signal type: request, schema_change, dependency, template_usage (default: request)
- `--demand-path <PATH>` - Path to demand system state

**Example:**
```bash
ggen kanban signal \
  "Implement OAuth2 authentication" \
  --priority critical \
  --signal-type request
```

## Workflow Example

### 1. Initialize the Kanban Board

```bash
ggen kanban init --max-concurrent 3
```

### 2. Create a Workflow Card

```bash
ggen kanban create \
  "Add User authentication" \
  "Implement User class with authentication properties" \
  --priority high
```

### 3. Pull Card into Analysis Stage

```bash
ggen kanban pull --stage analysis
```

### 4. Move Through Stages

As work progresses, cards are pulled through each stage:

```bash
ggen kanban pull --stage transformation
ggen kanban pull --stage validation
ggen kanban pull --stage generation
```

### 5. Monitor Flow Metrics

```bash
ggen kanban metrics
```

### 6. Identify Bottlenecks

```bash
ggen kanban status
```

### 7. Generate Flow Diagrams

```bash
ggen kanban diagram --csv weekly-flow.csv
```

## Architecture

### Module Organization

```
ggen-domain/src/kanban/
├── mod.rs              # Main module and types
├── workflow.rs         # Workflow state machine
├── card.rs             # Workflow cards and RDF tracking
├── limits.rs           # WIP limits enforcement
├── metrics.rs          # Flow efficiency metrics
├── flow_diagram.rs     # Cumulative flow diagrams
└── demand.rs           # Demand-driven pull system
```

### Core Types

#### WorkflowStage

```rust
pub enum WorkflowStage {
    Backlog,
    Analysis,
    Transformation,
    Validation,
    Generation,
    Done,
}
```

#### WorkflowCard

```rust
pub struct WorkflowCard {
    pub id: String,
    pub title: String,
    pub description: String,
    pub status: CardStatus,
    pub priority: Priority,
    pub stage: WorkflowStage,
    pub rdf_changes: Vec<RdfChangeType>,
    pub affected_templates: Vec<String>,
    pub affected_files: Vec<String>,
    // ... timestamps and metadata
}
```

#### RdfChangeType

```rust
pub enum RdfChangeType {
    ClassAdded { class_uri: String },
    ClassRemoved { class_uri: String },
    ClassModified { class_uri: String },
    PropertyAdded { property_uri: String },
    PropertyRemoved { property_uri: String },
    PropertyModified { property_uri: String },
    RelationshipAdded { from: String, to: String, predicate: String },
    RelationshipRemoved { from: String, to: String, predicate: String },
    CardinalityChanged { class_uri: String, property: String },
    QueryTemplateAdded { name: String },
    NamespaceModified { old_ns: String, new_ns: String },
}
```

## Best Practices

### 1. Set Appropriate WIP Limits

Start conservative and adjust based on metrics:

- **Analysis**: 3-5 cards (requires deep thinking)
- **Transformation**: 2-3 cards (complex semantic work)
- **Validation**: 3-5 cards (can parallelize testing)
- **Generation**: 1-2 cards (resource-intensive)

### 2. Monitor Flow Efficiency

Target flow efficiency > 30%:

```bash
ggen kanban metrics | jq '.flow_efficiency'
```

If below 30%, reduce WIP limits or eliminate wait times between stages.

### 3. Use Demand Signals

Don't start work speculatively:

```bash
# Signal explicit demand before creating cards
ggen kanban signal "Feature X requested by team" --priority high

# Then pull work as capacity allows
ggen kanban pull --stage analysis
```

### 4. Track Bottlenecks

Review bottlenecks weekly:

```bash
ggen kanban status | jq '.bottlenecks'
```

Address stages with utilization > 80%.

### 5. Measure Cycle Time

Optimize stages with high cycle time variance:

```bash
ggen kanban metrics | jq '.bottlenecks[] | select(.severity == "High")'
```

### 6. Visualize Flow

Generate weekly cumulative flow diagrams:

```bash
ggen kanban diagram --csv "flow-$(date +%Y-%m-%d).csv"
```

## Integration with ggen

The Kanban system integrates with ggen's existing features:

### Template Generation

```bash
# Create card for template modification
ggen kanban create "Update user template" "Add email validation" --priority medium

# Pull into transformation stage
ggen kanban pull --stage transformation

# Generate template with ggen
ggen template generate user.hbs --output src/models/user.rs

# Move to validation
ggen kanban pull --stage validation

# Run tests
ggen test

# Complete generation
ggen kanban pull --stage generation
ggen kanban pull --stage done
```

### RDF Schema Evolution

```bash
# Signal schema change
ggen kanban signal "Add Product class to ontology" \
  --signal-type schema_change \
  --priority high

# Pull into analysis
ggen kanban pull --stage analysis

# Analyze RDF impact
ggen graph query --sparql "SELECT ?s ?p ?o WHERE { ?s a :Product }"

# Continue through workflow...
```

### Marketplace Integration

```bash
# Create card for marketplace template
ggen kanban create "Install auth template from marketplace" \
  "Install and integrate authentication template" \
  --priority high

# Install template
ggen marketplace install auth-template

# Track in Kanban
ggen kanban pull --stage transformation
```

## Performance Monitoring

### Little's Law Validation

The system uses Little's Law to validate workflow health:

```
WIP = Throughput × Lead Time
```

If actual WIP significantly differs from expected WIP based on throughput and lead time, investigate:

- Bottlenecks
- Excessive multitasking
- Inefficient processes

### Key Performance Indicators (KPIs)

1. **Lead Time** - Target: < 24 hours for high-priority items
2. **Flow Efficiency** - Target: > 30%
3. **Throughput** - Track trend (should increase or stabilize)
4. **WIP** - Keep minimal while maintaining throughput
5. **Cycle Time Variance** - Minimize for predictability

## Troubleshooting

### High WIP, Low Throughput

**Problem**: Many cards in progress but few completing.

**Solution**:
```bash
# Reduce WIP limits
# Edit .ggen/kanban.json and lower stage limits

# Stop pulling new work
# Focus on completing existing cards
```

### Bottleneck in Transformation Stage

**Problem**: Transformation stage always at capacity.

**Solution**:
1. Increase transformation WIP limit (cautiously)
2. Reduce upstream WIP limits to prevent overfeeding
3. Investigate cycle time outliers
4. Consider breaking down complex transformations

### Low Flow Efficiency

**Problem**: Flow efficiency < 30%.

**Solution**:
1. Reduce wait times between stages
2. Automate validation and generation
3. Implement fast feedback loops
4. Minimize handoffs

### Overproduction Detected

**Problem**: Work being done without demand.

**Solution**:
```bash
# Check overproduction warning
ggen kanban metrics | jq '.overproduction_warning'

# Stop pulling new work
# Wait for demand signals before starting new cards
```

## Advanced Features

### Custom WIP Policies

The system supports three WIP enforcement policies:

1. **Strict** - Hard limit, reject new work
2. **Warn** - Warn when limit exceeded but allow
3. **Soft** - Allow controlled overflow (e.g., 20%)

Configured in `.ggen/kanban.json`:

```json
{
  "wip_policy": {
    "type": "soft",
    "overflow_percent": 20
  }
}
```

### Dynamic Limit Adjustment

Enable automatic WIP limit adjustment based on throughput:

```json
{
  "dynamic_adjustment": true
}
```

The system will:
- Reduce limits when throughput drops
- Cautiously increase limits when throughput is high
- Maintain history for trend analysis

### Automated Demand Signals

Integrate with CI/CD to auto-signal demand:

```bash
# In CI pipeline
if [ "$BRANCH" = "main" ]; then
  ggen kanban signal "Deploy to production" \
    --priority critical \
    --signal-type dependency
fi
```

## Conclusion

The ggen Kanban Pull System provides a structured, metrics-driven approach to managing ontology evolution and code generation. By enforcing WIP limits, tracking flow efficiency, and using demand signals, it prevents overproduction while maintaining high throughput and predictable delivery.

For more information, see:
- [ggen Documentation](https://github.com/seanchatmangpt/ggen)
- [Kanban Principles](https://www.kanbanize.com/kanban-resources/getting-started/what-is-kanban)
- [Flow Metrics](https://www.leankanban.com/project/flow-metrics/)
