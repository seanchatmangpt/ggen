<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TPS as the Manufacturing OS: From Craft Shop to Robotic Factory](#tps-as-the-manufacturing-os-from-craft-shop-to-robotic-factory)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
    - [What is TPS?](#what-is-tps)
    - [Why "Manufacturing OS"?](#why-manufacturing-os)
    - [The Paradigm Shift](#the-paradigm-shift)
  - [The Two Pillars: JIT and Jidoka](#the-two-pillars-jit-and-jidoka)
    - [Pillar 1: Just-In-Time (JIT)](#pillar-1-just-in-time-jit)
    - [Pillar 2: Jidoka (Autonomation)](#pillar-2-jidoka-autonomation)
  - [The Full TPS Method Stack](#the-full-tps-method-stack)
    - [1. Heijunka (Load Leveling)](#1-heijunka-load-leveling)
    - [2. Kanban (Visual Workflow)](#2-kanban-visual-workflow)
    - [3. Standard Work (Standardized Procedures)](#3-standard-work-standardized-procedures)
    - [4. Poka-Yoke (Error-Proofing)](#4-poka-yoke-error-proofing)
    - [5. Andon (Visual Management)](#5-andon-visual-management)
    - [6. Genchi Genbutsu (Go and See)](#6-genchi-genbutsu-go-and-see)
  - [Three Manufacturing Models](#three-manufacturing-models)
    - [Model 1: SCM (Source Code Mode) = Craft Shop](#model-1-scm-source-code-mode--craft-shop)
    - [Model 2: CLM (Continuous Learning Mode) = Assembly Line](#model-2-clm-continuous-learning-mode--assembly-line)
    - [Model 3: CCM (Continuous Commitment Mode) = Robotic Factory](#model-3-ccm-continuous-commitment-mode--robotic-factory)
    - [Comparison Table](#comparison-table)
  - [Inadmissible-Before Gates](#inadmissible-before-gates)
    - [Manufacturing Example](#manufacturing-example)
    - [Software Examples](#software-examples)
      - [Gate 1: Type Checking (Compile-Time)](#gate-1-type-checking-compile-time)
      - [Gate 2: SHACL Validation (Generation-Time)](#gate-2-shacl-validation-generation-time)
    - [CLM/CCM Enforcement Mechanisms](#clmccm-enforcement-mechanisms)
      - [CLM (Assembly Line): Manual Gates](#clm-assembly-line-manual-gates)
      - [CCM (Robotic Factory): Automatic Gates](#ccm-robotic-factory-automatic-gates)
  - [Discretionary Fixes: Bypassing the Line](#discretionary-fixes-bypassing-the-line)
    - [Craft Shop (SCM): Discretionary Fixes Everywhere](#craft-shop-scm-discretionary-fixes-everywhere)
    - [Assembly Line (CLM): Some Discretion Allowed](#assembly-line-clm-some-discretion-allowed)
    - [Robotic Factory (CCM): No Discretion (Fix at Source)](#robotic-factory-ccm-no-discretion-fix-at-source)
    - [Why CCM Prevents Bypasses](#why-ccm-prevents-bypasses)
  - [Software Development Mappings](#software-development-mappings)
    - [Complete Mapping Table](#complete-mapping-table)
  - [Visual Representations](#visual-representations)
    - [Visual 1: TPS as Manufacturing OS](#visual-1-tps-as-manufacturing-os)
    - [Visual 2: Three Manufacturing Models](#visual-2-three-manufacturing-models)
    - [Visual 3: Inadmissible-Before Gates](#visual-3-inadmissible-before-gates)
    - [Visual 4: Discretionary Fixes (SCM vs CCM)](#visual-4-discretionary-fixes-scm-vs-ccm)
    - [Visual 5: Andon System](#visual-5-andon-system)
  - [Case Study: ggen Implementation](#case-study-ggen-implementation)
    - [Architecture Overview](#architecture-overview)
    - [TPS Principles in ggen](#tps-principles-in-ggen)
      - [1. JIT: Generate Only What's Needed](#1-jit-generate-only-whats-needed)
      - [2. Jidoka: Built-In Quality](#2-jidoka-built-in-quality)
      - [3. Andon: CI Quality Monitoring](#3-andon-ci-quality-monitoring)
      - [4. Gemba Walk: Weekly Test Inspection](#4-gemba-walk-weekly-test-inspection)
      - [5. Kaizen: Weekly Improvement Metrics](#5-kaizen-weekly-improvement-metrics)
  - [Conclusion](#conclusion)
    - [Key Takeaways](#key-takeaways)
    - [The Paradigm Shift](#the-paradigm-shift-1)
    - [Next Steps](#next-steps)
    - [Further Reading](#further-reading)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TPS as the Manufacturing OS: From Craft Shop to Robotic Factory

**Understanding Toyota Production System as the operating system for code generation**

---

## Table of Contents

- [Introduction](#introduction)
- [The Two Pillars: JIT and Jidoka](#the-two-pillars-jit-and-jidoka)
- [The Full TPS Method Stack](#the-full-tps-method-stack)
- [Three Manufacturing Models](#three-manufacturing-models)
- [Inadmissible-Before Gates](#inadmissible-before-gates)
- [Discretionary Fixes: Bypassing the Line](#discretionary-fixes-bypassing-the-line)
- [Software Development Mappings](#software-development-mappings)
- [Visual Representations](#visual-representations)
- [Case Study: ggen Implementation](#case-study-ggen-implementation)
- [Conclusion](#conclusion)

---

## Introduction

**TL;DR**: Toyota Production System (TPS) is not just a methodology—it's an operating system for manufacturing. When applied to software development, it transforms code generation from artisanal craft work into a predictable, quality-assured production system.

### What is TPS?

The Toyota Production System is a manufacturing philosophy developed at Toyota between 1948-1975 by Taiichi Ohno and Eiji Toyoda. It revolutionized manufacturing by:

- Reducing waste (muda) systematically
- Building quality into the process (jidoka)
- Producing only what's needed, when needed (just-in-time)
- Empowering workers to stop production when defects appear (andon)

### Why "Manufacturing OS"?

Just as an operating system provides:
- **Process Management**: Schedule tasks, manage resources
- **Quality Control**: Prevent invalid states, enforce constraints
- **Error Handling**: Detect and respond to failures
- **Resource Optimization**: Minimize waste, maximize throughput

TPS provides the same capabilities for manufacturing—and by extension, for code generation.

### The Paradigm Shift

Traditional software development operates like a **craft shop**: individual artisans (developers) hand-craft unique solutions using discretionary judgment. Quality varies by craftsperson.

TPS-driven development operates like a **robotic factory**: standardized processes generate consistent output with built-in quality gates. Defects are impossible by design.

---

## The Two Pillars: JIT and Jidoka

TPS rests on two foundational pillars that work in concert:

### Pillar 1: Just-In-Time (JIT)

**Definition**: Produce the right item, in the right quantity, at the right time.

**Core Principles**:
- **Pull System**: Downstream demand triggers upstream production (not push)
- **Continuous Flow**: Eliminate batching and queues
- **Takt Time**: Match production rate to consumption rate

**In Software Development**:

```rust
// ❌ PUSH: Generate everything upfront (waste)
fn generate_all_models() {
    generate_rust_models();
    generate_typescript_models();
    generate_python_models();
    generate_java_models();  // Never used!
}

// ✅ JIT: Generate only what's requested
fn generate_on_demand(language: Language) {
    match language {
        Language::Rust => generate_rust_models(),
        Language::TypeScript => generate_typescript_models(),
        _ => {} // Don't generate unused code
    }
}
```

**Benefits**:
- Zero inventory waste (no unused generated code)
- Fast feedback loops (generate → test → iterate)
- Resource efficiency (CPU/memory used only when needed)

---

### Pillar 2: Jidoka (Autonomation)

**Definition**: Automation with human intelligence—machines detect abnormalities and stop automatically.

**Core Principles**:
- **Built-in Quality**: Defects cannot proceed to next stage
- **Stop and Alert**: Immediate notification when problems occur
- **Root Cause Analysis**: Fix problems at the source, not downstream

**In Software Development**:

```rust
// ❌ Manual Quality Check (error-prone)
fn generate_code(template: &str, data: &Data) -> String {
    let code = render_template(template, data);
    // Hope the output is valid!
    code
}

// ✅ Jidoka: Built-in Quality Check
fn generate_code(template: &str, data: &Data) -> Result<ValidatedCode, GenerationError> {
    let code = render_template(template, data)?;

    // Type check (automated quality gate)
    let ast = parse_ast(&code)?;
    validate_types(&ast)?;

    // Compile check (automated quality gate)
    compile_check(&code)?;

    Ok(ValidatedCode(code))
}
```

**Benefits**:
- Defects caught at generation time, not runtime
- No defective code propagates to users
- Immediate feedback for root cause fixing

---

## The Full TPS Method Stack

TPS is not just JIT + Jidoka. It's a complete stack of interlocking practices:

### 1. Heijunka (Load Leveling)

**Manufacturing**: Distribute production evenly over time to avoid peaks/valleys.

**Software Mapping**:

```rust
// ❌ Uneven Load: Generate all files at once (memory spike)
fn generate_project() {
    let files: Vec<String> = (0..1000)
        .map(|i| generate_large_file(i))  // 1000 files × 10MB = 10GB RAM!
        .collect();
    write_all(files);
}

// ✅ Heijunka: Level the load (streaming)
fn generate_project() {
    for i in 0..1000 {
        let file = generate_large_file(i);  // 10MB at a time
        write_file(file);                    // Constant memory usage
    }
}
```

**Benefits**: Predictable resource usage, no memory spikes, scalable to large projects.

---

### 2. Kanban (Visual Workflow)

**Manufacturing**: Cards signal when to produce next item (pull signal).

**Software Mapping**:

```yaml
# .ggen/workflow.yml
stages:
  - parse_ontology:     # Stage 1: Parse RDF
      triggers: [ontology_changed]
      outputs: [parsed_graph]

  - validate_constraints:  # Stage 2: SHACL validation
      requires: [parsed_graph]
      triggers: [graph_ready]
      outputs: [validated_graph]

  - generate_code:      # Stage 3: Code generation
      requires: [validated_graph]
      triggers: [validation_passed]
      outputs: [generated_code]

  - compile_check:      # Stage 4: Compilation
      requires: [generated_code]
      triggers: [code_generated]
```

**Kanban Board Visualization**:

```
┌─────────────┬──────────────┬──────────────┬─────────────┐
│  To Parse   │  To Validate │  To Generate │   To Test   │
├─────────────┼──────────────┼──────────────┼─────────────┤
│  user.ttl   │              │              │             │
│             │  order.ttl ● │              │             │
│             │              │ product.ttl ●│             │
│             │              │              │ invoice.ttl │
└─────────────┴──────────────┴──────────────┴─────────────┘
                  ● = Work in Progress
```

**Benefits**: Visual progress tracking, clear dependencies, pull-based workflow.

---

### 3. Standard Work (Standardized Procedures)

**Manufacturing**: Document best-known method for each operation.

**Software Mapping**:

```rust
// Standard Work: Generation Template (always follow this pattern)

/// Standard code generation procedure (DO NOT DEVIATE)
fn standard_generation_workflow(ontology_path: &Path) -> Result<GeneratedArtifacts, Error> {
    // Step 1: Parse (REQUIRED - cannot skip)
    let graph = parse_ontology(ontology_path)?;

    // Step 2: Validate (REQUIRED - cannot skip)
    validate_shacl(&graph)?;

    // Step 3: Extract (REQUIRED - cannot skip)
    let classes = extract_classes(&graph)?;
    let properties = extract_properties(&graph)?;

    // Step 4: Transform (REQUIRED - cannot skip)
    let ir = build_intermediate_representation(classes, properties)?;

    // Step 5: Generate (REQUIRED - cannot skip)
    let code = render_templates(&ir)?;

    // Step 6: Validate Output (REQUIRED - cannot skip)
    validate_generated_code(&code)?;

    Ok(GeneratedArtifacts { code, metadata: build_metadata() })
}
```

**Standard Work Instruction Card**:

```
┌─────────────────────────────────────────────┐
│ STANDARD WORK: Code Generation              │
├─────────────────────────────────────────────┤
│ Cycle Time: 2.3s (target)                   │
│ Takt Time: 3.0s (customer demand)           │
│                                              │
│ Operations (in order):                       │
│  1. Parse RDF      [0.5s] ✓ Graph valid     │
│  2. SHACL Check    [0.3s] ✓ Shapes pass     │
│  3. Extract Data   [0.4s] ✓ Classes found   │
│  4. Build IR       [0.6s] ✓ IR well-formed  │
│  5. Render Code    [0.3s] ✓ Syntax valid    │
│  6. Compile Check  [0.2s] ✓ Types correct   │
│                                              │
│ Quality Checks: 6/6 must pass                │
│ Safety: Stop on first failure (Andon)       │
└─────────────────────────────────────────────┘
```

**Benefits**: Consistency across all generation runs, clear quality gates, predictable timing.

---

### 4. Poka-Yoke (Error-Proofing)

**Manufacturing**: Design processes so errors are impossible (e.g., asymmetric connectors).

**Software Mapping**:

```rust
// ❌ Error-Prone: String-based (typos possible)
fn generate(format: &str, output: &str) {
    match format {
        "rust" => generate_rust(output),
        "typescript" => generate_typescript(output),
        _ => panic!("Invalid format!"),  // Runtime error!
    }
}

// ✅ Poka-Yoke: Type-safe (errors impossible)
enum Language {
    Rust,
    TypeScript,
    Python,
}

enum OutputMode {
    File(PathBuf),
    Stdout,
    Memory(Vec<u8>),
}

fn generate(language: Language, output: OutputMode) -> Result<(), Error> {
    let code = match language {
        Language::Rust => generate_rust()?,
        Language::TypeScript => generate_typescript()?,
        Language::Python => generate_python()?,
    };

    match output {
        OutputMode::File(path) => write_file(path, code)?,
        OutputMode::Stdout => println!("{}", code),
        OutputMode::Memory(mut buf) => buf.extend_from_slice(code.as_bytes()),
    }

    Ok(())
}

// ✅ Compiler enforces correctness!
// generate("rusttt", "output.rs");  // ❌ Compile error: expected Language, found &str
```

**Five Poka-Yoke Patterns in Software**:

| Pattern | Manufacturing | Software Example |
|---------|---------------|------------------|
| **Guide Pin** | Asymmetric plug | Enum variants (can't mix types) |
| **Limit Switch** | Machine stops at boundary | Bounded types (`struct Age(u8)`) |
| **Counter** | Count operations | Iterator exhaustion (`iter.nth(10)` returns `None`) |
| **Checklist** | Pre-flight checklist | Builder pattern (compile-time validation) |
| **Sequencing** | Must press A before B | Typestate pattern (state machine types) |

---

### 5. Andon (Visual Management)

**Manufacturing**: Lights/alarms signal production status (Green/Yellow/Red).

**Software Mapping**:

```rust
// Andon System for Code Generation

#[derive(Debug)]
enum AndonStatus {
    Green,   // All systems operational
    Yellow,  // Warning: degraded performance
    Red,     // Critical: stop the line
}

struct GenerationAndon {
    status: AndonStatus,
    failures: Vec<Failure>,
    metrics: Metrics,
}

impl GenerationAndon {
    fn check_status(&mut self) -> AndonStatus {
        let failure_rate = self.metrics.failures as f64 / self.metrics.total as f64;
        let avg_time = self.metrics.total_time / self.metrics.total;

        if failure_rate > 0.05 {
            // > 5% failure rate = RED ANDON
            self.status = AndonStatus::Red;
            self.stop_the_line();
            self.alert_team("Critical: >5% generation failures");
        } else if failure_rate > 0.02 {
            // > 2% failure rate = YELLOW ANDON
            self.status = AndonStatus::Yellow;
            self.alert_team("Warning: Elevated failure rate");
        } else if avg_time > Duration::from_secs(10) {
            // > 10s average = YELLOW ANDON
            self.status = AndonStatus::Yellow;
            self.alert_team("Warning: Slow generation times");
        } else {
            self.status = AndonStatus::Green;
        }

        self.status
    }

    fn stop_the_line(&self) {
        // HALT all generation until root cause fixed
        panic!("🔴 RED ANDON: Production stopped due to quality issues");
    }
}
```

**CI/CD Andon Integration**:

```yaml
# .github/workflows/andon.yml
name: Andon Quality Gate

on: [push, pull_request]

jobs:
  andon_check:
    runs-on: ubuntu-latest
    steps:
      - name: Run Generation Tests
        run: cargo test --all

      - name: Check Andon Status
        run: |
          FAILURES=$(cargo test --all 2>&1 | grep -c "FAILED" || true)

          if [ $FAILURES -ge 6 ]; then
            echo "🔴 RED ANDON: $FAILURES failures (≥6)"
            echo "ACTION: Stop all merges, emergency team meeting"
            exit 1
          elif [ $FAILURES -ge 1 ]; then
            echo "🟡 YELLOW ANDON: $FAILURES failures (1-5)"
            echo "ACTION: Fix before next feature work"
            exit 1
          else
            echo "✅ GREEN ANDON: All tests passing"
          fi
```

**Benefits**: Immediate visibility into quality issues, prevents cascading failures, empowers team to stop and fix.

---

### 6. Genchi Genbutsu (Go and See)

**Manufacturing**: Go to the factory floor to understand problems firsthand.

**Software Mapping**:

```bash
# ❌ Remote Diagnosis: Trust CI logs only
$ grep "FAILED" ci_logs.txt
test_generation ... FAILED

# What does this actually mean? Hard to tell from logs alone.

# ✅ Genchi Genbutsu: Go to the actual test
$ cd tests/integration/
$ cargo test test_generation -- --nocapture

# Observe actual behavior:
# - See exact RDF input
# - Watch generation steps
# - Inspect generated output
# - Understand failure context

# ✅ Genchi Genbutsu: Read the actual code
$ cat tests/integration/test_generation.rs

// Now I see: test uses hardcoded path that doesn't exist!
let ontology = load_ontology("/tmp/missing.ttl"); // ← Problem!
```

**Gemba Walk Procedure**:

```markdown
## Weekly Gemba Walk: Code Generation Quality

**Time**: 30 minutes every Friday
**Participants**: 2-3 developers (rotating)

### Procedure

1. **Select 10 Random Tests** (use `shuf`)
   ```bash
   find tests/ -name "*.rs" | shuf -n 10
   ```

2. **Read Each Test** (3 min per test)
   - What is it testing?
   - Does it test real behavior or mock everything?
   - Are error cases covered?
   - Is it fast? (<100ms ideal)

3. **Apply 8-Point Checklist**
   - [ ] Tests real implementation (not mocked)
   - [ ] Clear failure messages
   - [ ] Detects bugs (not just syntax)
   - [ ] Clear setup/teardown
   - [ ] Debuggable (good logging)
   - [ ] Fast (<30s)
   - [ ] Isolated (no shared state)
   - [ ] Reproducible (not flaky)

4. **Record Findings** (score 1-5 per criterion)

5. **Identify Top 3 Issues** (prioritize by impact)

6. **Create Action Items** (assign owners)
```

**Benefits**: Ground truth understanding, surface hidden problems, build team intuition.

---

### 7. Kaizen / PDCA (Continuous Improvement)

**Manufacturing**: Small, continuous improvements driven by PDCA (Plan-Do-Check-Act) cycle.

**Software Mapping**:

```
┌─────────────────────────────────────────────────────────┐
│             PDCA Cycle for Code Generation              │
└─────────────────────────────────────────────────────────┘

Week 1: PLAN
  Problem: Test suite takes 12 minutes
  Goal: Reduce to <5 minutes
  Hypothesis: Duplicate tests cause waste
  Experiment: Identify and remove duplicates

Week 2: DO
  Action: Run deduplication analysis
  Found: 67 duplicate tests (13.7% of total)
  Action: Remove duplicates, consolidate test fixtures

Week 3: CHECK
  Measure: Test suite now runs in 8 minutes (33% improvement)
  Analysis: Still above goal. What else?
  Found: 23 tests sleep for fixed durations (waste)

Week 4: ACT
  Action: Replace sleep() with timeout polling
  Measure: Test suite now runs in 4.2 minutes (65% improvement)
  Result: Goal exceeded! ✅
  Standardize: Document pattern in code review checklist

Week 5: PLAN (next cycle)
  New Problem: 3 flaky tests causing CI failures
  Goal: Zero flaky tests
  Hypothesis: Race conditions in concurrent tests
  ...
```

**Kaizen Event Template**:

```markdown
## Kaizen Event: [Problem Statement]

**Date**: YYYY-MM-DD
**Duration**: 2-4 hours
**Participants**: [Names]

### Current State (Before)
- Metric 1: [Value]
- Metric 2: [Value]
- Pain Points: [List]

### Root Cause Analysis (5 Whys)
1. Why? [Answer]
2. Why? [Answer]
3. Why? [Answer]
4. Why? [Answer]
5. Why? [Answer → ROOT CAUSE]

### Target State (After)
- Metric 1: [Target]
- Metric 2: [Target]

### Action Plan
1. [Action Item 1] - Owner: [Name] - Due: [Date]
2. [Action Item 2] - Owner: [Name] - Due: [Date]

### Results (Actual)
- Metric 1: [Actual]
- Metric 2: [Actual]
- ROI: [Calculation]

### Lessons Learned
- What worked: [List]
- What didn't: [List]
- Next steps: [List]
```

**Benefits**: Systematic improvement, data-driven decisions, team engagement, compounding gains over time.

---

## Three Manufacturing Models

Software development can operate under three distinct models, each with different quality characteristics:

### Model 1: SCM (Source Code Mode) = Craft Shop

**Characteristics**:
- **Manual crafting**: Developers hand-write every line
- **Discretionary judgment**: Each developer decides how to implement
- **Artisan quality**: Quality varies by skill level
- **High touch**: Intensive human involvement
- **Slow feedback**: Quality known only after integration

**Manufacturing Analogy**: Blacksmith shop. Each sword is unique, quality depends on craftsperson.

**Code Example**:

```rust
// Craft Shop: Each developer implements differently

// Developer A's style
fn process_order_a(order: Order) {
    if order.items.len() > 0 {
        for item in &order.items {
            if item.quantity > 0 {
                calculate_price(item);
            }
        }
    }
}

// Developer B's style
fn process_order_b(order: Order) {
    order.items
        .iter()
        .filter(|i| i.quantity > 0)
        .for_each(|i| calculate_price(i));
}

// Developer C's style (async)
async fn process_order_c(order: Order) -> Result<(), Error> {
    let futures: Vec<_> = order.items
        .into_iter()
        .map(|item| tokio::spawn(async move { calculate_price(&item) }))
        .collect();

    for future in futures {
        future.await??;
    }
    Ok(())
}

// Same functionality, completely different implementations!
```

**Quality Profile**:
- Inconsistent patterns
- Varied error handling
- Different performance characteristics
- Hard to review (each review unique)
- High defect escape rate

---

### Model 2: CLM (Continuous Learning Mode) = Assembly Line

**Characteristics**:
- **Semi-automated**: Templates + manual refinement
- **Guided judgment**: Frameworks provide guardrails
- **Consistent quality**: Standardized patterns enforced
- **Medium touch**: Human-in-the-loop for decisions
- **Fast feedback**: Quality checks at each station

**Manufacturing Analogy**: Assembly line. Standardized process, workers at stations, quality gates between stations.

**Code Example**:

```rust
// Assembly Line: Framework enforces structure

// Station 1: Parse (cannot skip)
fn station_1_parse(input: &str) -> Result<ParsedOrder, ParseError> {
    serde_json::from_str(input)
        .map_err(|e| ParseError::JsonInvalid(e))
}

// Station 2: Validate (cannot skip)
fn station_2_validate(order: ParsedOrder) -> Result<ValidatedOrder, ValidationError> {
    if order.items.is_empty() {
        return Err(ValidationError::EmptyOrder);
    }

    for item in &order.items {
        if item.quantity == 0 {
            return Err(ValidationError::ZeroQuantity(item.id));
        }
    }

    Ok(ValidatedOrder(order))
}

// Station 3: Process (cannot skip)
fn station_3_process(order: ValidatedOrder) -> ProcessedOrder {
    let total = order.0.items.iter()
        .map(|item| calculate_price(item))
        .sum();

    ProcessedOrder {
        order_id: order.0.id,
        total,
        processed_at: Utc::now(),
    }
}

// Assembly Line Workflow (enforced order)
fn assembly_line(input: &str) -> Result<ProcessedOrder, OrderError> {
    let parsed = station_1_parse(input)?;      // ✓ Must pass
    let validated = station_2_validate(parsed)?; // ✓ Must pass
    let processed = station_3_process(validated); // ✓ Must complete
    Ok(processed)
}
```

**Quality Profile**:
- Consistent structure
- Mandatory validation
- Predictable flow
- Easy to review (same pattern every time)
- Moderate defect escape rate

**Inadmissible-Before Gates**: Each station requires previous station's output. Cannot skip validation.

---

### Model 3: CCM (Continuous Commitment Mode) = Robotic Factory

**Characteristics**:
- **Fully automated**: Generate from specification (RDF ontology)
- **Zero discretion**: Machine follows specification exactly
- **Guaranteed quality**: Impossible to produce invalid output
- **Zero touch**: No human intervention in generation
- **Instant feedback**: Quality verified at generation time

**Manufacturing Analogy**: Lights-out robotic factory. Robots work 24/7, quality sensors at every step, defects impossible by design.

**Code Example**:

```rust
// Robotic Factory: Generated from Ontology (no human writes this)

// Source of Truth: RDF Ontology
/*
@prefix : <http://example.org/ontology#> .

:Order a rdfs:Class ;
    sh:property [
        sh:path :items ;
        sh:minCount 1 ;              # ← Enforced at generation time
        sh:class :OrderItem ;
    ] .

:OrderItem a rdfs:Class ;
    sh:property [
        sh:path :quantity ;
        sh:datatype xsd:positiveInteger ;  # ← Enforced at generation time
        sh:minInclusive 1 ;
    ] .
*/

// Generated Code (automatic from ontology above)

/// Order (generated from ontology)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Order {
    /// Items: minCount=1 enforced by NonEmpty
    pub items: NonEmpty<OrderItem>,  // ← Impossible to create empty!
}

/// OrderItem (generated from ontology)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrderItem {
    /// Quantity: positiveInteger enforced by type
    pub quantity: PositiveInteger,  // ← Impossible to be zero!
}

// Custom Types (generated from XSD datatypes)

/// PositiveInteger: 1..=u32::MAX (zero impossible)
#[derive(Debug, Clone, Copy)]
pub struct PositiveInteger(std::num::NonZeroU32);

impl PositiveInteger {
    pub fn new(value: u32) -> Result<Self, ValueError> {
        NonZeroU32::new(value)
            .map(PositiveInteger)
            .ok_or(ValueError::MustBePositive)
    }
}

/// NonEmpty list (at least one element)
#[derive(Debug, Clone)]
pub struct NonEmpty<T> {
    head: T,
    tail: Vec<T>,
}

impl<T> NonEmpty<T> {
    pub fn new(head: T, tail: Vec<T>) -> Self {
        NonEmpty { head, tail }  // ← Cannot construct empty!
    }
}

// Processing function (no validation needed!)
fn process_order(order: Order) -> ProcessedOrder {
    // No need to check:
    // - ✅ Items guaranteed non-empty (type system)
    // - ✅ Quantities guaranteed positive (type system)

    let total = order.items.iter()
        .map(|item| item.quantity.get() * item.price)  // Safe!
        .sum();

    ProcessedOrder { total }
}
```

**Quality Profile**:
- Perfect consistency (machine-generated)
- Impossible to produce invalid states (type system)
- Zero validation overhead (guaranteed valid)
- Instant review (diff ontology, not code)
- **Zero defect escape rate** (defects impossible)

**Inadmissible-Before Gates**: Compilation fails if ontology constraints violated. Invalid code cannot exist.

---

### Comparison Table

| Aspect | SCM (Craft Shop) | CLM (Assembly Line) | CCM (Robotic Factory) |
|--------|------------------|---------------------|----------------------|
| **Automation** | Manual | Semi-automated | Fully automated |
| **Quality Variance** | High (depends on artisan) | Medium (process enforced) | Zero (machine-generated) |
| **Speed** | Slow (hand-crafted) | Medium (template + manual) | Fast (instant generation) |
| **Defect Rate** | 5-15% | 1-5% | 0% (impossible) |
| **Scalability** | Poor (N developers for N features) | Good (frameworks scale) | Excellent (unlimited) |
| **Validation Cost** | High (manual review) | Medium (automated + manual) | Zero (compile-time) |
| **Consistency** | Low | High | Perfect |
| **Learning Curve** | Medium | High | Low (ontology-first) |
| **Innovation** | High (total freedom) | Medium (within framework) | Low (spec-driven) |

---

## Inadmissible-Before Gates

**Definition**: A quality gate that prevents work from proceeding to the next stage until criteria are met. Defects cannot pass through.

### Manufacturing Example

```
[Raw Steel] → Gate 1: Inspect → [Inspected Steel] → Gate 2: Heat → [Heated Steel]
                 ↓ FAIL                                  ↓ FAIL
              [Reject Bin]                            [Reject Bin]
```

If steel has cracks (Gate 1 fails), it **cannot** proceed to heating. Defective steel is inadmissible.

### Software Examples

#### Gate 1: Type Checking (Compile-Time)

```rust
// Inadmissible: Non-positive quantity
struct OrderItem {
    quantity: PositiveInteger,  // ← Gate: Must be > 0
}

// ❌ REJECTED AT COMPILE TIME
let item = OrderItem {
    quantity: 0,  // ❌ Type error: expected PositiveInteger, found i32
};

// ✅ ADMITTED
let item = OrderItem {
    quantity: PositiveInteger::new(5)?,  // ✅ Passes gate
};
```

#### Gate 2: SHACL Validation (Generation-Time)

```turtle
# Ontology Constraint (SHACL shape)
:OrderShape a sh:NodeShape ;
    sh:targetClass :Order ;
    sh:property [
        sh:path :items ;
        sh:minCount 1 ;  # ← Gate: Must have at least 1 item
    ] .
```

```rust
// Generation Pipeline

fn generate_code(ontology: &Graph) -> Result<GeneratedCode, GenerationError> {
    // Gate 1: Parse
    let parsed = parse_ontology(ontology)?;

    // Gate 2: SHACL Validation (inadmissible-before)
    validate_shacl(&parsed)?;  // ← If fails, CANNOT proceed
    //     ↓ FAIL
    // return Err(ValidationError)

    // Gate 3: Code Generation (only valid ontologies reach here)
    let code = render_templates(&parsed)?;

    // Gate 4: Compilation Check
    compile_check(&code)?;  // ← If fails, CANNOT return
    //     ↓ FAIL
    // return Err(CompileError)

    Ok(code)  // ← Only valid, compilable code reaches user
}
```

### CLM/CCM Enforcement Mechanisms

#### CLM (Assembly Line): Manual Gates

```rust
// CLM: Developer must manually check each gate

fn assembly_line_process(input: &str) -> Result<Order, Error> {
    // Station 1: Parse
    let parsed = parse_json(input)?;
    // ↑ Developer calls this manually

    // Station 2: Validate (manual gate)
    if parsed.items.is_empty() {
        return Err(Error::EmptyOrder);  // ← Developer writes this check
    }
    // ↑ Developer must remember to check

    // Station 3: Process
    Ok(process(parsed))
}
```

**Problem**: Developer can forget gates (human error).

```rust
// ❌ Oops! Forgot validation gate
fn buggy_process(input: &str) -> Result<Order, Error> {
    let parsed = parse_json(input)?;
    // ← Missing validation gate!
    Ok(process(parsed))  // ← Invalid data can reach here!
}
```

#### CCM (Robotic Factory): Automatic Gates

```rust
// CCM: Type system enforces gates (impossible to forget)

#[derive(Debug)]
struct ParsedOrder { /* ... */ }  // ← Can be invalid

#[derive(Debug)]
struct ValidatedOrder(ParsedOrder);  // ← Guaranteed valid (newtype pattern)

// Gate 1: Parse (returns potentially invalid order)
fn gate_1_parse(input: &str) -> Result<ParsedOrder, ParseError> {
    serde_json::from_str(input)
}

// Gate 2: Validate (ONLY way to get ValidatedOrder)
fn gate_2_validate(order: ParsedOrder) -> Result<ValidatedOrder, ValidationError> {
    if order.items.is_empty() {
        return Err(ValidationError::EmptyOrder);
    }
    Ok(ValidatedOrder(order))  // ← Type change signals validation
}

// Processing requires ValidatedOrder (gate enforced by type system)
fn process(order: ValidatedOrder) -> ProcessedOrder {
    // ✅ Guaranteed valid (type system enforces it)
    let total = order.0.items.iter().map(|i| i.price).sum();
    ProcessedOrder { total }
}

// ❌ IMPOSSIBLE to process unvalidated order
fn main() {
    let parsed = gate_1_parse(INPUT)?;

    // ❌ Compile error: expected ValidatedOrder, found ParsedOrder
    process(parsed);  // ← TYPE MISMATCH

    // ✅ Must validate first
    let validated = gate_2_validate(parsed)?;
    process(validated);  // ← OK
}
```

**Key Insight**: Different types for different states → compiler enforces gates.

---

## Discretionary Fixes: Bypassing the Line

**Definition**: Manual workarounds that bypass standardized processes. Common in craft shops, impossible in robotic factories.

### Craft Shop (SCM): Discretionary Fixes Everywhere

```rust
// Developer encounters bug in generated code

// ❌ Discretionary Fix: Edit generated file directly
// File: generated/models.rs (GENERATED - DO NOT EDIT)

pub struct User {
    pub name: String,
    pub email: String,
    // FIXME: Generator bug - age should be Option<u32>
    pub age: u32,  // ← Developer manually changed this
}

// Problem 1: Fix lost on next generation
// Problem 2: No audit trail (why was this changed?)
// Problem 3: Other developers don't know about workaround
// Problem 4: Root cause never fixed (generator still buggy)
```

### Assembly Line (CLM): Some Discretion Allowed

```rust
// CLM: Escape hatches for manual fixes

// Generated code with override mechanism
#[derive(Debug, Serialize, Deserialize)]
pub struct User {
    pub name: String,
    pub email: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub age: Option<u32>,  // ← Override applied
}

// Override file: overrides/user.json
{
  "field_overrides": {
    "User.age": {
      "type": "Option<u32>",
      "reason": "Age is optional in UI (generator bug #123)"
    }
  }
}

// Problem: Still bypassing root cause fix
// Benefit: Override is tracked and auditable
```

### Robotic Factory (CCM): No Discretion (Fix at Source)

```rust
// CCM: Cannot bypass. Must fix ontology (source of truth).

// Step 1: Identify problem
// Generated code has: pub age: u32  (wrong)
// Should be: pub age: Option<u32>

// Step 2: Fix ontology (source of truth)
// File: ontologies/user.ttl

# BEFORE (bug)
:User a rdfs:Class ;
    rdfs:property [
        sh:path :age ;
        sh:datatype xsd:positiveInteger ;
        sh:minCount 1 ;  # ← Bug: Required field
    ] .

# AFTER (fixed)
:User a rdfs:Class ;
    rdfs:property [
        sh:path :age ;
        sh:datatype xsd:positiveInteger ;
        sh:minCount 0 ;  # ← Fixed: Optional field
        sh:maxCount 1 ;
    ] .

// Step 3: Regenerate (automatic)
$ ggen sync

// Step 4: Result
pub struct User {
    pub name: String,
    pub email: String,
    pub age: Option<u32>,  // ← Correct (no manual edit needed)
}

// Benefits:
// ✅ Fix is permanent (in source of truth)
// ✅ All consumers get fix (automatic regeneration)
// ✅ Audit trail (git log on ontology)
// ✅ No drift between spec and code
```

### Why CCM Prevents Bypasses

1. **Generated Code is Read-Only**: Files marked `// GENERATED - DO NOT EDIT` enforced by CI
2. **No Override Mechanism**: No escape hatches (by design)
3. **Root Cause Fixing Required**: Only way to change output is fix input (ontology)
4. **Regeneration is Fast**: <5s to regenerate entire codebase → no incentive to bypass

---

## Software Development Mappings

Comprehensive mapping of TPS principles to software development:

### Complete Mapping Table

| TPS Principle | Manufacturing | Software Development | ggen Implementation |
|---------------|---------------|---------------------|---------------------|
| **JIT (Just-In-Time)** | Produce only what's ordered | Generate only requested languages | `ggen sync --language rust` (selective generation) |
| **Jidoka (Autonomation)** | Machine stops on defect | Compilation fails on invalid output | `compile_check()` in pipeline |
| **Heijunka (Load Leveling)** | Even production rate | Streaming generation (constant memory) | Iterator-based rendering (not `Vec::collect()`) |
| **Kanban (Pull System)** | Visual workflow cards | Dependency-based pipeline | `requires: [validated_graph]` in workflow |
| **Standard Work** | Document best method | Standardized generation workflow | `standard_generation_workflow()` function |
| **Poka-Yoke (Error-Proofing)** | Asymmetric connectors | Type-safe APIs (enums, not strings) | `enum Language { Rust, TypeScript }` |
| **Andon (Stop the Line)** | Red light stops assembly | CI fails on quality threshold | Red alert at >5% failure rate |
| **Genchi Genbutsu (Go See)** | Walk factory floor | Read actual test code (not just CI logs) | Weekly Gemba walks on test suite |
| **Kaizen (Continuous Improvement)** | PDCA cycles | Weekly improvement sprints | Metrics tracking + action items |
| **5 Whys** | Root cause analysis | Ask "why" 5 times to find bug source | Documented in issue templates |
| **Takt Time** | Match production to demand | Match generation speed to CI timeout | Target <5s for full sync |
| **Muda (Waste Elimination)** | Remove non-value steps | Remove duplicate tests, dead code | Automated duplication detection |
| **Mura (Consistency)** | Standardize process | Standardize test structure (AAA pattern) | Enforced by linters |
| **Muri (Overburden Prevention)** | Don't overload machines | Split mega-tests into focused tests | Max 50 lines per test |

---

## Visual Representations

### Visual 1: TPS as Manufacturing OS

```
┌────────────────────────────────────────────────────────────────┐
│                  TPS: The Manufacturing OS                      │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌──────────────┐           ┌──────────────┐                  │
│  │   JIT Pillar │           │ Jidoka Pillar│                  │
│  │  (Right Time)│           │ (Right Quality)│                │
│  └──────┬───────┘           └──────┬────────┘                 │
│         │                           │                          │
│         └────────────┬──────────────┘                          │
│                      │                                         │
│         ┌────────────▼────────────┐                            │
│         │   Core TPS Methods      │                            │
│         ├─────────────────────────┤                            │
│         │ • Heijunka (Leveling)   │                            │
│         │ • Kanban (Pull System)  │                            │
│         │ • Standard Work         │                            │
│         │ • Poka-Yoke (Error-Proof)│                           │
│         │ • Andon (Stop the Line) │                            │
│         │ • Genchi Genbutsu (Go See)│                          │
│         │ • Kaizen/PDCA (Improve) │                            │
│         └────────────┬────────────┘                            │
│                      │                                         │
│                      ▼                                         │
│         ┌──────────────────────────┐                           │
│         │   Output: High Quality   │                           │
│         │   • Zero Defects         │                           │
│         │   • Minimal Waste        │                           │
│         │   • Continuous Flow      │                           │
│         └──────────────────────────┘                           │
│                                                                 │
└────────────────────────────────────────────────────────────────┘
```

### Visual 2: Three Manufacturing Models

```
┌───────────────────────────────────────────────────────────────────┐
│         SCM (Craft Shop) vs CLM (Assembly) vs CCM (Robotic)       │
└───────────────────────────────────────────────────────────────────┘

SCM: Craft Shop (Manual)
──────────────────────────
    ┌─────┐   ┌─────┐   ┌─────┐
    │ 🧑‍🎨  │   │ 🧑‍🎨  │   │ 🧑‍🎨  │   Individual artisans
    └──┬──┘   └──┬──┘   └──┬──┘
       │         │         │
       ▼         ▼         ▼
    [Code]    [Code]    [Code]    Variable quality
       │         │         │
       └────┬────┴────┬────┘
            ▼         ▼
         [Review]  [Review]      Manual inspection
            │         │
            ▼         ▼
        [Deploy]  [Deploy]       Hope for the best

Quality: 85-95% (depends on artisan)
Speed: Slow (weeks)
Consistency: Low


CLM: Assembly Line (Semi-Automated)
────────────────────────────────────
    ┌────────┐   ┌────────┐   ┌────────┐
    │Station1│──▶│Station2│──▶│Station3│  Standardized stations
    │ Parse  │   │Validate│   │Generate│
    └───┬────┘   └───┬────┘   └───┬────┘
        │            │            │
        ▼            ▼            ▼
      [Gate]       [Gate]       [Gate]     Quality gates
        │            │            │
        ✅           ✅           ✅
        │            │            │
        └────────────┴────────────┘
                     │
                     ▼
                 [Output]                  Consistent quality

Quality: 95-99% (process enforced)
Speed: Medium (days)
Consistency: High


CCM: Robotic Factory (Fully Automated)
───────────────────────────────────────
    ┌──────────┐
    │Ontology  │                          Single source of truth
    │  (RDF)   │
    └────┬─────┘
         │
         ▼
    ┌────────────┐
    │  SHACL     │                         Validation (compile-time)
    │  Validate  │
    └─────┬──────┘
          │ ✅
          ▼
    ┌──────────────┐
    │  Generator   │◀───[Templates]        Automated generation
    │  🤖          │
    └──────┬───────┘
           │
           ▼
    ┌──────────────┐
    │  Type Check  │                       Impossible to fail
    │  ✅          │
    └──────┬───────┘
           │
           ▼
    ┌──────────────┐
    │  Valid Code  │                       Guaranteed quality
    └──────────────┘

Quality: 100% (defects impossible)
Speed: Fast (seconds)
Consistency: Perfect
```

### Visual 3: Inadmissible-Before Gates

```
┌─────────────────────────────────────────────────────────────┐
│              Inadmissible-Before Gates                       │
│         (Defects Cannot Proceed to Next Stage)              │
└─────────────────────────────────────────────────────────────┘

Input         Gate 1         Gate 2         Gate 3      Output
              [Parse]       [Validate]    [Generate]
  │               │              │             │           │
  │               │              │             │           │
  ▼               ▼              ▼             ▼           ▼
[RDF]───────▶ ┌─────┐      ┌─────┐      ┌─────┐     [Code]
 .ttl         │ 🚦  │ ✅   │ 🚦  │ ✅   │ 🚦  │ ✅   .rs
              └──┬──┘      └──┬──┘      └──┬──┘
                 │ ❌         │ ❌         │ ❌
                 ▼            ▼            ▼
              [Reject]     [Reject]     [Reject]   Defects stop here
              Syntax       SHACL        Compile
              Error        Error        Error


Example: Gate 2 (SHACL Validation)
───────────────────────────────────

Valid Input:                 Invalid Input:
:User a rdfs:Class ;         :User a rdfs:Class ;
    :hasEmail "a@b.com" .        # Missing email!

         ▼                            ▼
    ┌─────────┐                 ┌─────────┐
    │ SHACL   │                 │ SHACL   │
    │ Check   │                 │ Check   │
    └────┬────┘                 └────┬────┘
         │                           │
         ✅ PASS                     ❌ FAIL
         │                           │
         ▼                           ▼
    [Proceed]                   [REJECT]
    to Gate 3                   Error: Email required
                                (Cannot proceed)
```

### Visual 4: Discretionary Fixes (SCM vs CCM)

```
┌──────────────────────────────────────────────────────────────┐
│           Discretionary Fixes: SCM vs CCM                     │
└──────────────────────────────────────────────────────────────┘

SCM (Craft Shop): Fixes Bypass the Line
────────────────────────────────────────

[Generator] ──▶ [Code.rs] ──▶ [Bug Found!]
                    │              │
                    │              │ 😰 Developer panics
                    │              │
                    │              ▼
                    │         [Quick Fix]
                    │         Edit code.rs directly
                    │              │
                    └──────────────┘
                           ❌ Bypass!
                    (Root cause never fixed)

Result:
• Generator still buggy
• Fix lost on next generation
• No audit trail
• Other devs unaware


CCM (Robotic Factory): Must Fix at Source
──────────────────────────────────────────

[Ontology.ttl] ──▶ [Generator] ──▶ [Code.rs] ──▶ [Bug Found!]
       ▲                                              │
       │                                              │
       │ ✅ Fix ontology                             │
       │    (Source of truth)                         │
       │                                              ▼
       └──────────────────────────────────────── [Issue Reported]
                                                 "Generator produces
                                                  wrong type for X"

Developer workflow:
1. Find bug in generated code
2. Cannot edit (file is read-only)
3. Trace to ontology (source)
4. Fix ontology
5. Regenerate
6. Bug fixed permanently

Result:
• Generator improved (benefits all)
• Fix is permanent
• Git log captures why
• All consumers get fix
```

### Visual 5: Andon System

```
┌──────────────────────────────────────────────────────────────┐
│                    Andon Alert System                         │
│            (Visual Management for Quality)                    │
└──────────────────────────────────────────────────────────────┘

Metrics Dashboard:
┌────────────────────────────────────────────┐
│  Test Health Monitor                       │
├────────────────────────────────────────────┤
│  Total Tests: 487                          │
│  Failures: 2 (0.4%)        🟢 GREEN       │
│  Flaky: 3 (0.6%)                          │
│  Avg Time: 4.2s                            │
└────────────────────────────────────────────┘

Status Thresholds:

🟢 GREEN (All Good)
   • Failure rate: 0-2%
   • Flaky rate: 0-2%
   • Avg time: <10s

   Action: Continue normal operations

🟡 YELLOW (Warning)
   • Failure rate: 2-5%
   • OR flaky rate: 2-5%
   • OR avg time: 10-30s

   Action: Investigate before next feature

🔴 RED (Critical)
   • Failure rate: >5%
   • OR flaky rate: >5%
   • OR avg time: >30s

   Action: STOP THE LINE
           Emergency team meeting
           No new features until fixed


Andon Cord Workflow:

Developer runs tests locally:
   └─▶ cargo test
           │
           ▼
      ┌─────────┐
      │ 8 FAILED│ ──▶ 🔴 RED ANDON
      └─────────┘        │
                         ▼
                   [STOP WORK]
                         │
                         ▼
                   [Team Meeting]
                         │
                         ▼
                   [Root Cause]
                         │
                         ▼
                   [Fix Applied]
                         │
                         ▼
                   [Tests Pass]
                         │
                         ▼
                   🟢 GREEN
                   Resume work
```

---

## Case Study: ggen Implementation

How ggen implements TPS as manufacturing OS:

### Architecture Overview

```
ggen: RDF-Driven Code Generation
─────────────────────────────────

Source of Truth: Ontology (.ttl files)
         │
         ▼
    ┌─────────┐
    │ Parse   │ ◀── JIT: Load only requested ontologies
    │ (μ₁)    │
    └────┬────┘
         │ ✅ Gate 1: Valid RDF
         ▼
    ┌─────────┐
    │Validate │ ◀── Jidoka: SHACL shapes enforce constraints
    │ (μ₂)    │
    └────┬────┘
         │ ✅ Gate 2: Shapes pass
         ▼
    ┌─────────┐
    │Extract  │ ◀── Standard Work: Always extract in same order
    │ (μ₃)    │
    └────┬────┘
         │ ✅ Gate 3: Classes + properties found
         ▼
    ┌─────────┐
    │Transform│ ◀── Heijunka: Streaming (constant memory)
    │ (μ₄)    │
    └────┬────┘
         │ ✅ Gate 4: IR well-formed
         ▼
    ┌─────────┐
    │Generate │ ◀── Poka-Yoke: Templates type-checked
    │ (μ₅)    │
    └────┬────┘
         │ ✅ Gate 5: Code compiles
         ▼
    [Output] ◀── Andon: CI monitors quality metrics
```

### TPS Principles in ggen

#### 1. JIT: Generate Only What's Needed

```bash
# ❌ Push: Generate everything
$ ggen sync  # Generates all languages (waste if only need Rust)

# ✅ JIT: Generate on demand
$ ggen sync --language rust        # Only Rust
$ ggen sync --language typescript  # Only TypeScript
$ ggen sync --class User           # Only User class
```

#### 2. Jidoka: Built-In Quality

```rust
// ggen pipeline with jidoka (auto-stops on defect)

pub fn pipeline(ontology_path: &Path) -> Result<GeneratedCode, PipelineError> {
    // μ₁: Parse (jidoka gate 1)
    let graph = parse_ontology(ontology_path)
        .map_err(|e| PipelineError::ParseFailed(e))?;
    //  ↓ If parse fails, STOP (cannot proceed with invalid RDF)

    // μ₂: Validate (jidoka gate 2)
    validate_shacl(&graph)
        .map_err(|e| PipelineError::ValidationFailed(e))?;
    //  ↓ If validation fails, STOP (cannot proceed with constraint violations)

    // μ₃: Extract (jidoka gate 3)
    let classes = extract_classes(&graph)?;
    let properties = extract_properties(&graph)?;

    // μ₄: Transform (jidoka gate 4)
    let ir = build_ir(classes, properties)?;

    // μ₅: Generate (jidoka gate 5)
    let code = render_templates(&ir)?;

    // Final gate: Compile check
    compile_check(&code)
        .map_err(|e| PipelineError::CompileFailed(e))?;
    //  ↓ If compile fails, STOP (cannot return invalid code)

    Ok(code)  // ← Only valid, compilable code reaches here
}
```

#### 3. Andon: CI Quality Monitoring

```yaml
# .github/workflows/andon.yml

jobs:
  andon_monitor:
    runs-on: ubuntu-latest
    steps:
      - name: Run Full Test Suite
        run: cargo make test

      - name: Calculate Metrics
        id: metrics
        run: |
          TOTAL=$(cargo test --all 2>&1 | grep -c "test " || true)
          FAILURES=$(cargo test --all 2>&1 | grep -c "FAILED" || true)
          RATE=$(echo "scale=2; $FAILURES / $TOTAL * 100" | bc)
          echo "failure_rate=$RATE" >> $GITHUB_OUTPUT

      - name: Andon Check
        run: |
          RATE=${{ steps.metrics.outputs.failure_rate }}
          if (( $(echo "$RATE > 5.0" | bc -l) )); then
            echo "🔴 RED ANDON: ${RATE}% failure rate (>5%)"
            echo "STOP THE LINE - Emergency meeting required"
            exit 1
          elif (( $(echo "$RATE > 2.0" | bc -l) )); then
            echo "🟡 YELLOW ANDON: ${RATE}% failure rate (>2%)"
            echo "Investigate before next feature"
            exit 1
          else
            echo "✅ GREEN ANDON: ${RATE}% failure rate"
          fi
```

#### 4. Gemba Walk: Weekly Test Inspection

```bash
#!/bin/bash
# scripts/gemba_walk.sh

echo "=== Gemba Walk: ggen Test Quality Inspection ==="
echo "Date: $(date)"
echo

# Select 10 random tests
TESTS=$(find tests/ -name "*.rs" -type f | shuf -n 10)

for test_file in $TESTS; do
    echo "────────────────────────────────────"
    echo "Inspecting: $test_file"
    echo

    # Count assertions
    assertions=$(grep -c "assert" "$test_file" || true)
    echo "Assertions: $assertions"

    # Check for sleeps (waste indicator)
    sleeps=$(grep -c "sleep" "$test_file" || true)
    if [ $sleeps -gt 0 ]; then
        echo "⚠️  Warning: Contains $sleeps sleep() calls (potential waste)"
    fi

    # Check for mocking (jidoka indicator)
    mocks=$(grep -c "mock" "$test_file" || true)
    if [ $mocks -gt 5 ]; then
        echo "⚠️  Warning: Heavy mocking ($mocks mocks) - testing real behavior?"
    fi

    # Run test and measure time
    test_name=$(basename "$test_file" .rs)
    time_output=$(cargo test --test "$test_name" 2>&1 | grep "test result")
    echo "Result: $time_output"
    echo
done

echo "=== Gemba Walk Complete ==="
echo "Action Items:"
echo "1. Address tests with sleep() calls"
echo "2. Review heavily mocked tests"
echo "3. Celebrate tests with good assertions"
```

#### 5. Kaizen: Weekly Improvement Metrics

```markdown
## Week 47 Kaizen Report

### Metrics (This Week)

| Metric | Last Week | This Week | Δ |
|--------|-----------|-----------|---|
| Test count | 487 | 468 | -19 (removed duplicates) |
| Test time | 12m 34s | 4m 12s | -66% ⬆️ |
| Flaky tests | 23 | 3 | -87% ⬆️ |
| Coverage | 85% | 87% | +2% ⬆️ |
| Generation time | 8.2s | 4.7s | -43% ⬆️ |

### Action Items (Completed)

- ✅ Removed 67 duplicate tests (Muda elimination)
- ✅ Fixed 20 flaky tests (Jidoka improvement)
- ✅ Replaced sleep() with timeouts (Muda elimination)
- ✅ Split 12 mega-tests (Muri reduction)

### Action Items (Next Week)

- [ ] Add property-based tests for parsers
- [ ] Implement parallel test execution
- [ ] Document test patterns in TESTING.md

### Lessons Learned

**What worked:**
- Gemba walks identified specific waste
- Andon system prevented cascading failures
- Team engagement in PDCA process

**What didn't:**
- Initial resistance to removing "comprehensive" tests
- Learning curve for streaming generation

**Root Cause (5 Whys):**
Why slow tests? → Duplicate tests
Why duplicates? → No deduplication check
Why no check? → Not in code review checklist
Why not in checklist? → Nobody proposed it
Why nobody proposed? → No Kaizen culture
→ **Root Cause: Missing continuous improvement culture**

**Countermeasure:** Established weekly Kaizen meetings
```

---

## Conclusion

### Key Takeaways

1. **TPS is an Operating System**: Just like Linux manages processes, TPS manages manufacturing processes. In software, it manages code generation processes.

2. **Two Pillars are Essential**: JIT (right thing, right time) + Jidoka (built-in quality) work together. Neither alone is sufficient.

3. **Full Method Stack Multiplies Impact**: Heijunka, Kanban, Standard Work, Poka-Yoke, Andon, Genchi Genbutsu, and Kaizen are not optional extras—they're the implementation details of the OS.

4. **Three Models Have Different Quality**:
   - SCM (Craft Shop): 85-95% quality, high variance
   - CLM (Assembly Line): 95-99% quality, enforced process
   - CCM (Robotic Factory): 100% quality, defects impossible

5. **Inadmissible-Before Gates Prevent Defects**: Type systems and compile-time checks make invalid states impossible, not just unlikely.

6. **Discretionary Fixes Must Be Eliminated**: CCM forces fixes at the source (ontology), preventing drift and ensuring permanent solutions.

### The Paradigm Shift

Traditional software development is stuck in **SCM (Craft Shop)** mode:
- Developers hand-craft every line
- Quality depends on individual skill
- Reviews are subjective
- Defects escape to production

TPS-driven development operates in **CCM (Robotic Factory)** mode:
- Ontologies define specifications
- Generators produce consistent code
- Type systems prevent defects
- Quality is guaranteed by construction

### Next Steps

1. **Start Small**: Apply one TPS principle (e.g., Andon) to your test suite
2. **Measure Impact**: Track metrics before/after (use Kaizen framework)
3. **Expand Gradually**: Add more principles over time (PDCA cycle)
4. **Build Culture**: Make continuous improvement a team habit, not a one-time event

### Further Reading

- [Lean Manufacturing Introduction](../diataxis/tutorials/04-lean-manufacturing-intro.md)
- [Andon + Gemba Playbook](../lean_quality/ANDON_GEMBA_PLAYBOOK.md)
- [Lean Vocabulary Reference](../diataxis/reference/lean-vocabulary.md)
- [Poka-Yoke Patterns](../diataxis/reference/poka-yoke-patterns.md)

---

**Document Version**: 1.0
**Author**: ggen Documentation Team
**Last Updated**: 2026-02-09
**Status**: Complete
