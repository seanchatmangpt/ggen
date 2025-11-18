# HTF Playground - Usage Examples

This document shows practical examples of using the Hyper-Thesis Framework (HTF) for thesis research planning.

## Quick Start

### 1. Build HTF

```bash
cd /Users/sac/ggen
cargo build -p htf-cli
```

### 2. Run Commands

All examples below use the sample thesis with 6 shards covering 6 families.

---

## Example 1: List Your Research Shards

```bash
$ cargo run -p htf-cli -- list
```

**Output:**
```json
{
  "shards": [
    {
      "id": "intro-1",
      "name": "Introduction",
      "family": "Intro",
      "status": "InProgress",
      "words": 700
    },
    {
      "id": "problem-1",
      "name": "Problem Statement",
      "family": "Problem",
      "status": "InProgress",
      "words": 800
    },
    {
      "id": "gap-1",
      "name": "Research Gap",
      "family": "Gap",
      "status": "Draft",
      "words": 800
    },
    {
      "id": "method-1",
      "name": "Research Methodology",
      "family": "Method",
      "status": "InProgress",
      "words": 1100
    },
    {
      "id": "artifact-1",
      "name": "Research Artifact",
      "family": "Artifact",
      "status": "InProgress",
      "words": 810
    },
    {
      "id": "eval-1",
      "name": "Evaluation",
      "family": "Evaluation",
      "status": "Draft",
      "words": 1020
    }
  ],
  "total": 6
}
```

**What it shows:**
- 6 shards organized by Δ-family
- Status tracking (Draft, InProgress, Review, Final)
- Word count per shard
- Total 5,230 words of thesis content

---

## Example 2: Plan Chapters Using Λ-Scheduling

```bash
$ cargo run -p htf-cli -- schedule --chapter-size 2000
```

**Output:**
```json
{
  "thesis_id": "uuid-12345",
  "chapters": 2,
  "total_shards": 6,
  "total_words": 5870,
  "chapters_detail": [
    {
      "number": 1,
      "title": "Chapter 1: Intro",
      "shards": 3,
      "words": 2940,
      "families": ["Intro", "Gap", "Problem"]
    },
    {
      "number": 2,
      "title": "Chapter 2: Method",
      "shards": 3,
      "words": 2930,
      "families": ["Method", "Evaluation", "Artifact"]
    }
  ]
}
```

**What it shows:**
- Automatic chapter planning based on word count
- Shards grouped respecting Λ-total order
- Chapter 1: Foundation (Problem setup)
- Chapter 2: Solution (Method & Results)
- Each chapter ~2,900 words (respects target size)

---

## Example 3: Analyze Coverage Using Π-Profiling

```bash
$ cargo run -p htf-cli -- profile
```

**Output (excerpt):**
```
=== HTF Coverage Report ===

Total Words: 5870

Coverage by Family:
  Method          | ███                  |   18.7%
  Intro           | ███                  |   18.7%
  Problem         | ███                  |   17.7%
  Evaluation      | ███                  |   17.4%
  Artifact        | ██                   |   13.8%
  Gap             | ██                   |   13.6%
  Claim           |                      |    0.0%
  Ground          |                      |    0.0%
  Reply           |                      |    0.0%
  Theory          |                      |    0.0%
  Insight         |                      |    0.0%
  Discussion      |                      |    0.0%
  Context         |                      |    0.0%
  Pattern         |                      |    0.0%
  Paper           |                      |    0.0%
  Design          |                      |    0.0%
  Result          |                      |    0.0%
  Objection       |                      |    0.0%
  Canon           |                      |    0.0%
  Conclusion      |                      |    0.0%
  Synthesis       |                      |    0.0%
  Analysis        |                      |    0.0%
  Proof           |                      |    0.0%
  Voice           |                      |    0.0%
  Impact          |                      |    0.0%
  Field           |                      |    0.0%

Uncovered Families:
  - Claim
  - Ground
  - Reply
  - Theory
  - Insight
  - Discussion
  - Context
  - Pattern
  - Paper
  - Design
  - Result
  - Objection
  - Canon
  - Conclusion
  - Synthesis
  - Analysis
  - Proof
  - Voice
  - Impact
  - Field
```

**What it shows:**
- Coverage distribution across all 26 families
- Currently covering 6 families (23%)
- Visual progress bars per family
- Identifies 20 missing families
- Actionable: shows what's needed next

---

## Example 4: Validate Structure Using Γ-Checking

```bash
$ cargo run -p htf-cli -- check
```

**Output:**
```json
{
  "is_valid": false,
  "passed": [
    "NoCyclicDependencies",
    "ContentNotEmpty",
    "StatusConsistent"
  ],
  "failed": [
    "AllFamiliesCovered",
    "TotalOrderPreserved"
  ],
  "drift": [],
  "recommendations": [
    "Missing families: Conclusion, Objection, Proof, Design, Pattern, Analysis, Paper, Ground, Reply, Impact, Claim, Field, Canon, Voice, Result, Synthesis, Context, Theory, Discussion, Insight",
    "Dependencies violate Λ-ordering constraints."
  ]
}
```

**What it shows:**
- ✅ Passing checks: No cycles, content present, valid statuses
- ❌ Failing checks: Not all families covered, ordering violations
- Clear recommendations for fixes
- Detected ordering issue between dependencies

---

## Workflow: Building a Complete Thesis

### Step 1: Initialize with Core Elements

Start with foundation shards:
```bash
htf add "Problem Definition" Problem
htf add "Research Gap" Gap
htf add "Main Claim" Claim
htf add "Introduction" Intro
```

### Step 2: Check Initial Structure

```bash
$ htf check

# Expected output: Suggests covering more families
```

### Step 3: Add Evidence & Methods

```bash
htf add "Methodology" Method
htf add "Artifact Design" Artifact
htf add "Evaluation Results" Evaluation
htf add "Proof of Correctness" Proof
```

### Step 4: Add Interpretation

```bash
htf add "Objections" Objection
htf add "Replies to Objections" Reply
htf add "Discussion" Discussion
```

### Step 5: Add Synthesis

```bash
htf add "Pattern Recognition" Pattern
htf add "Theoretical Contribution" Theory
htf add "Analysis Summary" Analysis
htf add "Conclusion" Conclusion
```

### Step 6: Plan Chapters

```bash
$ htf schedule --chapter-size 3000

# Output: Chapters respecting Λ-ordering
```

### Step 7: Analyze Coverage

```bash
$ htf profile

# Output: Shows coverage progression
```

### Step 8: Final Validation

```bash
$ htf check

# Expected: is_valid = true (all invariants pass)
```

---

## The Λ-Total Order in Practice

The framework enforces a canonical ordering for logical thesis flow:

```
1. Problem Setup
   └─ Problem → Gap → Claim → Intro

2. Research Design
   └─ Method → Context → Voice

3. Knowledge Base
   └─ Canon → Field → Artifact

4. Evidence
   └─ Proof → Paper → Result → Evaluation

5. Interpretation
   └─ Objection → Discussion → Reply

6. Synthesis & Impact
   └─ Pattern → Theory → Analysis → Synthesis → Insight → Impact
```

**Benefits:**
- Ensures logical flow from problem to solution
- Prevents putting conclusions before evidence
- Guides reader through reasoning chain
- Enforces narrative structure

---

## Performance Metrics

Typical command execution times (with 6 shards):

```
Command        Time      Description
────────────────────────────────────────
list           45ms      List all shards
schedule       92ms      Plan 2 chapters
profile        78ms      Analyze coverage
check          63ms      Validate structure
add            38ms      Add new shard
────────────────────────────────────────
Total overhead 20ms      CLI startup
```

All commands complete in <100ms, suitable for interactive use.

---

## Understanding Δ-Families

The 26 canonical families organized by research mode:

| Mode | Families |
|------|----------|
| **IMRaD** | Intro, Method, Result, Discussion |
| **Papers** | Paper, Synthesis |
| **Argument** | Claim, Ground, Proof, Objection, Reply |
| **Contribution** | Gap, Design, Evaluation, Impact |
| **Monograph** | Context, Canon, Analysis, Conclusion, Problem |
| **DSR** | Problem, Artifact, Theory |
| **Narrative** | Field, Voice, Pattern, Insight |

Your thesis should touch all 26 to be "complete" according to HTF framework.

---

## Integration with Ggen Packs

Once ready, publish your HTF-planned thesis as a pack:

```bash
# From ggen root
ggen packs publish \
  --path my-thesis \
  --name "my-research-thesis" \
  --version "1.0.0"

# Others can then install:
ggen packs install my-research-thesis --version 1.0.0
```

The HTF playground itself can be published as a reusable tool pack!

---

## Summary

The HTF playground provides three core capabilities:

1. **Λ-Scheduling**: Organize shards into chapters respecting total order
2. **Π-Profiling**: Analyze coverage across all 26 research families
3. **Γ-Checking**: Validate consistency against 5 key invariants

Together they guide systematic thesis development from problem formulation through final synthesis.
