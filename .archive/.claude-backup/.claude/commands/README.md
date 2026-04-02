# Claude Code DfLSS Commands

Design for Lean Six Sigma (DfLSS) slash commands for systematic software quality engineering.

## What is DfLSS?

**DfLSS (Design for Lean Six Sigma)** combines:
- **Lean** - Eliminate waste (Muda, Mura, Muri)
- **Six Sigma** - Prevent defects through design
- **Design** - Build quality in from the start, not inspect it in later

**CRITICAL**: DfLSS ≠ DFSS (Design for Six Sigma). DFSS only addresses quality, missing critical waste elimination. DfLSS addresses BOTH efficiency (waste) AND quality (defects).

## Available Commands

All commands are invoked with `/dflss:<command-name>` in Claude Code.

### Core Quality Methods

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `/dflss:fmea` | Failure Mode & Effects Analysis | Identify and prevent potential failures before they occur |
| `/dflss:root-cause-analysis` | 5 Whys analysis | Find root causes of problems, not just symptoms |
| `/dflss:dmaic-problem-solving` | Define-Measure-Analyze-Improve-Control | Systematic data-driven problem solving |
| `/dflss:poka-yoke-design` | Error-proofing through type system | Make errors impossible at compile time |
| `/dflss:gemba-walk` | Go to the source | Verify actual code behavior, not assumptions |

### Continuous Improvement

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `/dflss:kaizen-improvement` | Small incremental improvements | Make continuous small improvements, not big rewrites |
| `/dflss:eliminate-muda` | Waste elimination (7 types) | Remove dead code, duplication, unnecessary complexity |
| `/dflss:eliminate-mura` | Unevenness elimination | Standardize patterns, reduce variation |
| `/dflss:80-20-fill-gaps` | 80/20 capability completion | Complete the 20% that delivers 80% of value |

### Design & Engineering

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `/dflss:dmedi-design-process` | Define-Measure-Explore-Develop-Implement | Design new features systematically |
| `/dflss:robust-design` | Robust parameter design | Design code that works across conditions |
| `/dflss:concept-selection` | Systematic concept evaluation | Choose best design from alternatives |
| `/dflss:triz-problem-solving` | Creative problem solving | Solve contradictions, find innovative solutions |

### Quality Signals & Monitoring

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `/dflss:andon-signals` | Visual quality alerts | Make quality issues immediately visible |
| `/dflss:voice-of-customer-qfd` | Voice of Customer → Quality Function Deployment | Translate requirements into design |

### Workflows & Integration

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `/dflss:acp` | Add-Commit-Push workflow | Complete git workflow with quality gates |
| `/dflss:release-preparation` | Release checklist | Prepare production-ready releases |
| `/dflss:verify-tests` | Test verification | Ensure tests are comprehensive and passing |
| `/dflss:expert-testing-patterns` | Advanced testing | Apply expert testing patterns |

### Validation

| Command | Purpose | When to Use |
|---------|---------|-------------|
| `/dflss:validate-generation` | Template generation validation | Verify template output is correct and deterministic |
| `/dflss:validate-rdf` | RDF/ontology validation | Validate semantic data structures |
| `/dflss:check-determinism` | Deterministic behavior checks | Ensure reproducible outputs |

## Usage in Claude Code

Each command file (`dflss-<name>.md`) can be invoked as a slash command:

```bash
/dflss:fmea
/dflss:gemba-walk
/dflss:poka-yoke-design
/dflss:kaizen-improvement
/dflss:eliminate-muda
/dflss:root-cause-analysis
/dflss:dmaic-problem-solving
/dflss:acp
/dflss:release-preparation
/dflss:80-20-fill-gaps
```

## Command Philosophy

All DfLSS commands follow these principles:

### 1. **Execution Over Documentation**
- ❌ **WRONG**: Create analysis documents and reports
- ✅ **RIGHT**: Create 10+ item todo lists and execute them
- **Principle**: Fix problems, don't document them

### 2. **Quality First**
- Quality, consistency, and maintainability are **high value**, not optional
- Prevent defects AND waste from the start (DfLSS principle)
- Design quality in, don't inspect it in later

### 3. **80/20 Thinking**
- Focus on the 20% that delivers 80% of value
- Value includes quality, consistency, maintainability
- High-quality work may require more effort, but it's still high value

### 4. **Type-Level Prevention**
- Use Rust's type system to prevent errors at compile time
- Make invalid states unrepresentable
- Compile-time > Runtime checks

## Quick Start

```bash
# Find root cause of a bug
/dflss:root-cause-analysis

# Prevent errors through types
/dflss:poka-yoke-design

# Eliminate waste
/dflss:eliminate-muda

# Make improvements
/dflss:kaizen-improvement

# Prepare release
/dflss:release-preparation

# Complete capabilities
/dflss:80-20-fill-gaps
```

## Integration with Build System

DfLSS commands integrate with Rust build system:

```bash
cargo make check          # Compilation check
cargo make test           # Run tests
cargo make lint           # Clippy linting
cargo make fmt            # Code formatting
cargo make pre-commit     # Pre-commit validation
cargo make pre-push       # Pre-push validation
```

---

**Remember**: DfLSS addresses BOTH efficiency (waste elimination) AND quality (defect prevention). Execute fixes, don't document them. Use the type system to prevent errors. Make continuous small improvements.
