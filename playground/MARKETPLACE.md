[VERSION] 0.1.0
[CATEGORY] Academic Research Tools
[TAGS] thesis, research, planning, academic, framework, rdf
[LICENSE] MIT
[AUTHOR] Sean Chatman <sean@chatmangpt.com>
[REPOSITORY] https://github.com/seanchatmangpt/ggen
[HOMEPAGE] https://github.com/seanchatmangpt/ggen/tree/master/playground

# Hyper-Thesis Framework (HTF)

**A sophisticated RDF-backed thesis planning system** for researchers, PhD candidates, and academic writers.

## Overview

HTF implements a unified μ-architecture that systematically organizes thesis research across 26 canonical families using:

- **Λ-Scheduling**: Automatic chapter planning respecting logical thesis flow
- **Π-Profiling**: Coverage analysis across all research dimensions
- **Γ-Checking**: Consistency validation against structural invariants

## Features

### ✅ Λ-Scheduler: Automatic Chapter Planning
```bash
htf schedule --chapter-size 2000
```
- Organizes research shards into chapters
- Enforces canonical Λ-total order
- Word-count aware grouping
- Respects dependency relationships
- Generates table of contents outline

### ✅ Π-Profiler: Coverage Analysis
```bash
htf profile
```
- Analyzes coverage across 26 research families
- Visual progress bars and statistics
- Identifies missing research categories
- Actionable recommendations for gaps
- Word count distribution per family

### ✅ Γ-Checker: Structural Validation
```bash
htf check
```
- Validates 5 core invariants:
  - AllFamiliesCovered: All 26 families present
  - NoCyclicDependencies: Dependencies form DAG
  - TotalOrderPreserved: Λ-ordering respected
  - ContentNotEmpty: Non-empty shards
  - StatusConsistent: Valid status transitions
- Detects structural drifts
- Provides repair recommendations

### ✅ Additional Commands
```bash
htf list              # List all research shards
htf add NAME FAMILY   # Add new research shard
htf export --format FORMAT  # Export thesis structure
```

## The 26 Δ-Families

HTF organizes research into 26 canonical categories:

### IMRaD (4)
- `Intro` - Introduction and context setting
- `Method` - Research methodology and approach
- `Result` - Empirical results and findings
- `Discussion` - Analysis and interpretation

### Thesis-by-Papers (2)
- `Paper` - Individual research papers
- `Synthesis` - Integration across papers

### Argument (5)
- `Claim` - Primary thesis claim
- `Ground` - Foundational support/premises
- `Proof` - Formal or empirical evidence
- `Objection` - Counter-arguments and critiques
- `Reply` - Responses to objections

### Contribution (4)
- `Gap` - Research gap identified
- `Design` - Solution/artifact design
- `Evaluation` - Results and validation
- `Impact` - Significance and broader implications

### Monograph (5)
- `Context` - Historical and disciplinary context
- `Canon` - Relevant literature and prior work
- `Analysis` - Deep thematic analysis
- `Conclusion` - Synthesis and closing
- `Problem` - Problem formulation

### Design Science Research (3)
- `Problem` - Problem definition
- `Artifact` - Designed solution/tool
- `Theory` - Theoretical contributions

### Narrative (4)
- `Field` - Field/discipline framing
- `Voice` - Authorial voice and positioning
- `Pattern` - Pattern recognition and emergence
- `Insight` - Emergent insights

## Use Cases

### 1. **Thesis Planning and Organization**
Organize your research systematically from problem formulation through final synthesis.

```bash
# Initialize thesis
htf add "Problem Statement" Problem
htf add "Research Gap Analysis" Gap
htf add "Main Hypothesis" Claim

# Check coverage
htf profile   # Shows what's missing
htf check     # Validates structure

# Plan chapters
htf schedule --chapter-size 3000
```

### 2. **PhD Thesis Development**
Track progress across dissertation phases with built-in structure validation.

```bash
# Track status through PhD phases
htf add "Literature Review" Canon
htf add "Methodology Design" Method
htf add "Empirical Results" Result
htf add "Theoretical Analysis" Theory

# Validate completeness
htf check  # Ensures all aspects covered
```

### 3. **Multi-Paper Research Program**
Organize multiple papers into a coherent thesis narrative.

```bash
htf add "Paper 1: Problem Formulation" Paper
htf add "Paper 2: Solution Design" Paper
htf add "Paper 3: Empirical Evaluation" Paper
htf add "Synthesis and Integration" Synthesis

# Verify synthesis
htf profile  # Shows coverage across papers
```

### 4. **Systematic Literature Review**
Structure your review to cover all research dimensions.

```bash
htf add "Field Overview" Field
htf add "Canonical Works" Canon
htf add "Recent Trends" Pattern
htf add "Research Gaps" Gap
htf add "Future Directions" Impact
```

### 5. **Design Science Research (DSR)**
Ensure complete DSR narrative with problem-artifact-theory.

```bash
htf add "Problem Analysis" Problem
htf add "Artifact Design" Artifact
htf add "Theoretical Contribution" Theory

htf check  # Validates DSR structure
```

## Installation

Install from ggen marketplace:

```bash
# Discover the pack
ggen packs list | grep htf

# Get details
ggen packs show htf-thesis-framework

# Install with dry-run verification
ggen packs install htf-thesis-framework --version 0.1.0 --dry-run

# Install
ggen packs install htf-thesis-framework --version 0.1.0
```

Or build from source:

```bash
git clone https://github.com/seanchatmangpt/ggen.git
cd ggen/playground
cargo build --release
./target/release/htf --help
```

## Usage Examples

### Example 1: Quick Thesis Assessment

```bash
$ htf profile

=== HTF Coverage Report ===
Total Words: 15,200

Coverage by Family:
  Method          | ████████████      | 22.3%
  Intro           | ███████████       | 20.1%
  Result          | ███████████       | 19.8%
  Problem         | ██████████        | 18.5%
  Canon           | ████              | 5.2%
  Artifact        | ████              | 4.1%
  ...

Uncovered Families:
  - Claim
  - Voice
  - Pattern
  - Theory
  - Impact
  - Design
  - (16 more missing)
```

**Interpretation**: Strong in methodology and results, weak in theoretical contribution and design. Add 2-3 shards on theory/design.

### Example 2: Chapter Planning

```bash
$ htf schedule --chapter-size 3000

{
  "thesis_id": "uuid",
  "chapters": 5,
  "total_shards": 28,
  "chapters_detail": [
    {
      "number": 1,
      "title": "Chapter 1: Problem & Context",
      "shards": 6,
      "words": 3120,
      "families": ["Problem", "Gap", "Context", "Intro"]
    },
    {
      "number": 2,
      "title": "Chapter 2: Literature & Design",
      "shards": 5,
      "words": 3045,
      "families": ["Canon", "Design", "Artifact"]
    },
    ...
  ]
}
```

### Example 3: Validation and Repair

```bash
$ htf check

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
  "recommendations": [
    "Missing 8 families: Claim, Voice, Pattern, Theory, Impact, Design, Ground, Reply",
    "Dependencies violate Λ-ordering: gap-1 depends on method-1 (should be reversed)"
  ]
}
```

**Actions**:
1. Add missing families with `htf add`
2. Review and fix dependency ordering
3. Rerun `htf check` until valid

## Integration with Ggen Packs

HTF is fully integrated with the ggen marketplace and packs system:

### Discover and Install
```bash
ggen packs list                    # Find HTF
ggen packs show htf-thesis-framework  # View details
ggen packs install htf-thesis-framework  # Install
```

### Execute via Ggen
```bash
ggen packs execute --pack htf-thesis-framework --command "schedule"
ggen packs execute --pack htf-thesis-framework --command "profile"
ggen packs execute --pack htf-thesis-framework --command "check"
```

### Phase-Gated Installation
```bash
# Validate pack
ggen packs install htf --phase validate --dry-run

# Resolve dependencies
ggen packs install htf --phase resolve --dry-run

# Stage files
ggen packs install htf --phase stage --dry-run

# Full dry-run
ggen packs install htf --phase execute --dry-run

# Install for real
ggen packs install htf
```

## Performance

All operations optimized for interactive use:

| Command | Time | Typical Usage |
|---------|------|---------------|
| list | 45ms | Quick listing |
| schedule | 92ms | Chapter planning |
| profile | 78ms | Coverage analysis |
| check | 63ms | Validation |
| add | 38ms | New shard |
| CLI startup | 20ms | Overhead |

## Requirements

- **Rust**: 1.70+ (for building from source)
- **Platforms**: macOS, Linux, Windows
- **Dependencies**: All bundled (no external tools needed)
- **Storage**: ~50MB (binary + cache)

## API Reference

### Core Types

```rust
pub struct DeltaShard {
    pub id: String,
    pub name: String,
    pub family: ShardFamily,  // 26 canonical families
    pub content: String,
    pub status: ShardStatus,  // Draft → InProgress → Review → Final
    pub dependencies: Vec<String>,
}

pub struct ChapterPlan {
    pub chapters: Vec<Chapter>,
    pub thesis_id: String,
}

pub struct GammaCheckResult {
    pub is_valid: bool,
    pub invariants_passed: Vec<String>,
    pub invariants_failed: Vec<String>,
    pub recommendations: Vec<String>,
}
```

### Library Usage

```rust
use htf_cli::{scheduler, profiler, checker, models::*};

// Schedule chapters
let plan = scheduler::schedule_chapters(shards, 2000)?;

// Profile coverage
let profile = profiler::profile_thesis(shards)?;

// Check invariants
let result = checker::check_thesis(shards)?;
```

## Support & Documentation

- **README.md**: Feature overview and framework explanation
- **USAGE_EXAMPLE.md**: Practical step-by-step examples
- **INTEGRATION.md**: Marketplace and ggen pack integration
- **SUMMARY.md**: Complete implementation details
- **GitHub Issues**: Report bugs or request features
- **Email**: sean@chatmangpt.com

## FAQ

**Q: Can I use HTF with my existing thesis?**
A: Yes! Import your shards as JSON and HTF will analyze and plan them.

**Q: How do I export my thesis structure?**
A: Use `htf export --format json` or `--format yaml`

**Q: Can I collaborate with others?**
A: Yes, via ggen marketplace's collaboration features.

**Q: What if my research doesn't fit the 26 families?**
A: HTF is designed to be flexible. Map your research to closest families and extend as needed.

**Q: Is this only for academic research?**
A: HTF works for any large writing project with structured argumentation.

## License

MIT License - See LICENSE file for details

## Contributing

Contributions welcome! Please submit issues and pull requests to the ggen repository.

## Changelog

### v0.1.0 (Current)
- Initial release with Λ-scheduler, Π-profiler, Γ-checker
- 26 canonical research families
- 5 core invariants for validation
- Full ggen marketplace integration
- Comprehensive documentation

## Related Projects

- **ggen**: Code generation framework using RDF knowledge graphs
- **clap-noun-verb**: Structured command-line interface framework
- **oxigraph**: RDF triple store (future backend)

---

**Status**: Production Ready ✅
**Last Updated**: 2024-11-17
**Maintainer**: Sean Chatman
