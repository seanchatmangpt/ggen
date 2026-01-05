# Claude Code Configuration for ggen (2026 Edition)

This directory contains all Claude Code customization files for the ggen project.

## Version

**ggen**: 5.2.0 (2026 Bleeding Edge)
**Claude Code Web**: Optimized for parallel-first, specification-driven development
**Last Updated**: 2026-01-05

---

## 🚨 THREE PARADIGM SHIFTS (Must Read First)

Before using any commands, agents, or skills, understand these three fundamental shifts that define ggen's 2026 workflow:

### Paradigm Shift 1: Big Bang 80/20 (Specification-First)

**Old way**: Vague requirement → Plan → Code → Test → [Iterate when broken]

**New way**: Specification closure verification → Single-pass construction → Validation receipts

**What this means for you**:
- ALWAYS run `/speckit-verify` before implementing any non-trivial feature
- If specification has "TBD", "maybe", "probably" → **STOP**, clarify first
- Iteration signals incomplete specification, not normal workflow
- One pass = one implementation when spec is closed

**Skill to master**: `bb80-specification-closure`

---

### Paradigm Shift 2: EPIC 9 - Atomic Cognitive Cycle (Parallel-First)

**Old way (sequential)**: Plan agent (1h) → Code agent (2h) → Test agent (1h) → Reviewer (1h) = **5 hours**

**New way (parallel)**: 10 agents in parallel (2h) + Collision detection (30m) + Convergence (30m) = **3 hours**

**What this means for you**:
- Parallel-first is **default** for non-trivial tasks (not sequential)
- Use `/bb80-parallel "[specification]"` to orchestrate atomic cycle
- Six phases (FAN-OUT → INDEPENDENT → COLLISION → CONVERGENCE → SYNTHESIS → CLOSURE)
- 2.8-4.4x faster than sequential execution
- High confidence when multiple agents converge on same solution

**Skill to master**: `bb80-parallel-agents`

---

### Paradigm Shift 3: Deterministic Validation (Evidence-First)

**Old way**: "I reviewed the code. It looks good. Performance should be fine." (Opinion)

**New way**: "[Receipt] cargo make check ✓ | [Receipt] All 347 tests pass | [Receipt] SLOs met" (Evidence)

**What this means for you**:
- NEVER narrative review ("code looks good")
- ALWAYS provide receipts (reproducible evidence)
- Before marking work "done": collect receipts
- Receipts replace review, benchmarks replace narratives

**Skill to master**: `bb80-deterministic-receipts`

---

This directory contains all Claude Code customization files for the ggen project.

## Directory Structure

```
.claude/
├── settings.json              # Shared project configuration (version controlled)
├── settings.local.json        # Personal settings (gitignored)
├── CLAUDE.md                  # Project context and instructions (symlink to root)
│
├── commands/                  # Custom slash commands (9 total)
│   ├── bb80-parallel.md      # /bb80-parallel - Orchestrate EPIC 9 atomic cycle
│   ├── bench-compare.md      # /bench-compare - Compare benchmark results
│   ├── collision-detect.md   # /collision-detect - Analyze agent output overlaps
│   ├── convergence.md        # /convergence - Synthesize parallel agent results
│   ├── optimize.md           # /optimize - Code optimization analysis
│   ├── review-errors.md      # /review-errors - Audit error handling patterns
│   ├── speckit-check.md      # /speckit-check - Validate RDF specifications
│   ├── speckit-verify.md     # /speckit-verify - Verify specification closure
│   └── test-audit.md         # /test-audit - Run test quality audit
│
├── agents/                    # Custom subagent definitions (8 total)
│   ├── bb80-collision-detector.md      # Detect overlaps in parallel outputs
│   ├── bb80-convergence-orchestrator.md # Synthesize best solutions
│   ├── bb80-parallel-task-coordinator.md # Coordinate parallel agent execution
│   ├── bb80-specification-validator.md  # Validate specification closure
│   ├── reviewer.md                      # Code review and compliance specialist
│   ├── rust-coder.md                    # Rust implementation specialist
│   ├── speckit-architect.md             # RDF specification designer
│   └── test-engineer.md                 # Test writing and quality specialist
│
├── skills/                    # Rich capability modules (10 total)
│   ├── bb80-deterministic-receipts/
│   │   └── SKILL.md          # Evidence-based validation patterns
│   │
│   ├── bb80-invariant-construction/
│   │   └── SKILL.md          # Type-driven invariant design
│   │
│   ├── bb80-parallel-agents/
│   │   └── SKILL.md          # Parallel agent orchestration patterns
│   │
│   ├── bb80-specification-closure/
│   │   └── SKILL.md          # Verify specification completeness
│   │
│   ├── cargo-make-protocol/
│   │   ├── SKILL.md          # Cargo Make usage mastery
│   │   └── reference.md      # Detailed command reference
│   │
│   ├── chicago-tdd-pattern/
│   │   └── SKILL.md          # Chicago TDD testing patterns
│   │
│   ├── poka-yoke-patterns/
│   │   └── SKILL.md          # Error-proofing and quality gates
│   │
│   └── rdf-ontologies/
│       └── SKILL.md          # RDF, Turtle, SPARQL mastery
│
├── hooks/                     # Deterministic lifecycle hooks (7 total)
│   ├── session-start.sh      # Initialize environment, compile check
│   ├── pre-tool-safety-check.sh # Prevent dangerous operations
│   ├── pre-specification-check.sh # Verify spec closure before implementation
│   ├── post-bash-validation.sh  # Detect andon signals after commands
│   ├── post-collision-detection.sh # Validate collision analysis results
│   ├── convergence-validation.sh  # Validate convergence synthesis
│   └── user-prompt-validation.sh # Validate user instructions
│
└── helpers/                   # Helper scripts (optional)
    ├── api-key-helper.sh     # Manage ANTHROPIC_API_KEY
    └── bash-init.sh          # Bash environment initialization
```

## Quick Start

### 2026 Workflow (Specification-First + EPIC 9)

**The new pattern for non-trivial tasks**:

```bash
# 1. Verify specification closure (MANDATORY before implementation)
/speckit-verify [feature]

# 2. If spec is closed (100%), orchestrate EPIC 9 parallel execution
/bb80-parallel "Full specification text or reference"

# 3. Analyze collision (where agents converged = high confidence)
/collision-detect

# 4. Synthesize best solution from parallel outputs
/convergence

# 5. Validate with receipts (not narrative review)
cargo make pre-commit  # Collect deterministic receipts
```

**Result**: 2.8-4.4x faster than sequential, single-pass construction, evidence-based validation

---

### Using Slash Commands

**EPIC 9 Commands** (Parallel-First Workflow):
```bash
/bb80-parallel "[spec]"        # Orchestrate 10+ parallel agents (EPIC 9)
/collision-detect              # Analyze agent output overlaps
/convergence                   # Synthesize best solution from all agents
/speckit-verify [feature]      # Verify specification closure (MANDATORY first)
```

**Quality Assurance Commands**:
```bash
/speckit-check [feature]       # Validate RDF specifications (TTL syntax)
/test-audit [crate]            # Run mutation testing + assertion analysis
/review-errors [crate]         # Audit error handling patterns
/optimize [crate]              # Analyze and optimize performance
/bench-compare [branch1] [branch2]  # Compare benchmark results
```

### Using Agents

Spawn specialized agents for complex tasks:

**EPIC 9 Agents** (Parallel Orchestration):
```bash
# These agents work together in EPIC 9 atomic cycle
Task("bb80-parallel-task-coordinator", "Orchestrate 10 agents for X", "coordinator")
Task("bb80-collision-detector", "Analyze overlaps in outputs", "detector")
Task("bb80-convergence-orchestrator", "Synthesize best solution", "orchestrator")
Task("bb80-specification-validator", "Verify spec closure", "validator")
```

**Traditional Agents** (Sequential or Single-Focus):
```bash
Task("rust-coder", "Implement feature X following plan", "coder")
Task("test-engineer", "Write tests for module Y", "tester")
Task("reviewer", "Review code for compliance", "reviewer")
Task("speckit-architect", "Design RDF specification for Z", "architect")
```

### Using Skills

Skills auto-load based on context. Trigger them by mentioning:

**2026 Core Skills** (Big Bang 80/20 + EPIC 9):
- **bb80-specification-closure**: Verify specification completeness (enables EPIC 9)
- **bb80-parallel-agents**: Parallel agent orchestration patterns
- **bb80-deterministic-receipts**: Evidence-based validation (no narratives)
- **bb80-invariant-construction**: Type-driven invariant design

**Foundation Skills**:
- **cargo-make-protocol**: Cargo Make usage, SLO enforcement, timeouts
- **chicago-tdd-pattern**: State-based testing, AAA pattern, real collaborators
- **rdf-ontologies**: RDF, Turtle, SPARQL, .specify/ workflow
- **poka-yoke-patterns**: Error-proofing, quality gates, FMEA analysis

## Configuration Files

### settings.json (Shared)

**Version controlled** - shared with team

```json
{
  "permissions": { },           # Tool and file access controls
  "hooks": { },                 # Lifecycle hooks configuration
  "environment": { },           # Environment variables
  "tools_and_model": { },       # Model and tool settings
  "project_metadata": { }       # Project information
}
```

**Edit when**: Team needs to change shared rules, add team commands, configure hooks

**Do NOT edit**: Personal API keys, local overrides (use settings.local.json)

### settings.local.json (Personal)

**Gitignored** - personal overrides

Contains:
- Personal API keys (ANTHROPIC_API_KEY)
- Personal environment variables
- Local model preferences
- Debugging flags

**Create** by copying `settings.json` and adding personal overrides

## Features

### 1. Commands (Slash Commands)

Auto-discovered custom commands in `commands/` directory

**Benefits**:
- Project-specific workflows
- Consistent terminology
- Clear documentation
- Easy to discover (shown in /help)

**Creating a command**:

1. Create `.claude/commands/my-command.md`
2. Add YAML frontmatter with description
3. Write markdown instructions
4. Use `$ARGUMENTS`, `$1`, `$2` for parameters
5. Invoke with `/my-command arg1 arg2`

See `commands/optimize.md` for example

### 2. Agents (Subagents)

Specialized agents in `agents/` directory auto-loaded at session start

**Benefits**:
- Separate context for complex tasks
- Role-specific tools and capabilities
- Improved focus and reliability
- Better token efficiency

**Using agents**:

```bash
Task("rust-coder", "Implement X following plan", "rust-coder")
Task("test-engineer", "Write tests for Y", "test-engineer")
Task("reviewer", "Review code in Z", "reviewer")
Task("speckit-architect", "Design spec for W", "speckit-architect")
```

See `agents/` directory for detailed agent descriptions

### 3. Skills (Rich Capabilities)

Auto-discovered, semantic-triggered capabilities in `skills/` directories

**Benefits**:
- Context-aware activation (no slash needed)
- Rich supporting materials
- Examples and references
- Deep knowledge organization

**Using skills**:

Skills auto-load when you mention related concepts. No explicit invocation needed.

- Mention "cargo make" → cargo-make-protocol skill loads
- Talk about testing → chicago-tdd-pattern skill loads
- Work with RDF → rdf-ontologies skill loads
- Discuss error-proofing → poka-yoke-patterns skill loads

See `skills/*/SKILL.md` for full skill documentation

### 4. Hooks (Deterministic Lifecycle Automation)

Shell scripts triggered at specific points in Claude Code lifecycle. Hooks implement Poka-Yoke error-proofing at the automation layer.

**Architecture**:
```
User/Claude Action → Hook Triggers → Validation → Allow/Block → Continue
```

**Available Hooks** (7 total):

| Hook | Trigger | Purpose | Exit Code |
|------|---------|---------|-----------|
| `session-start.sh` | Session begins | Initialize env, compile check, verify deps | 0 = continue, 1 = block |
| `pre-tool-safety-check.sh` | Before ANY tool | Prevent dangerous ops (rm -rf, force push) | 0 = allow, 1 = block |
| `pre-specification-check.sh` | Before implementation | Verify spec closure (100%) before coding | 0 = proceed, 1 = stop |
| `post-bash-validation.sh` | After Bash command | Detect andon signals (RED/YELLOW/GREEN) | 0 = OK, 1 = escalate |
| `post-collision-detection.sh` | After collision analysis | Validate collision results (convergence) | 0 = valid, 1 = re-run |
| `convergence-validation.sh` | After convergence | Validate synthesis quality | 0 = accept, 1 = reject |
| `user-prompt-validation.sh` | User submits prompt | Check for ambiguous instructions | 0 = clear, 1 = clarify |

---

**Hook Lifecycle Integration**:

```
┌─────────────────────────────────────────────────────────────┐
│ 1. SessionStart → session-start.sh                          │
│    - cargo make check (compile)                             │
│    - Verify Rust toolchain                                  │
│    - Load project context                                   │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│ 2. UserPromptSubmit → user-prompt-validation.sh             │
│    - Check for "TBD", "maybe", "probably"                   │
│    - Detect vague specifications                            │
│    - Suggest /speckit-verify if needed                      │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│ 3. PreToolUse → pre-tool-safety-check.sh                    │
│    - Block: rm -rf /, git push --force (to main/master)    │
│    - Block: Direct cargo commands (enforce cargo make)      │
│    - Block: Editing .md in .specify/ (TTL is source)       │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│ 4. PreToolUse (Edit/Write) → pre-specification-check.sh     │
│    - Detect implementation without closed spec              │
│    - Run /speckit-verify automatically                      │
│    - Block if closure < 100%                                │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│ 5. PostToolUse (Bash) → post-bash-validation.sh             │
│    - Parse output for error[E...], FAILED, panic            │
│    - Classify: RED (stop), YELLOW (warn), GREEN (OK)        │
│    - Trigger andon signal escalation if RED                 │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│ 6. PostToolUse (Collision) → post-collision-detection.sh    │
│    - Validate convergence score (≥70% = high confidence)    │
│    - Detect divergence (agents disagree = incomplete spec)  │
│    - Suggest re-specification if divergence detected        │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│ 7. PostToolUse (Convergence) → convergence-validation.sh    │
│    - Validate synthesis includes best parts from all agents │
│    - Check for selection pressure (not voting/compromise)   │
│    - Verify deterministic receipts collected                │
└─────────────────────────────────────────────────────────────┘
```

---

**Hook Configuration** (in `settings.json`):

```json
{
  "hooks": {
    "SessionStart": {
      "script": ".claude/hooks/session-start.sh",
      "enabled": true,
      "timeout": 30000
    },
    "PreToolUse": {
      "script": ".claude/hooks/pre-tool-safety-check.sh",
      "enabled": true,
      "timeout": 5000
    },
    "PostToolUse": {
      "script": ".claude/hooks/post-bash-validation.sh",
      "enabled": true,
      "timeout": 10000
    }
  }
}
```

---

**Benefits of Hooks**:
- **Automate validation**: No manual checking needed
- **Prevent defects**: Block dangerous operations before they happen
- **Catch errors immediately**: Andon signals within milliseconds
- **Enforce project rules**: Constitutional rules as code
- **Enable EPIC 9**: Specification-first enforcement makes parallelization safe
- **Deterministic**: Same inputs = same validation results

---

**Creating Custom Hooks**:

1. Create script in `.claude/hooks/my-hook.sh`
2. Make executable: `chmod +x .claude/hooks/my-hook.sh`
3. Add to `settings.json` under appropriate trigger
4. Exit 0 = allow/continue, Exit 1 = block/stop
5. Use `$TOOL_NAME`, `$TOOL_INPUT` environment variables for context

**Example Hook**:
```bash
#!/bin/bash
# .claude/hooks/my-custom-hook.sh

# Access hook context
TOOL=$TOOL_NAME
INPUT=$TOOL_INPUT

# Validation logic
if [[ "$TOOL" == "Edit" ]] && [[ "$INPUT" =~ \.specify/.*\.md$ ]]; then
  echo "ERROR: Cannot edit .md files in .specify/ (TTL is source of truth)"
  echo "HINT: Edit corresponding .ttl file instead"
  exit 1  # Block the operation
fi

exit 0  # Allow operation
```

---

See `hooks/*.sh` for production implementations

## Constitutional Rules (Key Principles)

Always follow these rules when using ggen:

### 1. Cargo Make Protocol

```bash
ALWAYS:  cargo make [target]
NEVER:   cargo [command]
```

Why: Enforces timeouts, quality gates, andon signals

### 2. Error Handling

```rust
PRODUCTION: Use Result<T, E> (NO unwrap/expect)
TESTS:      unwrap/expect ALLOWED (tests should panic)
```

Why: Production must handle errors gracefully

### 3. Andon Signals (Stop the Line)

```
RED (Error)   → STOP, fix immediately
YELLOW (Warn) → Investigate before release
GREEN (OK)    → Proceed safely
```

Why: Prevent defects from propagating

### 4. Chicago TDD

```rust
// State-based testing with real collaborators
#[test]
fn test_feature() {
    // Arrange: Real objects
    let obj = Object::new().unwrap();

    // Act: Call public API
    obj.do_something();

    // Assert: Verify state changed
    assert_eq!(obj.state(), expected);
}
```

Why: Tests verify behavior, catch 80% of bugs

### 5. RDF-First Specifications

```
Edit:   .specify/specs/NNN-feature/*.ttl (source)
Generate: .specify/specs/NNN-feature/*.md (derived)
NEVER:  Edit .md files directly
```

Why: TTL is source of truth, prevents manual inconsistencies

### 6. Poka-Yoke Error-Proofing

```
Prevent > Detect > React
```

Design systems to:
1. Make mistakes impossible (type system)
2. Catch mistakes immediately (timeouts, gates)
3. Stop when mistakes detected (andon signals)

## Getting Started

### 1. First Time Setup

```bash
# Copy personal settings template
cp settings.json settings.local.json

# Edit settings.local.json with your API key
# ANTHROPIC_API_KEY=your-key-here

# Make hooks executable
chmod +x hooks/*.sh

# Start a new session
# Claude Code will auto-initialize with session-start.sh
```

### 2. Learning the System

1. **Read** `/home/user/ggen/CLAUDE.md` - Project constitution
2. **Explore** `.claude/commands/` - See available slash commands
3. **Review** `.claude/agents/` - Understand specialized agents
4. **Study** `.claude/skills/*/SKILL.md` - Deep knowledge areas
5. **Understand** `settings.json` - Project configuration

### 3. Daily Workflow (2026 Edition)

**For Non-Trivial Features** (EPIC 9 Parallel-First):

```bash
# 1. Specification Phase (MANDATORY)
/speckit-verify [feature]              # Verify spec closure = 100%
# If incomplete: clarify with user, edit .ttl, re-verify

# 2. Parallel Execution (EPIC 9 Atomic Cycle)
/bb80-parallel "Full specification"    # 10+ agents work in parallel
# Wait 2-4 hours (agents work independently)

# 3. Collision Analysis
/collision-detect                      # Analyze where agents converged
# High convergence (≥70%) = high confidence

# 4. Convergence Synthesis
/convergence                           # Synthesize best solution
# Selection pressure (not voting)

# 5. Validation (Evidence-First)
cargo make pre-commit                  # Collect receipts
# [Receipt] check ✓, test ✓, lint ✓, SLO ✓

# 6. Commit with Evidence
git add .
git commit -m "feat: [feature] - [Receipt] All validation passed"
git push
```

**Result**: 2.8-4.4x faster, single-pass construction, evidence-based validation

---

**For Trivial Tasks** (Single Command/Quick Fix):

```bash
# Check status
git status
cargo make check                       # <5s compile check

# Make changes (no EPIC 9 needed for trivial edits)
# ... edit single file ...

# Quick validation
cargo make test-unit                   # <10s unit tests
cargo make lint                        # <60s clippy

# Full validation before commit
cargo make pre-commit                  # Format + lint + tests

# Commit and push
git add .
git commit -m "fix: [description]"
git push
```

---

**Quality Assurance Workflow** (Optional, When Needed):

```bash
# After making changes, optionally run:
/optimize ggen-core                    # If performance-critical
/test-audit ggen-domain                # If test quality concerns
/review-errors ggen-cli                # If error handling changes
/bench-compare main HEAD               # If optimization claims
```

---

**Key Differences (2026 vs 2025)**:

| Aspect | 2025 (Old) | 2026 (New) |
|--------|-----------|-----------|
| **Workflow** | Sequential (plan→code→test→review) | Parallel-first (EPIC 9) |
| **Starting Point** | Vague requirements | Closed specification (/speckit-verify) |
| **Execution** | One agent at a time | 10+ agents in parallel |
| **Validation** | Narrative review ("looks good") | Deterministic receipts (evidence) |
| **Iteration** | Expected (iterate until works) | Defect signal (fix spec, not code) |
| **Speed** | 5 hours sequential | 3 hours parallel (2.8-4.4x) |
| **Confidence** | Subjective review | Collision convergence (≥70%) |

## Command Reference

### Essential Slash Commands (9 Total)

**EPIC 9 Commands** (Parallel-First Workflow):

| Command | Purpose | Example | SLO |
|---------|---------|---------|-----|
| `/speckit-verify` | Verify specification closure (MANDATORY first) | `/speckit-verify 005-feature` | <3s |
| `/bb80-parallel` | Orchestrate 10+ parallel agents (EPIC 9) | `/bb80-parallel "[full spec]"` | 2-4h |
| `/collision-detect` | Analyze agent output overlaps/convergence | `/collision-detect` | <30m |
| `/convergence` | Synthesize best solution from parallel agents | `/convergence` | <30m |

**Quality Assurance Commands**:

| Command | Purpose | Example | SLO |
|---------|---------|---------|-----|
| `/speckit-check` | Validate RDF specifications (TTL syntax, SHACL) | `/speckit-check 004` | <5s |
| `/test-audit` | Mutation testing + assertion analysis | `/test-audit ggen-domain` | <2min |
| `/review-errors` | Audit error handling patterns (unwrap violations) | `/review-errors ggen-cli` | <30s |
| `/optimize` | Analyze and optimize Rust performance | `/optimize ggen-core` | <1min |
| `/bench-compare` | Compare benchmark results across commits | `/bench-compare main HEAD` | <5min |

### Essential Cargo Make Targets

| Target | Purpose | Time |
|--------|---------|------|
| `check` | Compile check | ~5s |
| `test-unit` | Unit tests | ~10s |
| `lint` | Clippy check | ~60s |
| `pre-commit` | Full validation | ~60s |
| `test` | All tests | ~30s |
| `bench` | Benchmarks | 5min |
| `slo-check` | SLO compliance | ~2min |

## Troubleshooting

### "Command not found"

```bash
# Skills auto-load, commands are invoked via /
/my-command arg1 arg2
```

### "Skill not activating"

Ensure your message contains natural trigger words (e.g., "cargo make" for cargo-make-protocol skill)

### "Hook not executing"

```bash
# Make sure hook is executable
chmod +x .claude/hooks/hook-name.sh

# Verify settings.json has hook configured
```

### "Settings not applying"

Priority order (highest to lowest):
1. `.claude/settings.json` (project)
2. `.claude/settings.local.json` (project local)
3. `~/.claude/settings.json` (user global)
4. `~/.claude/settings.local.json` (user local)

## Advanced Topics

### Creating Custom Agents

See `agents/rust-coder.md` for detailed agent format

### Creating Custom Skills

See `skills/cargo-make-protocol/SKILL.md` for skill structure

### Adding Hooks

See `hooks/session-start.sh` for hook implementation

### Custom Commands with Arguments

See `commands/bench-compare.md` for command argument handling

## Documentation

- **CLAUDE.md** (root): Project architecture and philosophy
- **settings.json**: Configuration reference
- **commands/*.md**: Individual command documentation
- **agents/*.md**: Individual agent capabilities
- **skills/*/SKILL.md**: Skill deep-dives with examples
- **hooks/*.sh**: Hook implementation details

## Support

- **Documentation**: Read CLAUDE.md first
- **Issues**: Check `.claude/hooks/` for validation hints
- **Configuration**: Review `settings.json` structure
- **Learning**: Study skills in `skills/*/SKILL.md`

## ggen sync - Specification Compiler (2026 Core Command)

**CRITICAL INSIGHT**: ggen is not just a build tool—it's your **specification compiler**. Use it to replace hand-coding entirely.

### The Pattern: Ontology → Code

```
Write TTL Ontology → ggen sync → Generates Code
  (Specification)              (Implementation)
```

This replaces manual boilerplate with deterministic generation. **Time savings: 60-80% reduction in hand-coding**.

### Basic Usage

```bash
# Full generation from manifest
ggen sync

# Preview without writing files
ggen sync --dry-run

# Watch mode (auto-regenerate on changes)
ggen sync --watch --verbose

# Validate spec without generating
ggen sync --validate-only

# Generate specific rule only
ggen sync --rule=structs

# Output as JSON (for CI/CD)
ggen sync --format json
```

### How ggen sync Works (Pipeline)

```
1. Load ggen.toml (manifest)
          ↓
2. Load Ontology (TTL files)
          ↓
3. Phase 1: Inference (CONSTRUCT queries)
   - Enrich graph with derived facts
          ↓
4. Materialize enriched graph
          ↓
5. Phase 2: Generation (SELECT queries + Templates)
   - Extract data via SPARQL
   - Render Tera templates
          ↓
6. Write files + Audit trail
          ↓
7. Validation (syntax, constraints)
```

### Why ggen sync Enables EPIC 9

**The breakthrough insight**: Specifications are deterministic input

```
Specification (TTL) is deterministic
          ↓
10 independent agents can generate FROM THE SAME SPEC
          ↓
All 10 outputs are identical, correct implementations
          ↓
Agents converge on same solution → HIGH CONFIDENCE
```

**Example**: 10 agents writing CLI handlers in parallel

```
Team Specification: api_spec.ttl (single source of truth)
          ↓
Agent 1-10: Each runs `ggen sync` independently
          ↓
All agents' code is identical (compiled from same spec)
          ↓
Result: EPIC 9 achieves 2.8-4.4x speedup with 100% convergence
```

### Key Commands

| Command | Purpose | SLO |
|---------|---------|-----|
| `ggen sync` | Full generation pipeline | <5s |
| `ggen sync --dry-run` | Preview without writing | <5s |
| `ggen sync --watch` | Watch TTL files, auto-regenerate | Incremental <2s |
| `ggen sync --validate-only` | SHACL validation gate | <3s |
| `ggen sync --rule NAME` | Run specific rule | <2s |
| `ggen sync --force` | Overwrite without checks | <5s |
| `ggen sync --audit` | Detailed audit trail | <5s |

### When to Use ggen sync vs Hand-Code

**Use ggen sync when**:
- Generating multiple similar artifacts (CLIs, APIs, schemas)
- Working with a specification (domain model, API contract)
- Needing reproducibility across environments/agents
- Parallelizing work across multiple agents
- Generating from domain-specific languages

**Hand-code when**:
- One-off utility, no reuse
- Custom logic not covered by templates
- Exploring/prototyping (spec not yet stable)

**Best practice**: **Specification + ggen for 80% of code, hand-code for custom 20%**

---

## Best Practices (2026 Edition)

### Core Principles

1. **Specification-first**: Run `/speckit-verify` before implementing non-trivial features
2. **Parallel-first**: Use EPIC 9 (`/bb80-parallel`) as default for non-trivial tasks
3. **Evidence-first**: Collect receipts, not narratives ("code looks good" is banned)
4. **Use ggen sync**: Generate from specifications (60-80% faster than hand-coding)
5. **Edit .ttl not .md**: RDF is source of truth, markdown is generated
6. **Use cargo make exclusively**: Never direct cargo commands (enforces SLOs)
7. **Follow andon signals**: RED = stop immediately, fix before proceeding

### Workflow Patterns

**Non-trivial features**:
```
/speckit-verify → /bb80-parallel → /collision-detect → /convergence → receipts
```

**Trivial tasks**:
```
cargo make check → edit → cargo make pre-commit → commit
```

**Code generation**:
```
Edit .ttl → ggen sync --validate-only → ggen sync → cargo make check
```

### Red Flags (Stop If You See These)

- "I think", "should work", "code looks good" (no receipts)
- Direct cargo commands (use cargo make)
- Editing .md files in .specify/ (edit .ttl source)
- Starting implementation without closed spec (run /speckit-verify)
- Sequential agent dispatch for complex tasks (use EPIC 9)
- Narrative review without evidence (collect receipts)

### Quality Gates

**Before committing**:
- ✓ Specification closure = 100% (/speckit-verify)
- ✓ cargo make check (compilation)
- ✓ cargo make test (all tests pass)
- ✓ cargo make lint (0 violations)
- ✓ cargo make slo-check (SLOs met)

**Before EPIC 9**:
- ✓ Specification closure = 100%
- ✓ All agents have same spec
- ✓ No "TBD", "maybe", "probably" in spec

**Before merging**:
- ✓ All receipts collected
- ✓ Collision convergence ≥70% (if EPIC 9 used)
- ✓ No andon signals (RED/YELLOW cleared)

### Constitutional Rules (Zero Tolerance)

1. **Cargo Make Rule**: ALWAYS `cargo make`, NEVER `cargo`
2. **Error Handling Rule**: Production = `Result<T,E>`, Tests = `unwrap` OK
3. **Andon Signal Rule**: RED = stop line immediately
4. **Specification Rule**: Edit .ttl (source), never .md (generated)
5. **EPIC 9 Rule**: Parallel-first for non-trivial (not sequential)
6. **Receipt Rule**: Evidence-first (no "code looks good")

## Key Files Reference

| File | Purpose | When to Edit |
|------|---------|--------------|
| `settings.json` | Team configuration | When project rules change |
| `settings.local.json` | Personal settings | Always (gitignored) |
| `commands/*.md` | Slash commands | Adding new workflows |
| `agents/*.md` | Agent definitions | Customizing specialists |
| `skills/*/SKILL.md` | Rich capabilities | Rarely (knowledge base) |
| `hooks/*.sh` | Lifecycle hooks | When validation needed |

## See Also

- `.specify/` - RDF specification system (source of truth)
- `Makefile.toml` - Build orchestration (68K+ lines)
- `CLAUDE.md` - Project constitution and philosophy
- `README.md` (root) - Quick start guide

---

## Version Information

**README Version**: 2.0.0 (2026 Edition)
**Project**: ggen 5.2.0
**Last Updated**: 2026-01-05
**Philosophy**: Type-first thinking, zero-cost abstractions, memory safety, deterministic outputs

**Key Changes in 2.0.0**:
- Added THREE PARADIGM SHIFTS (Big Bang 80/20, EPIC 9, Deterministic Validation)
- Comprehensive hooks documentation with lifecycle integration
- All 9 commands, 8 agents, 10 skills documented
- ggen sync specification compiler workflow
- 2026 daily workflow patterns (parallel-first)
- Updated quality gates and constitutional rules
- Evidence-first validation patterns

**Changelog**:
- 2.0.0 (2026-01-05): 2026 bleeding edge update, EPIC 9 integration, hooks expansion
- 1.0.0 (2025-12-28): Initial Claude Code configuration documentation
