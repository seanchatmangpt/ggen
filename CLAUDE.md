# ggen - Rust Project Configuration (Bleeding-Edge 2026 Edition)

## 🎯 Core Identity

**ggen**: Language-agnostic, deterministic code generation CLI. Ontologies + RDF → reproducible code projections.

**Tech Stack**: Rust (workspace), SPARC + Chicago TDD + DfLSS
**Philosophy**: Type-first thinking, zero-cost abstractions, memory safety, deterministic outputs

---

## 🚨 THREE PARADIGM SHIFTS (The Bleeding Edge - 2026)

This section defines the most impactful changes to ggen workflow. Mastery of these three shifts unlocks 2.8-4.4x speedup and dramatically better outcomes.

### Paradigm Shift 1: Big Bang 80/20 (Specification-First)

**Old way**: Vague requirement → Plan → Code → Test → [Iterate when broken]

**New way**: Specification closure verification → Single-pass construction → Validation receipts

**The principle**: Iteration is a defect signal of incomplete specification, not a normal workflow.

**What changes**:

1. **Specification closure is MANDATORY before implementation**
   - Before coding: run `/speckit-verify`
   - If incomplete: **STOP**, clarify with user, update .ttl
   - Only proceed when closure score = 100%

2. **Iteration means your spec was incomplete**
   - This is NOT normal
   - Fix the spec, don't iterate the code
   - One pass = one implementation

3. **Monoidal composition**: Systems compose without rework
   - Types enforce invariants
   - Code generation is "compilation from specification"
   - No adaptation layers needed

**When you use it**:
- Every non-trivial task starts with `/speckit-verify`
- If spec says "TBD", "maybe", "probably": **NOT closed**
- Incomplete spec → use `bb80-specification-closure` skill

**Benefits**:
- Fewer iterations (single-pass when spec is closed)
- Clearer success criteria upfront
- Fewer surprises during review

**Skill to master**: `bb80-specification-closure` (read it!)

---

### Paradigm Shift 2: EPIC 9 - Atomic Cognitive Cycle (Parallel-First)

**Old way (sequential)**: Plan agent (1h) → Code agent (2h) → Test agent (1h) → Reviewer (1h) = **5 hours**

**New way (parallel)**: 10 agents in parallel (2h) + Collision detection (30m) + Convergence (30m) = **3 hours**

**The principle**: Parallel-first is default for non-trivial tasks. Specification closure enables it.

**Six mandatory phases (non-skippable, non-reorderable)**:

```
1. FAN-OUT (Spawn 10+ independent agents)
         ↓
2. INDEPENDENT CONSTRUCTION (All work in parallel, NO coordination)
         ↓
3. COLLISION DETECTION (Analyze overlaps: structural & semantic)
         ↓
4. CONVERGENCE (Apply selection pressure, synthesize best solution)
         ↓
5. REFACTORING & SYNTHESIS (Merge, discard, rewrite as needed)
         ↓
6. CLOSURE (Validate all phases complete or no output)
```

**What makes it work**:

- **Parallelism**: 10 agents simultaneously exploring solution space (2.8-4.4x faster)
- **Diversity**: Multiple perspectives prevent blind spots
- **Collision detection**: When agents converge, you know you're right
- **Selection pressure** (not voting): Best solution wins, not compromise

**Trivial tasks skip EPIC 9**:
- Reading one file
- Running one script
- Displaying help
- (Most other tasks: use EPIC 9)

**When to use**:
```bash
# For any non-trivial task:
/speckit-verify [feature]        # Verify closure first (MANDATORY)
/bb80-parallel "[specification]" # Orchestrate atomic cycle
```

**What happens**:
1. 10 agents spawn with full spec
2. Each works independently (no "waiting for agent X")
3. All produce complete artifacts
4. Collision detection reveals where they converged
5. Convergence synthesizes best parts from all agents
6. You get polished, multi-perspective solution

**Benefits**:
- 2.8-4.4x faster than sequential
- High confidence (collision = multiple agents agreed)
- Better coverage (agents hit different parts)
- No iteration needed if spec is closed

**Skills to master**:
- `bb80-parallel-agents` (when/why to use)
- `bb80-specification-closure` (prerequisite)

**Commands to use**:
- `/speckit-verify` (verify closure)
- `/bb80-parallel` (orchestrate cycle)
- `/collision-detect` (analyze overlaps)
- `/convergence` (synthesize results)

---

### Paradigm Shift 3: Deterministic Validation (Evidence-First)

**Old way**: "I reviewed the code. It looks good. Performance should be fine." (Opinion)

**New way**: "[Receipt] cargo make check ✓ | [Receipt] All 347 tests pass | [Receipt] SLOs met" (Evidence)

**The principle**: Receipts replace review, benchmarks replace narratives, guards replace trust.

**What changes**:

1. **Never narrative review**
   ```
   ❌ "Code looks good"
   ✅ "[Receipt] cargo make lint: ✓ (0 violations)"

   ❌ "Tests should pass"
   ✅ "[Receipt] cargo make test: ✓ (347/347 pass)"

   ❌ "Performance is fine"
   ✅ "[Receipt] cargo make slo-check: ✓ (check 4.2s, test 28s, lint 58s)"
   ```

2. **Evidence is reproducible**
   - Anyone can run `cargo make pre-commit`
   - Same output every time
   - Auditable (timestamps, versions)

3. **Receipts are the new "done"**
   ```
   [Receipt] cargo make check: ✓
   [Receipt] cargo make test: ✓ (347/347)
   [Receipt] cargo make lint: ✓ (0 violations)
   [Receipt] Specification coverage: 95%
   [Receipt] SLO compliance: ✓
   Status: READY FOR DEPLOYMENT
   ```

**When you use it**:
- Before marking work "done": collect receipts
- In communication: reference receipts, not opinions
- Code review: "Receipt-based only, no narrative"

**Receipt categories**:
- Compilation: `cargo make check` pass
- Tests: All tests pass, coverage ≥80%
- Linting: 0 violations (clippy, format, security audit)
- Performance: SLO check pass
- Integration: E2E tests pass
- Security: 0 vulnerabilities

**Benefits**:
- Objective (no opinion involved)
- Reproducible (anyone can verify)
- Auditable (timestamps, full history)
- Measurable (0 violations, not "probably fine")

**Skill to master**: `bb80-deterministic-receipts` (what counts as evidence)

**How to produce receipts**:
```bash
cargo make pre-commit  # Produces timestamped receipts
                        # All validation phases complete
```

---

## 🚨 CRITICAL CONSTITUTIONAL RULES (Supporting Detail)

These rules support the three paradigm shifts. They are still mandatory but operate at a lower level.

### 1. CONCURRENT EXECUTION RULE

**Golden Rule**: "1 MESSAGE = ALL RELATED OPERATIONS"

```javascript
// ✅ CORRECT: Single message with ALL operations
[Single Message]:
  Task("Agent 1", "Full instructions...", "coder")
  Task("Agent 2", "Full instructions...", "tester")
  Task("Agent 3", "Full instructions...", "reviewer")

  TodoWrite { todos: [10+ todos in ONE call] }

  Write "file1.rs"
  Write "file2.rs"
  Write "file3.rs"

  Bash "cmd1 && cmd2 && cmd3"
```

**Why**: 2.8-4.4x speed improvement, prevents coordination failures

---

### 2. CARGO MAKE RULE

**NEVER USE DIRECT CARGO COMMANDS**

```bash
# ❌ WRONG
cargo check
cargo test
cargo clippy

# ✅ CORRECT
cargo make check     # <5s timeout
cargo make test      # All tests with timeouts
cargo make lint      # Clippy with timeouts
```

**Why**: Prevents hanging, enforces SLOs, integrated with hooks

---

### 3. ANDON SIGNAL RULE

**Stop the Line When Signals Appear**

| Signal | Trigger | Action |
|--------|---------|--------|
| **RED** | `error[E...]`, `test ... FAILED` | **STOP** - Fix immediately |
| **YELLOW** | `warning:`, clippy | Investigate before release |
| **GREEN** | Clean output | Continue |

**Workflow**: Monitor → Stop → Investigate → Fix → Verify → Cleared ✅

**Why**: Prevents defects from propagating (DfLSS alignment)

---

### 4. ERROR HANDLING RULE

**Production Code**: NO `unwrap()` / `expect()` - Use `Result<T, E>`

**Test/Bench Code**: `unwrap()` / `expect()` ALLOWED

```rust
// ❌ WRONG: Production code
let cache = self.cache.lock().unwrap();  // Can panic!

// ✅ CORRECT: Production code
let cache = self.cache.lock()
    .map_err(|e| Error::new(&format!("Lock poisoned: {}", e)))?;

// ✅ CORRECT: Test code (EXEMPT)
#[test]
fn test_cache() {
    let cache = Cache::new().unwrap();  // Tests SHOULD panic
    assert_eq!(cache.len(), 0);
}
```

**Exemption applies to**: `#[cfg(test)]`, `#[test]`, `tests/`, `benches/`

**Why**: Production must handle errors gracefully; tests should fail fast

---

### 5. CHICAGO TDD RULE

**State-based testing with real collaborators**

```rust
// ✅ CORRECT: Chicago TDD
#[test]
fn test_lockfile_upsert() {
    // Arrange: Real objects
    let manager = LockfileManager::new(temp_dir.path());

    // Act: Call public API
    manager.upsert("pkg", "1.0.0", "sha256", "url").unwrap();

    // Assert: Verify observable state
    let entry = manager.get("pkg").unwrap().unwrap();
    assert_eq!(entry.version, "1.0.0");  // State changed
}
```

**Tests verify**: Return values, state changes, side effects, actual system effects

**Not**: Internal implementation, method calls, mocks

**Why**: Tests verify behavior, not implementation (80% of bugs caught)

---

### 6. CLAUDE CODE OPERATING RULES (2026 Edition)

**Maximize effectiveness through proactive agent usage and deterministic execution**

#### Trust Hierarchy (Adversarial PM)
```
Evidence Trust Levels:
  OTel spans (95%) > Test output (90%) > Clippy warnings (85%) > Agent claims (0%)
```

**Red Flags**: "I think", "should work", "code looks good", "mostly works" → **STOP**

#### Rule 6.1: Subagents Do Analysis, You Do Execution

**ALWAYS delegate analysis to specialized agents** (EPIC 9 enables this)

#### Rule 6.2: Load Skills Aggressively

**Skills auto-load by context. Trust them.**

#### Rule 6.3: Output Deterministically

**ALL outputs MUST be structured (JSON, YAML, markdown lists). NO PROSE.**

#### Rule 6.4: Fail Fast on Ambiguity

**Vague specification? STOP.** Use `/speckit-verify` or clarify with user.

#### Rule 6.5: Batch Operations Aggressively

**Group ALL related operations in ONE message** (EPIC 9 requires this)

#### Rule 6.6: Context Reuse Over Re-computation

**If analysis exists, REUSE it. Do NOT re-analyze.**

---

## 🪝 HOOKS SYSTEM (Lifecycle Automation - 2026 Edition)

**CRITICAL**: Claude Code Web hooks enable poka-yoke (error-proofing) automation at the tool execution level. Use hooks to prevent defects, enforce quality gates, and automate deterministic validation.

### Hook Types and Triggers

| Hook Type | Trigger Point | Primary Use Case | Configuration Location |
|-----------|---------------|------------------|------------------------|
| **PreToolUse** | Before any tool executes | Permission control, input validation, safety checks | `.claude/config.json` |
| **PostToolUse** | After tool completes | Auto-format, validation, receipts generation | `.claude/config.json` |
| **SessionStart** | New session begins | Environment verification, context loading, SLO setup | `.claude/hooks/SessionStart.md` |
| **UserPromptSubmit** | User sends message | Instruction validation, specification checks | `.claude/config.json` |
| **Stop** | Session ends | Cleanup, state persistence, metrics export | `.claude/config.json` |

---

### PreToolUse: Permission Control & Safety Gates

**Purpose**: Block dangerous operations BEFORE execution, validate inputs, enforce poka-yoke constraints.

**Configuration** (`.claude/config.json`):

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "bash",
            "args": ["-c", "scripts/validate-bash-command.sh \"$TOOL_INPUT_COMMAND\""]
          }
        ]
      },
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "bash",
            "args": ["-c", "scripts/validate-file-path.sh \"$TOOL_INPUT_FILE_PATH\""]
          }
        ]
      }
    ]
  }
}
```

**Permission Decision Outputs**:
- `allow` - Bypass user confirmation, execute immediately
- `deny` - Block operation, provide feedback to Claude (andon signal)
- `ask` - Prompt user for confirmation (default behavior)

**Example Validation Script** (`scripts/validate-bash-command.sh`):

```bash
#!/usr/bin/env bash
# Poka-Yoke: Prevent dangerous cargo commands

COMMAND="$1"

# CRITICAL: Enforce cargo make protocol
if echo "$COMMAND" | grep -qE '^cargo (check|test|build|clippy|run)'; then
  echo "deny"
  echo "ANDON RED: Direct cargo commands forbidden. Use 'cargo make' instead." >&2
  exit 1
fi

# Allow cargo make commands
if echo "$COMMAND" | grep -qE '^cargo make'; then
  echo "allow"
  exit 0
fi

# Deny destructive operations
if echo "$COMMAND" | grep -qE '(rm -rf|git push.*--force|git reset --hard)'; then
  echo "deny"
  echo "ANDON RED: Destructive operation blocked by poka-yoke." >&2
  exit 1
fi

# Default: ask user
echo "ask"
```

**Example Path Validation** (`scripts/validate-file-path.sh`):

```bash
#!/usr/bin/env bash
# Poka-Yoke: Prevent editing protected files

FILE_PATH="$1"

# Block editing generated markdown (must edit .ttl source)
if echo "$FILE_PATH" | grep -qE '\.specify/.*\.md$'; then
  echo "deny"
  echo "ANDON RED: Cannot edit generated .md files. Edit .ttl source instead." >&2
  exit 1
fi

# Block editing Cargo.lock
if echo "$FILE_PATH" | grep -q 'Cargo.lock$'; then
  echo "deny"
  echo "ANDON RED: Cargo.lock is auto-generated. Do not edit manually." >&2
  exit 1
fi

# Allow all other writes
echo "allow"
```

**Use Cases**:
- **Cargo Make Enforcement**: Block direct `cargo` commands, require `cargo make`
- **Path Protection**: Prevent edits to `.specify/**/*.md` (generated files)
- **Specification Closure**: Require `/speckit-verify` before implementation
- **Git Safety**: Block force pushes to main/master
- **Secrets Protection**: Deny commits containing `.env`, `credentials.json`

---

### PostToolUse: Automatic Quality Enforcement

**Purpose**: Auto-format code, validate outputs, generate receipts, train patterns.

**Configuration** (`.claude/config.json`):

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": "bash",
            "args": ["-c", "scripts/post-edit-hook.sh \"$TOOL_INPUT_FILE_PATH\""]
          }
        ]
      },
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "bash",
            "args": ["-c", "scripts/post-bash-hook.sh \"$TOOL_INPUT_COMMAND\" \"$TOOL_EXIT_CODE\""]
          }
        ]
      }
    ]
  }
}
```

**Example Post-Edit Hook** (`scripts/post-edit-hook.sh`):

```bash
#!/usr/bin/env bash
# Auto-format and validate after file edits

FILE_PATH="$1"

# Rust files: Run rustfmt
if [[ "$FILE_PATH" =~ \.rs$ ]]; then
  rustfmt "$FILE_PATH" 2>/dev/null || true
fi

# TOML files: Validate syntax
if [[ "$FILE_PATH" =~ \.toml$ ]]; then
  cargo metadata --format-version 1 &>/dev/null || echo "ANDON YELLOW: TOML syntax may be invalid" >&2
fi

# TTL files: Validate RDF syntax (if oxigraph CLI available)
if [[ "$FILE_PATH" =~ \.ttl$ ]]; then
  if command -v sparql &>/dev/null; then
    sparql --data="$FILE_PATH" --query=<(echo "SELECT * WHERE {} LIMIT 1") &>/dev/null || echo "ANDON YELLOW: TTL syntax invalid" >&2
  fi
fi

# Log edit for audit trail
echo "$(date -Iseconds) EDIT $FILE_PATH" >> .claude/audit.log
```

**Example Post-Bash Hook** (`scripts/post-bash-hook.sh`):

```bash
#!/usr/bin/env bash
# Generate receipts for validation commands

COMMAND="$1"
EXIT_CODE="$2"

# Generate receipt for cargo make commands
if echo "$COMMAND" | grep -qE '^cargo make (check|test|lint|pre-commit)'; then
  TASK="${COMMAND#cargo make }"
  if [ "$EXIT_CODE" -eq 0 ]; then
    echo "[Receipt] cargo make $TASK: ✓ ($(date -Iseconds))" >> .claude/receipts.log
  else
    echo "[Receipt] cargo make $TASK: ✗ ($(date -Iseconds), exit $EXIT_CODE)" >> .claude/receipts.log
    echo "ANDON RED: $TASK failed with exit code $EXIT_CODE" >&2
  fi
fi

# Log all bash commands for audit
echo "$(date -Iseconds) BASH exitcode=$EXIT_CODE: $COMMAND" >> .claude/audit.log
```

**Use Cases**:
- **Auto-Format**: Run `rustfmt` after Rust file edits
- **Syntax Validation**: Check TOML/TTL syntax after edits
- **Receipt Generation**: Log validation results (Paradigm Shift 3)
- **Audit Trail**: Track all commands for reproducibility
- **Andon Signals**: Emit warnings/errors for quality gates
- **Pattern Training**: Feed successful patterns to neural systems

---

### SessionStart: Environment Verification & Context Loading

**Purpose**: Verify environment setup, load project context, initialize SLOs, restore session state.

**Configuration** (`.claude/hooks/SessionStart.md`):

```markdown
---
description: Initialize ggen development environment and verify toolchain
allowed_tools: Bash, Read, Write
---

# Session Start Hook - ggen Project

## 1. Environment Verification (Poka-Yoke)

**Verify Rust toolchain:**
```bash
rustc --version | grep -q '1.91' || echo "ANDON YELLOW: Rust 1.91.1 recommended, $(rustc --version) detected"
cargo --version
cargo make --version || echo "ANDON RED: cargo-make not installed"
```

**Verify required tools:**
```bash
command -v git >/dev/null || echo "ANDON RED: git not found"
command -v gh >/dev/null || echo "ANDON YELLOW: gh CLI not installed (needed for PRs)"
```

## 2. Project Context Loading

**Read critical documentation:**
```bash
# Load project constitution
cat CLAUDE.md | head -100  # First 100 lines for quick reference

# Check current git status
git status --short
git log --oneline -5  # Recent commits

# Verify clean workspace
[ -z "$(git status --porcelain)" ] || echo "ANDON YELLOW: Uncommitted changes detected"
```

## 3. SLO Initialization

**Establish performance baselines:**
```bash
# Record session start time
echo "SESSION_START=$(date +%s)" > .claude/session.env

# Quick health check
timeout 5s cargo make check &>/dev/null && echo "SLO ✓: check <5s" || echo "ANDON YELLOW: check timeout"
```

## 4. Specification Status

**Verify specification closure:**
```bash
# List incomplete specifications
find .specify/specs -name '*.ttl' -exec grep -l 'TBD\|TODO\|FIXME' {} \; | while read f; do
  echo "ANDON YELLOW: Incomplete spec: $f"
done
```

## 5. Memory Restoration (if applicable)

**Restore previous session context:**
```bash
# Load session memory if exists
if [ -f .claude/session-memory.json ]; then
  echo "Restoring session from $(date -r .claude/session-memory.json)"
fi
```

## Summary

Provide Claude with:
- ✅ Environment status (Rust version, tools available)
- ✅ Git status (branch, uncommitted changes)
- ✅ SLO baselines (check passed/failed)
- ✅ Specification status (complete/incomplete)
- ⚠️ Any ANDON signals requiring attention
```

**Use Cases**:
- **Toolchain Verification**: Ensure Rust 1.91.1, cargo-make installed
- **Git Context**: Load current branch, recent commits, dirty state
- **SLO Baselines**: Verify `cargo make check` passes in <5s
- **Specification Status**: Check for incomplete .ttl files
- **Session Restoration**: Load previous context from `.claude/session-memory.json`

---

### Hook Best Practices (Aligned with ggen Philosophy)

**1. Poka-Yoke First**
- Use PreToolUse to **prevent defects**, not detect them
- Block operations that violate constitutional rules
- Example: Deny edits to `.specify/**/*.md` (generated files)

**2. Deterministic Receipts**
- Use PostToolUse to **generate evidence**, not opinions
- Log validation results to `.claude/receipts.log`
- Format: `[Receipt] cargo make test: ✓ (347/347 pass, 2026-01-05T14:32:01Z)`

**3. Andon Signal Protocol**
- Emit signals via stderr: `echo "ANDON RED: ..." >&2`
- **RED**: Blocking defect, stop the line
- **YELLOW**: Warning, investigate before proceeding
- **GREEN**: Clean output, continue

**4. Audit Trail**
- Log ALL tool executions to `.claude/audit.log`
- Format: `2026-01-05T14:32:01Z BASH exitcode=0: cargo make check`
- Enables reproducibility and debugging

**5. Zero-Cost Abstractions**
- Keep hooks **fast** (<100ms overhead)
- Use `timeout` for validation commands
- Example: `timeout 1s rustfmt --check file.rs`

**6. Fail-Safe Defaults**
- Hooks SHOULD NOT fail the operation on script errors
- Use `|| true` to continue on validation failures
- Example: `rustfmt "$FILE" 2>/dev/null || true`

**7. Specification-First**
- SessionStart hook MUST check specification closure
- Block implementation if `/speckit-verify` shows incomplete specs
- Align with Paradigm Shift 1 (Big Bang 80/20)

**8. EPIC 9 Support**
- Hooks enable parallel agent coordination
- PostToolUse can broadcast completion to other agents
- Example: Write completion markers to shared memory

---

### Hook Configuration File Locations

| File | Purpose | Scope |
|------|---------|-------|
| `.claude/config.json` | Pre/Post hooks, permissions | Project |
| `.claude/hooks/SessionStart.md` | Environment verification | Project |
| `~/.claude/config.json` | Global hook defaults | User |
| `scripts/validate-*.sh` | Validation logic | Project |
| `.claude/audit.log` | Audit trail (auto-generated) | Session |
| `.claude/receipts.log` | Validation receipts (auto-generated) | Session |

---

### Example: Complete Hook Setup for ggen

**`.claude/config.json`:**

```json
{
  "hooks": {
    "PreToolUse": [
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "bash",
            "args": ["-c", "scripts/validate-bash-command.sh \"$TOOL_INPUT_COMMAND\""]
          }
        ]
      },
      {
        "matcher": "Write|Edit",
        "hooks": [
          {
            "type": "command",
            "command": "bash",
            "args": ["-c", "scripts/validate-file-path.sh \"$TOOL_INPUT_FILE_PATH\""]
          }
        ]
      }
    ],
    "PostToolUse": [
      {
        "matcher": "Edit|Write",
        "hooks": [
          {
            "type": "command",
            "command": "bash",
            "args": ["-c", "scripts/post-edit-hook.sh \"$TOOL_INPUT_FILE_PATH\""]
          }
        ]
      },
      {
        "matcher": "Bash",
        "hooks": [
          {
            "type": "command",
            "command": "bash",
            "args": ["-c", "scripts/post-bash-hook.sh \"$TOOL_INPUT_COMMAND\" \"$TOOL_EXIT_CODE\""]
          }
        ]
      }
    ]
  },
  "permissions": {
    "allow": [
      "Read(**)",
      "Bash(cargo make *)",
      "Edit(src/**)",
      "Edit(crates/**)",
      "Write(tests/**)",
      "Write(.claude/**)"
    ],
    "deny": [
      "Bash(cargo check)",
      "Bash(cargo test)",
      "Bash(cargo build)",
      "Bash(rm -rf *)",
      "Edit(.specify/**/*.md)",
      "Edit(Cargo.lock)",
      "Write(.env*)",
      "Write(**/*secret*)",
      "Write(**/*credential*)"
    ]
  }
}
```

**Validation Scripts**: Create `scripts/validate-bash-command.sh`, `scripts/validate-file-path.sh`, `scripts/post-edit-hook.sh`, `scripts/post-bash-hook.sh` as shown above.

**SessionStart Hook**: Create `.claude/hooks/SessionStart.md` as shown above.

---

### Integration with Three Paradigm Shifts

**Paradigm Shift 1: Big Bang 80/20**
- SessionStart hook verifies specification closure before implementation
- PreToolUse blocks code generation if `/speckit-verify` not run
- Enforces "no implementation without closed spec"

**Paradigm Shift 2: EPIC 9 Parallel-First**
- PostToolUse broadcasts completion events to coordination layer
- Hooks enable agents to work independently without polling
- Example: Write `agent-1-complete` marker after task finish

**Paradigm Shift 3: Deterministic Validation**
- PostToolUse generates timestamped receipts automatically
- All validation results logged to `.claude/receipts.log`
- Replaces human review with reproducible evidence

---

### Hook Variables Reference

**Available in hook commands:**

| Variable | Description | Example |
|----------|-------------|---------|
| `$TOOL_NAME` | Tool being executed | `Bash`, `Edit`, `Write` |
| `$TOOL_INPUT_COMMAND` | Bash command (Bash tool) | `cargo make check` |
| `$TOOL_INPUT_FILE_PATH` | File path (Edit/Write) | `src/main.rs` |
| `$TOOL_EXIT_CODE` | Exit code (PostToolUse) | `0`, `1` |
| `$TOOL_OUTPUT` | Tool output (PostToolUse) | Command stdout |

**Custom environment variables** can be set in hooks and persisted to `.claude/session.env`.

---

### Advanced: Hook Chains & Composition

**Hooks can be chained for complex workflows:**

```json
{
  "hooks": {
    "PostToolUse": [
      {
        "matcher": "Edit(src/**/*.rs)",
        "hooks": [
          {
            "type": "command",
            "command": "rustfmt",
            "args": ["$TOOL_INPUT_FILE_PATH"]
          },
          {
            "type": "command",
            "command": "cargo",
            "args": ["make", "check"]
          },
          {
            "type": "command",
            "command": "bash",
            "args": ["-c", "echo '[Receipt] Edit + check: ✓' >> .claude/receipts.log"]
          }
        ]
      }
    ]
  }
}
```

**This creates a quality gate**: Edit → Auto-format → Fast check → Receipt generation.

---

### Troubleshooting Hooks

**Hook script not executing:**
- Verify script has execute permissions: `chmod +x scripts/*.sh`
- Check script shebang: `#!/usr/bin/env bash`
- Test manually: `bash scripts/validate-bash-command.sh "cargo make check"`

**Hook blocking valid operations:**
- Check PreToolUse output: Should be `allow`, `deny`, or `ask`
- Review `.claude/audit.log` for details
- Temporarily disable hook in `.claude/config.json`

**Hook performance issues:**
- Add `timeout` to slow operations: `timeout 1s validation-script.sh`
- Use background jobs for non-blocking tasks: `task.sh &`
- Profile with `time`: `time bash scripts/post-edit-hook.sh file.rs`

---

### 7. CONTEXT WINDOW OPTIMIZATION RULE

**Maximize token efficiency and cognitive throughput through aggressive context management**

Context windows are finite resources. Inefficient context usage creates three critical failures:
1. **Token exhaustion** - Running out of context mid-task
2. **Cognitive dilution** - Important signals buried in noise
3. **Latency bloat** - Processing irrelevant context slows responses

**The 80% Rule**: Never exceed 80% of context window capacity. Reserve 20% for:
- Unexpected complexity expansion
- Error recovery context
- Collision detection results (EPIC 9)
- Receipt accumulation (validation artifacts)

---

#### 7.1: The /context Command Pattern

**Purpose**: Explicit context window monitoring and management

**Usage**:
```bash
# Check current context utilization
/context status

# Show context breakdown by category
/context breakdown

# Identify largest context consumers
/context top

# Set context budget for current task
/context budget 60%  # Reserve 40% for expansion
```

**When to use**:
- Before starting EPIC 9 cycles (verify headroom for 10+ agent outputs)
- When approaching 60% utilization (yellow threshold)
- After large file reads (check impact)
- Before spawning subagents (ensure each has adequate budget)

**Thresholds**:
```
  0-60%: GREEN   - Normal operation, full capabilities available
 60-80%: YELLOW  - Caution, begin context compression strategies
 80-95%: ORANGE  - Critical, immediate compaction required
95-100%: RED     - Emergency, task segmentation mandatory
```

---

#### 7.2: The /compact Command Pattern

**Purpose**: Aggressive context compression without information loss

**Usage**:
```bash
# Compact current conversation (preserve decisions, discard exploration)
/compact conversation

# Compact file contents (extract schema, discard boilerplate)
/compact files

# Compact agent outputs (preserve conclusions, discard intermediate reasoning)
/compact agents

# Full context compression (emergency mode)
/compact all --aggressive
```

**What gets compressed**:
1. **Exploration artifacts**: Trial outputs, rejected alternatives, "what if" discussions
2. **Redundant information**: Repeated file contents, duplicate explanations
3. **Verbose outputs**: Stack traces → error summaries, full diffs → change summaries
4. **Intermediate reasoning**: Keep conclusions, compress derivation paths

**What gets preserved** (NEVER compact):
1. **Specifications**: TTL files, API contracts, requirements
2. **Decisions**: Architecture choices, trade-off rationale
3. **Receipts**: Validation outputs, test results, SLO compliance
4. **Active context**: Current task state, pending work

**When to compact**:
- Immediately when hitting YELLOW threshold (60%)
- Before EPIC 9 fan-out (create clean slate for agents)
- After exploration phases (compress before convergence)
- When context contains >3 large file reads

---

#### 7.3: Subagent Isolation Patterns (EPIC 9 Enabler)

**The Problem**: 10 parallel agents with full context = 10x token consumption = context explosion

**The Solution**: Surgical context allocation via isolation

**Pattern 1: Minimal Context Injection**

Give agents ONLY what they need:

```python
# ❌ WRONG: Full context dump
Agent("Implement user auth", context=full_conversation_history)

# ✅ CORRECT: Surgical context
Agent(
  "Implement user auth",
  context={
    "spec": auth_spec_ttl,        # Only relevant spec
    "constraints": security_reqs,  # Only relevant constraints
    "interfaces": api_contract     # Only relevant interfaces
  }
)
```

**Pattern 2: Reference-Based Context**

Instead of copying large artifacts, use references:

```python
# ❌ WRONG: Embed 50KB file in agent context
Agent("Generate handler", files={"user.rs": file_contents_50kb})

# ✅ CORRECT: Reference path, agent reads if needed
Agent("Generate handler", file_refs=["/path/to/user.rs"])
```

**Pattern 3: Context Sharding**

Partition work so each agent operates on disjoint context:

```python
# Shard by module (no overlap)
Agent 1: context={module: "auth",   files: ["auth/**/*.rs"]}
Agent 2: context={module: "db",     files: ["db/**/*.rs"]}
Agent 3: context={module: "api",    files: ["api/**/*.rs"]}

# Each agent sees ONLY their shard (10x context efficiency)
```

**Pattern 4: Lazy Context Loading**

Agents request context on-demand, not upfront:

```python
# Agent spawned with minimal context
agent = Agent("Optimize query performance", context={"target": "db_queries"})

# Agent requests specific files ONLY when needed
agent.read_file("src/db/query.rs")  # On-demand, not preloaded
```

---

#### 7.4: Token Efficiency Best Practices

**1. Prefer grep over read for exploration**

```bash
# ❌ INEFFICIENT: Read 10 files speculatively (10,000+ tokens)
Read "file1.rs"
Read "file2.rs"
...

# ✅ EFFICIENT: Grep first, read only matches (200 tokens)
Grep pattern="UserAuth" → identifies 2 relevant files → Read those 2
```

**2. Use head_limit for large outputs**

```bash
# ❌ INEFFICIENT: Full 5000-line file
Grep pattern="TODO" output_mode="content"

# ✅ EFFICIENT: First 50 matches only
Grep pattern="TODO" output_mode="content" head_limit=50
```

**3. Compress receipts into summaries**

```bash
# ❌ INEFFICIENT: 10KB test output
[Full pytest output with 347 test names and timing details...]

# ✅ EFFICIENT: Receipt summary
[Receipt] cargo make test: ✓ (347/347 pass, 28.4s, 0 failures)
```

**4. Reference, don't repeat**

```
# ❌ INEFFICIENT: Repeat spec in every message
"As specified in the TTL file (content repeated here)..."

# ✅ EFFICIENT: Reference by path
"As per specification in .specify/specs/024-auth/auth.ttl (lines 45-67)"
```

**5. Batch file operations**

```python
# ❌ INEFFICIENT: Sequential reads (10x round-trips)
Read "a.rs"
Read "b.rs"
Read "c.rs"

# ✅ EFFICIENT: Parallel reads (1 round-trip)
[Single message with 3 Read calls]
```

---

#### 7.5: Context Window SLOs

**Target Utilization Curves**:

```
Task Type          | Peak Utilization | Reserve
-------------------|------------------|----------
Trivial (1 file)   | <20%            | 80%
Moderate (3-5 ops) | <40%            | 60%
Complex (EPIC 9)   | <70%            | 30%
Emergency          | <90%            | 10% (danger zone)
```

**Enforcement**:
- Monitor utilization after every major operation
- Trigger /compact when crossing thresholds
- Abort task if unable to maintain 10% reserve
- Use subagent isolation for tasks predicting >80% utilization

**Measurement**:
```bash
# Check if within SLO
current_tokens / max_tokens <= 0.80  # PASS
current_tokens / max_tokens > 0.80   # FAIL → compact required
```

---

#### 7.6: EPIC 9 Context Budget Allocation

**The Challenge**: 10 agents + collision detection + convergence = massive context

**The Solution**: Pre-allocated context budgets with hard limits

**Budget Allocation** (for 200K token context):
```
Main thread:           40K tokens (20%)  - Orchestration, specifications
Agent outputs:        100K tokens (50%)  - 10 agents × 10K each (isolated)
Collision detection:   20K tokens (10%)  - Overlap analysis
Convergence synthesis: 20K tokens (10%)  - Selection + merge
Reserve:               20K tokens (10%)  - Error recovery, expansion
```

**Enforcement**:
- Each agent gets hard 10K token limit
- Outputs exceeding limit → automatic summarization
- Collision detection works on compressed artifacts
- Convergence synthesizes from summaries, not full outputs

**Example**:
```python
# EPIC 9 with context budgets
for agent in agents:
    agent.context_limit = 10_000  # Hard cap
    agent.output_limit = 8_000    # Reserve for receipts

# If agent tries to output 15K tokens → auto-compressed to 8K
# Compression: preserve decisions, discard exploration
```

---

#### 7.7: Context-Aware Task Segmentation

**When single task exceeds context budget → segment into phases**

**Pattern**:
```
Phase 1: Specification (consume: 20%, output: spec.ttl)
  ↓ [/compact conversation]
Phase 2: Implementation (consume: 60%, input: spec.ttl, output: code)
  ↓ [/compact files]
Phase 3: Validation (consume: 30%, input: code, output: receipts)
  ↓ [/compact agents]
Done: Total context reused 3x via aggressive compaction
```

**Segmentation Triggers**:
- Task estimated to require >70% context (segment into 2-3 phases)
- Current utilization >60% and more work remains (segment immediately)
- EPIC 9 with >15 agents (phase 1: agents 1-10, phase 2: agents 11-15)

**Benefits**:
- Each phase starts with clean context (post-compact)
- Intermediate artifacts stored in files (not context)
- Can handle arbitrarily large tasks (N phases)

---

#### 7.8: Prohibited Patterns (Context Anti-Patterns)

1. **Speculative reading**: Reading files "just in case" → Use grep first
2. **Full file dumps**: Including entire files in responses → Reference by path + line numbers
3. **Redundant explanations**: Repeating concepts already in context → Reference previous explanation
4. **Verbose error outputs**: Full stack traces → Summarize to error type + location
5. **Uncompressed agent outputs**: Keeping all 10 agent explorations → Compress after convergence
6. **No utilization monitoring**: Blindly consuming context → Check after every major operation
7. **Context hoarding**: Keeping "might need later" artifacts → Aggressive eviction, re-read if needed

---

#### 7.9: Benefits Summary

**With Context Window Optimization**:
- **2-3x more work per session** (via compression + reuse)
- **Faster EPIC 9 cycles** (isolated agents = lower overhead)
- **Higher quality outputs** (signal-to-noise ratio maximized)
- **Reduced latency** (smaller context = faster processing)
- **Deterministic capacity** (SLO-based budgets prevent exhaustion)

**Without it**:
- Context exhaustion mid-task (failure)
- Cognitive dilution (important signals buried)
- Latency bloat (processing irrelevant context)
- EPIC 9 failure (10 agents exceed budget)

**Context optimization is NOT optional for EPIC 9** - it's the prerequisite that makes parallel agent orchestration viable.

---

## 📁 File Organization (Never Save to Root)

```
ggen/
├── .specify/                      # RDF-first specification system (source of truth)
│   ├── ontology/                  # Ontology schemas
│   ├── memory/                    # Project memory (constitution.ttl)
│   ├── specs/NNN-feature/         # Feature specifications
│   │   ├── *.ttl                  # TTL source files (EDIT THESE)
│   │   ├── *.md                   # Generated (NEVER EDIT)
│   │   └── evidence/              # Test evidence
│   └── templates/                 # Templates for generation
├── crates/*/src/                  # Source code (per crate)
├── crates/*/tests/                # Integration tests
├── tests/                         # Workspace tests
├── docs/                          # Documentation
├── scripts/                       # Build scripts
├── benches/                       # Benchmarks
└── templates/                     # Code generation templates
```

**Rule**: TTL is source of truth, markdown is generated. **NEVER edit .md, edit .ttl source, regenerate.**

---

## 🦀 Elite Rust Mindset (Type-First Thinking)

### The Questions to Ask:
1. **"What can I express in types?"** (before runtime values)
2. **"Is this abstraction zero-cost?"** (generics yes, trait objects no)
3. **"What are the ownership semantics?"** (explicit is better)
4. **"How can I make misuse impossible?"** (type safety > runtime checks)

---

## 🔧 Essential Commands

**Quick Feedback Loop**:
- `cargo make check` - Compilation (<5s)
- `cargo make test-unit` - Unit tests (<10s)
- `cargo make lint` - Clippy

**Full Validation**:
- `cargo make test` - All tests
- `cargo make pre-commit` - Format + lint + tests
- `cargo make ci` - Full CI pipeline

**EPIC 9 Commands**:
- `/speckit-verify [feature]` - Verify closure before implementation
- `/bb80-parallel "[spec]"` - Orchestrate atomic cycle (10+ agents)
- `/collision-detect` - Analyze overlaps between agent outputs
- `/convergence` - Synthesize results from parallel agents

**Quality Assurance Commands**:
- `/speckit-check` - Validate RDF specs in .specify/ directory
- `/test-audit` - Run mutation testing and assertion analysis
- `/review-errors` - Audit error handling patterns
- `/optimize` - Analyze and optimize Rust performance
- `/bench-compare` - Compare benchmark results across commits

**Available Skills** (auto-loaded by context):
- `cargo-make-protocol` - Master cargo make build orchestration
- `chicago-tdd-pattern` - State-based testing with AAA pattern
- `poka-yoke-patterns` - Error-proofing and quality gates
- `rdf-ontologies` - RDF/Turtle syntax and SPARQL queries
- `bb80-specification-closure` - Verify specification completeness
- `bb80-parallel-agents` - Parallel agent orchestration patterns
- `bb80-deterministic-receipts` - Evidence-based validation
- `bb80-invariant-construction` - Type-driven invariant design

---

## 🚫 Prohibited Patterns (Zero Tolerance)

1. **Direct cargo commands** - ALWAYS use `cargo make`
2. **Unwrap/expect in production** - Use `Result<T, E>`
3. **Ignoring Andon signals** - Stop the line when RED
4. **Iteration due to vague spec** - Use `/speckit-verify` first
5. **Sequential agent dispatch** - Use EPIC 9 (parallel)
6. **Narrative review** - Use receipts (deterministic)
7. **Saving to root** - Use proper subdirectories
8. **Context window violations** - Exceeding 80% without compaction

---

## 🎯 SLOs (Service Level Objectives)

- First build ≤ 15s
- Incremental ≤ 2s
- RDF processing ≤ 5s for 1k+ triples
- Generation memory ≤ 100MB
- CLI scaffolding ≤ 3s end-to-end
- 100% reproducible outputs
- Test execution ≤ 30s (timeout escalates to 120s on contention)

---

## 📝 Speckit Workflow (RDF-First Specification Development)

**MANDATORY**: NO implementation without spec. RDF ontology is source of truth.

### Architecture: Ontology → Code

```
spec.ttl (source) → ggen render → spec.md (derived artifact)
```

### Workflow Integration (RDF-First)

1. **Before feature**: Run `/speckit-verify` → Check closure
2. **During planning**: Invoke `speckit-architect` agent → Architecture decisions in RDF/Turtle
3. **Before EPIC 9**: Verify specification is 100% closed (run `/speckit-verify`)
4. **During implementation**: Read TTL source for truth
5. **Throughout**: Evidence in `.specify/specs/NNN-feature/evidence/`

**NEVER manually edit .md files** - Edit .ttl source, regenerate markdown.

---

## 🔧 Using ggen Internally: Code Generation Instead of Hand-Coding

**CRITICAL INSIGHT**: ggen is not just a build tool—it's your **specification compiler**. Use it to replace hand-coding entirely.

### The Specification-First Approach

Instead of writing code by hand, you **declare** what you want in RDF (Turtle), then ggen **compiles** it into code.

**Pattern:**
```
Write TTL Ontology → ggen sync → Generates Code
  (Specification)              (Implementation)
```

This replaces manual boilerplate with deterministic generation. **Time savings: 60-80% reduction in hand-coding**.

### 1. The ggen sync Command - Your Unified Interface

**Single command**: `ggen sync` orchestrates the entire code generation pipeline:

```bash
# Basic generation from manifest
ggen sync

# Dry-run preview
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

**How it works** (pipeline):
```
Load ggen.toml → Load Ontology (TTL) → Phase 1: Inference (CONSTRUCT queries)
  ↓
Materialize enriched graph → Phase 2: Generation (SELECT queries + Templates)
  ↓
Render Tera templates with SPARQL results → Write files
  ↓
Audit trail + reproducibility metadata
```

### 2. Define Your Domain in RDF/Turtle

Instead of hand-writing hundreds of lines, write a declarative TTL specification:

**Example: CLI Project Specification** (from ggen marketplace)

```turtle
@prefix cnv: <https://ggen.dev/clap-noun-verb#> .

# Project metadata
<http://example.com/calculator> a cnv:CliProject ;
    cnv:projectName "calculator" ;
    cnv:projectVersion "0.1.0" ;
    cnv:projectDescription "Simple arithmetic CLI" .

# Define resource (noun)
<#calc> a cnv:Noun ;
    cnv:nounName "calc" ;
    cnv:hasVerbs <#add>, <#subtract>, <#multiply> .

# Define actions (verbs)
<#add> a cnv:Verb ;
    cnv:verbName "add" ;
    cnv:verbDescription "Add two numbers" ;
    cnv:hasArguments <#leftArg>, <#rightArg> .

# Define parameters (arguments)
<#leftArg> a cnv:Argument ;
    cnv:argumentName "left" ;
    cnv:argumentType "i32" ;
    cnv:isRequired true .

<#rightArg> a cnv:Argument ;
    cnv:argumentName "right" ;
    cnv:argumentType "i32" ;
    cnv:isRequired true .
```

This 25-line TTL spec replaces **500+ lines of hand-written clap derive code**.

### 3. Manifest Configuration (ggen.toml)

Define generation rules and templates:

```toml
[project]
name = "calculator"
version = "0.1.0"

[ontology]
source = "ontology.ttl"       # Your RDF specification
imports = ["common.ttl"]      # Optional imports

[ontology.prefixes]
cnv = "https://ggen.dev/clap-noun-verb#"

# Phase 1: Enrichment via CONSTRUCT queries
[[inference.rules]]
name = "derive-verbs"
description = "Enrich graph with verb handlers"
construct = """
CONSTRUCT {
  ?verb a cnv:HandlerFunction ;
    cnv:handler_name ?name .
}
WHERE {
  ?verb a cnv:Verb ;
    cnv:verbName ?name .
}
"""

# Phase 2: Generation via SELECT + Templates
[[generation.rules]]
name = "generate-cli"
query = { inline = """
PREFIX cnv: <https://ggen.dev/clap-noun-verb#>
SELECT ?projectName ?version ?noun ?verbName
WHERE {
  ?proj a cnv:CliProject ;
    cnv:projectName ?projectName ;
    cnv:projectVersion ?version .
  ?noun cnv:hasVerbs ?verb .
  ?verb cnv:verbName ?verbName .
}
""" }
template = { file = "templates/cli.rs.tmpl" }
output_file = "src/main.rs"
mode = "Overwrite"

# Validation rules
[validation]
validate_syntax = true  # Validate generated Rust syntax
no_unsafe = true        # Reject code containing unsafe blocks
```

**Verification Note**: This documentation has been verified against the actual `GgenManifest` type definitions in `crates/ggen-core/src/manifest/types.rs`. The manifest structure, query source formats, and template source syntax all match the source code specifications. A live `ggen sync` execution has not yet been performed due to build dependencies still compiling, but the API is verified architecturally correct.

### 4. Templates Extract SPARQL Results

Tera templates access SPARQL query results via special context variables:

**Example: CLI Template** (`templates/cli.rs.tmpl`)

```jinja2
---
to: "src/main.rs"
vars:
  - project_name
sparql:
  verbs: |
    PREFIX cnv: <https://ggen.dev/clap-noun-verb#>
    SELECT ?noun ?verbName ?description
    WHERE {
      ?noun a cnv:Noun ; cnv:nounName ?noun_name .
      ?verb a cnv:Verb ;
        cnv:verbName ?verbName ;
        cnv:verbDescription ?description ;
        cnv:inNoun ?noun .
    }
---

// Generated by ggen from ontology
use clap::Parser;

#[derive(Parser)]
#[command(name = "{{ project_name | pascal }}")]
pub enum Cli {
    {% for verb in sparql_results.verbs %}
    /// {{ verb.description }}
    #[command(name = "{{ verb.verbName | lower }}")]
    {{ verb.verbName | pascal }} {
        /// Left operand
        left: i32,
        /// Right operand
        right: i32,
    },
    {% endfor %}
}

impl Cli {
    pub async fn execute(self) -> Result<()> {
        match self {
            {% for verb in sparql_results.verbs %}
            Self::{{ verb.verbName | pascal }} { left, right } => {
                let result = handlers::{{ verb.noun | snake }}::{{ verb.verbName }}(left, right)?;
                println!("{}", result);
            }
            {% endfor %}
        }
        Ok(())
    }
}
```

**Result**: 30-line template generates **complete CLI handler boilerplate** from SPARQL results.

### 5. Real-World Workflow: REST API Generation

**How to generate REST API without hand-coding:**

```bash
# Step 1: Write API specification in TTL
cat > api_spec.ttl << 'EOF'
@prefix api: <http://ggen.io/ontology/api#> .

<http://example.com/api> a api:RestApi ;
    api:basePath "/api" ;
    api:version "1.0.0" .

<#users> a api:Resource ;
    api:resourceName "users" ;
    api:resourcePath "/users" ;
    api:hasEndpoints <#createUser>, <#listUsers> .

<#createUser> a api:Endpoint ;
    api:method "POST" ;
    api:path "/users" ;
    api:requestBody api:CreateUserRequest ;
    api:responseBody api:User .

<#listUsers> a api:Endpoint ;
    api:method "GET" ;
    api:path "/users" ;
    api:responseBody api:UserList .
EOF

# Step 2: Create ggen.toml referencing API template pack
cat > ggen.toml << 'EOF'
[project]
name = "rest-api"

[ontology]
source = "api_spec.ttl"

[[generation.rules]]
name = "handlers"
template = { pack = "rest-api-handlers", version = "1.0" }
EOF

# Step 3: Generate
ggen sync

# Result: Complete Axum handlers, request/response types, error handling
# Generated in: src/handlers/users.rs, src/models.rs, src/error.rs
```

### 6. Incremental Generation (Watch Mode)

**Leverage caching for 2-4x speedup on iteration**:

```bash
# Watch mode monitors TTL changes
ggen sync --watch --verbose

# On each change:
# 1. Detects what changed (manifest/ontology/rules)
# 2. Invalidates only affected generation rules
# 3. Re-runs dependent rules (propagates changes)
# 4. Skips unchanged rules
# Result: 1-2s incremental regeneration vs 5-10s full rebuild
```

**Cache stored in**: `.ggen/cache/` (hashes of manifest, ontology, per-rule state)

### 7. Validation Before Generation (Poka-Yoke)

**Prevent defective specs from generating bad code**:

```bash
# Validate specification against SHACL constraints
ggen sync --validate-only

# This checks:
# - All required properties present (sh:minCount)
# - Values match datatypes (sh:datatype)
# - Values match patterns (sh:pattern - regex)
# - Values in allowed enumeration (sh:in)
# - No circular dependencies in inference rules
# If validation fails: ggen exits with error code, NO code is generated

# Only proceed to generation when validation passes
if ggen sync --validate-only; then
  ggen sync  # Safe to generate
fi
```

### 8. Parallelization: Why ggen Enables EPIC 9

This is the **breakthrough insight** that makes parallel agent orchestration possible:

**The Pattern:**
```
Specification (TTL) is deterministic input
             ↓
10 independent agents can generate FROM THE SAME SPEC
             ↓
All 10 outputs are identical, correct implementations
             ↓
Agents converge on same solution → HIGH CONFIDENCE
```

**Example: 10 agents writing CLI handlers in parallel**

```
Team Specification: api_spec.ttl (single source of truth)
       ↓
Agent 1: ggen sync → handler 1 generated
Agent 2: ggen sync → handler 2 generated
...
Agent 10: ggen sync → handler 10 generated
       ↓
All agents' code is identical (compiled from same spec)
Result: EPIC 9 achieves 2.8-4.4x speedup
```

**Why this works:**
- Specifications are unambiguous (RDF is formal logic)
- Code generation is deterministic (same spec = same code)
- No iteration needed (spec is verified closed before generation)
- Agents can work in true parallelism (no coordination overhead)

### 9. Command Reference

| Command | Purpose | SLO |
|---------|---------|-----|
| `ggen sync` | Full generation pipeline | <5s |
| `ggen sync --dry-run` | Preview without writing | <5s |
| `ggen sync --watch` | Watch TTL files, auto-regenerate | Incremental <2s |
| `ggen sync --validate-only` | SHACL validation gate | <3s |
| `ggen sync --rule NAME` | Run specific rule | <2s |
| `ggen sync --force` | Overwrite without checks | <5s |
| `ggen sync --audit` | Detailed audit trail | <5s |

### 10. Key Design Principles (Why ggen beats hand-coding)

| Aspect | Hand-Coding | ggen Specification |
|--------|-------------|-------------------|
| **Declarative** | ❌ Imperative (how) | ✅ Declarative (what) |
| **Reproducible** | ❌ Manual variations | ✅ Deterministic |
| **Parallelizable** | ❌ Coordination overhead | ✅ Same spec → same output |
| **Composable** | ❌ Each project unique | ✅ Specifications compose |
| **Validatable** | ❌ Test after coding | ✅ Validate before generation |
| **Iterable** | ❌ Iterate code | ✅ Iterate specification |
| **Auditable** | ❌ Narrative reviews | ✅ Deterministic receipts |
| **Time** | ❌ 100% from scratch | ✅ 60-80% faster |

### Summary: When to Use ggen vs Hand-Code

**Use ggen when:**
- Generating multiple similar artifacts (CLIs, APIs, database schemas)
- Working with a specification (domain model, API contract)
- Needing reproducibility across environments/agents
- Parallelizing work across multiple agents
- Generating from domain-specific languages

**Hand-code when:**
- One-off utility, no reuse
- Custom logic not covered by templates
- Exploring/prototyping (spec not yet stable)

**Best practice**: **Specification + ggen for 80% of code, hand-code for custom 20%**

---

## 🏗️ CODEBASE ARCHITECTURE (2026 Edition)

### Project Status
- **Version**: 5.2.0 (ggen init, example rewrites, gVisor E2E testing)
- **Rust**: 1.91.1 (installed), MSRV: 1.75+, Edition 2021
- **Repository**: https://github.com/seanchatmangpt/ggen
- **License**: MIT

### Recent Changes (v5.2.0)

**New Features**:
- `ggen init` command for project scaffolding with templates
- Complete rewrite of 5 core examples (simple-project, basic-template-generation, ai-template-creation, complete-project-generation)
- gVisor sandbox integration for deterministic E2E testing
- 8-phase DEB (Docker Enterprise Build) pipeline

**Architecture Improvements**:
- EPIC 9 atomic cognitive cycle automation with `/bb80-parallel`
- Adversarial validation framework for quality assurance
- TypeScript examples converted to pure ESM/MJS
- Enhanced capability detection and permission validation

**CI/CD Enhancements**:
- Local GitHub Actions testing via `act`
- Improved pre-commit hooks with timeout escalation
- SLO enforcement on all cargo make targets

### Workspace Structure (14 Active Crates, 17 Total)

**Core Infrastructure**:
- **ggen-core**: Graph-aware code generation engine, RDF processing, caching
- **ggen-utils**: Error handling, logging, configuration
- **ggen-domain**: CLI-agnostic domain layer, concurrent agents
- **ggen-config**: Configuration types and validation
- **ggen-cli-validation**: IO validation, path traversal prevention
- **ggen-config-clap**: Clap-based CLI configuration

**User-Facing**:
- **ggen-cli**: Main CLI interface (binary: `ggen`)
- **ggen-ai**: LLM integration (OpenAI, Anthropic, Ollama) - *experimental, excluded from v5.2 build*

**Advanced Features**:
- **ggen-marketplace** (20K lines): Package registry, FMEA dependency management
- **ggen-test-audit**: Mutation testing, assertion analysis
- **ggen-test-opt**: Test optimization
- **ggen-e2e**: End-to-end testing with testcontainers + gVisor
- **ggen-node**: Node.js/WASM bindings
- **ggen-macros**: Procedural macros
- **ggen-dod**: Data-Oriented Design utilities

### Technology Stack (Current Versions)

**Async & Concurrency**:
- `tokio 1.47`, `rayon 1.11`, `async-trait 0.1`, `lru 0.16`

**RDF & Ontologies**:
- `oxigraph 0.5.1` (RDF store, SPARQL queries)
- `tera 1.20` (template engine)

**Error Handling**:
- `thiserror 2.0`, `anyhow 1.0`

**Testing**:
- `chicago-tdd-tools 1.4.0` (state-based, AAA pattern)
- `proptest 1.8` (property-based)
- `criterion 0.7` (14 benchmark suites in benches/)
- `testcontainers 0.25` (E2E with gVisor sandboxing)
- `insta 1.43` (snapshot testing)
- `assert_cmd 2.0` + `assert_fs 1.1` (CLI testing)

**Validation & Security**:
- `pqcrypto-mldsa 0.1` (Post-Quantum Cryptography)
- `sha2 0.10`, `hex 0.4`, `base64 0.22`

**Observability**:
- `opentelemetry 0.21` (OTLP export)
- `tracing 0.1` (structured logging)
- SLO monitoring via cargo make targets

### Build System (Poka-Yoke via Makefile.toml)

**Key Targets**:
```
FAST FEEDBACK          FULL VALIDATION       QUALITY ASSURANCE
├─ check (5s)          ├─ test (30s)         ├─ speckit-verify
├─ test-unit (10s)     ├─ pre-commit (60s)   └─ timeout-check
└─ lint (60s)          └─ ci (full pipeline)

PERFORMANCE           EPIC 9 SUPPORT
├─ slo-check          └─ collision-detect
└─ bench (14 suites)
```

**Poka-Yoke Mechanisms**:
1. Timeout enforcement (SLOs: check <5s, test <30s, lint <60s)
2. Warnings as errors (RUSTFLAGS="-D warnings")
3. Quality gates (pre-commit depends on check + lint + test-unit)
4. Andon signal escalation (quick → escalation timeout on contention)
5. SLO violation detection (every target documents SLO)

### Examples Directory (30+ Examples)

Rich example ecosystem demonstrating ggen capabilities:

**Core Examples** (recently rewritten for v5.2):
- `simple-project/` - Basic project scaffolding
- `basic-template-generation/` - Template fundamentals
- `ai-template-creation/` - AI-assisted template generation
- `complete-project-generation/` - Full project workflows

**Advanced Examples**:
- `advanced-sparql-graph/` - SPARQL query patterns
- `advanced-lifecycle-demo/` - Full lifecycle management
- `comprehensive-rust-showcase/` - Rust idioms showcase
- `cli-noun-verb/` - Clap noun-verb CLI patterns
- `ggen-usage-wrapping/` - API wrappers (REST, custom CLI)

**Domain Examples**:
- `openapi/`, `graphql-schema/`, `grpc-service/` - API generation
- `database-schema/` - Schema generation
- `microservices-architecture/` - Microservice patterns
- `thesis-gen/`, `maturity-matrix-showcase/` - Document generation

### Marketplace (70+ Packages)

Pre-built templates and ontologies in `marketplace/packages/`:
- Domain templates: banking, healthcare, academic, HR, asset management
- Infrastructure: API gateway, service mesh, CDN
- AI agents: cli-copilot, context-crafter, memory-forge, reasoning-mcp
- CLI tools: chatman-cli, shacl-cli, sparql-cli, reasoner-cli

### Testing Approach (Chicago TDD + Property-Based)

**Philosophy**: State-based testing with real objects (NO mocks except London TDD in tests)

**Organization**:
- **Unit Tests**: Inline #[test] blocks in src/ (chicago-tdd-tools 1.4.0)
- **Integration Tests**: crates/*/tests/ and tests/ (proptest, assert_cmd/assert_fs)
- **E2E Tests**: ggen-e2e/tests/ with testcontainers + gVisor sandbox
- **Snapshots**: insta 1.43 for golden file comparison
- **Benchmarks**: 14 criterion suites with HTML reports

**Test Quality Features**:
- Mutation testing (cargo-mutants, ggen-test-audit)
- Assertion analysis & false-positive detection
- London TDD mocking ONLY in #[cfg(test)] blocks
- gVisor sandbox isolation for deterministic E2E tests

---

## 📝 Remember

**Specification closure is prerequisite for EPIC 9**

**EPIC 9 is default for non-trivial tasks**

**Deterministic validation replaces human review**

**Big Bang 80/20: Single-pass construction in low-entropy domains**

**Context 80% rule: Never exceed 80% utilization without compaction**

**Subagent isolation: Surgical context allocation, not full dumps**

**Always use `cargo make` - NEVER direct cargo!**

**Collect receipts, not narratives**

**TTL before code - NO implementation without .ttl specifications!**

**Markdown is generated - NEVER edit .md, edit .ttl source!**

---

## Important Reminders

- Do what has been asked; nothing more, nothing less
- NEVER create files unless absolutely necessary
- ALWAYS prefer editing an existing file
- NEVER proactively create documentation files
- Never save working files or tests to root
- TODO LISTS: Always 10+ items in ONE call, fully completed before progressing

---

## Active Technologies (2026 Edition)

**Core**: Rust 1.91.1 (MSRV 1.75+, Edition 2021), Tokio 1.47 async runtime, Oxigraph 0.5.1 (RDF + SPARQL), Tera 1.20

**Quality**: chicago-tdd-tools 1.4.0, proptest 1.8, criterion 0.7 (14 suites), testcontainers 0.25, insta 1.43

**Security**: pqcrypto-mldsa 0.1 (Post-Quantum), sha2 0.10, glob-based path protection

**Observability**: OpenTelemetry 0.21, tracing 0.1 (json), SLO monitoring

**Spec System**: RDF ontologies (TTL), SPARQL queries, Tera template generation

**CLI**: clap 4.5, clap-noun-verb 5.3.4 (noun-verb patterns), indicatif 0.18 (progress), console 0.16

**E2E**: testcontainers 0.25, gVisor sandboxing, cleanroom isolation (clnrm 0.1.0)

---

## 🔧 Key CLI Commands

### ggen init (New in v5.2)

Scaffold new projects from marketplace templates:

```bash
# Interactive mode
ggen init

# With template name
ggen init my-project --template hello-world

# List available templates
ggen init --list
```

### ggen sync

Generate code from RDF specifications:

```bash
ggen sync                    # Full generation
ggen sync --dry-run          # Preview changes
ggen sync --watch            # Watch mode
ggen sync --validate-only    # Validate spec only
```

---

**Constitutional Equation**: `spec.md = μ(feature.ttl)` | EPIC 9 is default | Receipts replace review
