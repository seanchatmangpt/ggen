# ggen - Rust Project Configuration (80/20 Edition)

## 🎯 Core Identity

**ggen**: Language-agnostic, deterministic code generation CLI. Ontologies + RDF → reproducible code projections.

**Tech Stack**: Rust (workspace), SPARC + Chicago TDD + DfLSS
**Philosophy**: Type-first thinking, zero-cost abstractions, memory safety, deterministic outputs

---

## 🚨 CRITICAL: THE VITAL FEW (20% that matters 80%)

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

**NEVER discuss unwrap/expect violations in test/benchmark code**

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

## 📁 File Organization (Never Save to Root)

```
ggen/
├── crates/*/src/   # Source code (per crate)
├── crates/*/tests/ # Integration tests
├── tests/          # Workspace tests
├── docs/           # Documentation
├── scripts/        # Build scripts
├── benches/        # Benchmarks
└── templates/      # Code generation templates
```

**Never**: Save working files, text/mds, tests to root folder

---

## 🦀 Elite Rust Mindset (Type-First Thinking)

### The Questions to Ask:
1. **"What can I express in types?"** (before runtime values)
2. **"Is this abstraction zero-cost?"** (generics yes, trait objects no)
3. **"What are the ownership semantics?"** (explicit is better)
4. **"How can I make misuse impossible?"** (type safety > runtime checks)

### 80/20 Idea Generation:
Always generate 3 ideas:
1. **First**: Solve immediate problem
2. **Second**: Go bigger (80% of problems with 20% effort) **← Sweet spot**
3. **Third**: Maximum value (type-level solutions, compile-time guarantees)

**Quality-First 80/20**: Value = quality + consistency + maintainability (not optional)

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

**Performance**:
- `cargo make slo-check` - Verify SLOs
- `cargo make bench` - Benchmarks

---

## 🚫 Prohibited Patterns (Zero Tolerance)

1. **Direct cargo commands** - ALWAYS use `cargo make`
2. **Unwrap/expect in production** - Use `Result<T, E>`
3. **Ignoring Andon signals** - Stop the line when RED
4. **Skipping timeout checks** - `cargo make timeout-check`
5. **Using --no-verify** - Never bypass git hooks
6. **Meaningless tests** - Must verify observable behavior
7. **Multiple messages** - Batch ALL operations in ONE message
8. **Saving to root** - Use proper subdirectories

---

## 🎯 SLOs (Service Level Objectives)

- First build ≤ 15s
- Incremental ≤ 2s
- RDF processing ≤ 5s for 1k+ triples
- Generation memory ≤ 100MB
- CLI scaffolding ≤ 3s end-to-end
- 100% reproducible outputs

---

## 🚀 Claude Code Task Tool (Primary Agent Execution)

**Claude Code's Task tool spawns ACTUAL agents that do work:**

```javascript
// ✅ CORRECT: Use Task tool for parallel execution
Task("Rust Coder", "Implement RDF parser with Result<T,E>", "coder")
Task("Test Engineer", "Write Chicago TDD tests with AAA pattern", "tester")
Task("Code Reviewer", "Review for type safety and Andon signals", "reviewer")
```

**MCP tools are ONLY for coordination setup:**
- `mcp__claude-flow__swarm_init` - Topology setup
- `mcp__claude-flow__agent_spawn` - Agent type definitions
- `mcp__claude-flow__task_orchestrate` - High-level planning

**KEY**: MCP coordinates strategy, Task tool executes with real agents

---

## 📋 Agent Coordination Protocol

**Every agent MUST run hooks:**

```bash
# 1️⃣ BEFORE Work
npx claude-flow@alpha hooks pre-task --description "[task]"

# 2️⃣ DURING Work
cargo make check          # Monitor Andon signals
cargo make test-unit      # Quick feedback
npx claude-flow@alpha hooks post-edit --file "[file]"

# 3️⃣ AFTER Work
cargo make test           # All tests must pass
npx claude-flow@alpha hooks post-task --task-id "[task]"
```

---

## 🚨 Definition of Done (Mandatory Validation)

**BEFORE marking complete:**

```bash
# 1. Check for compiler errors (RED signal)
cargo make check  # Must be clean

# 2. Check for test failures (RED signal)
cargo make test   # Must be 100% pass

# 3. Check for clippy warnings (YELLOW signal)
cargo make lint   # Must be clean

# 4. Verify SLOs
cargo make slo-check  # Must meet targets
```

**ONLY mark complete when ALL signals cleared**

---

## 🔗 Git Hooks System (80/20 Defect Detection)

**Pre-Commit Hook** (<5s, 62% defect detection):
- Cargo check (RED if errors)
- Format check (YELLOW, auto-fix)

**Pre-Push Hook** (<60s, 100% defect detection):
- Cargo check (RED)
- Clippy lint (RED)
- Format check (YELLOW)
- Unit tests (RED)
- Security audit (YELLOW)

**NEVER use `--no-verify`** - Defeats defect prevention

---

## 🎯 Key Associations (Mental Models)

- **Types = invariants = compile-time guarantees**
- **Zero-cost = generics/macros/const generics**
- **Performance = references/stack/minimize allocations**
- **Ownership = explicit = memory safety**
- **APIs = type-safe = ergonomic = composable**
- **Tests = observable outputs = behavior verification**
- **80/20 = second idea = sweet spot = maximum value**
- **Andon Signals = stop = fix = verify**
- **DfLSS = prevent defects AND waste from start**

---

## 📝 Remember

**Claude Flow coordinates, Claude Code creates!**

**Stop the line when Andon signals appear!**

**Always use `cargo make` - NEVER direct cargo!**

**TodoWrite always has 10+ todos in ONE call!**

**Tests verify behavior - code doesn't work if tests don't pass!**

---

# important-instruction-reminders
Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files.
Never save working files, text/mds and tests to the root folder.
TODO LISTS ARE ALWAYS 10 ITEMS OR MORE. THEY ARE ALWAYS FULLY COMPLETED BEFORE PROGRESSING TO THE NEXT TASK.
