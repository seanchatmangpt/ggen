# ggen v2 Automation Strategy

**Created**: 2025-11-01
**Purpose**: Automate command generation, testing, and documentation
**Goal**: 80% reduction in manual work for remaining 47 commands

---

## Overview

**Current Manual Effort per Command**: 3-4 hours
- Write command wrapper (30 min)
- Write domain logic (1 hour)
- Write tests (1 hour)
- Write documentation (30 min)
- Integration testing (30 min)

**Target Automated Effort per Command**: 30-60 minutes
- Generate scaffolding (automated)
- Fill in business logic (manual)
- Auto-generate tests (automated)
- Auto-generate docs (automated)

**Savings**: 67-75% time reduction

---

## 1. Command Scaffolding Generator

### Tool: `ggen internal gen-command`

**Purpose**: Auto-generate command boilerplate

**Input**: YAML specification
```yaml
# commands/specs/marketplace-verify.yml
command:
  name: marketplace verify
  domain: marketplace
  description: Verify template integrity and signatures

args:
  - name: template_name
    type: String
    required: true
    help: Template name to verify

  - name: signature
    type: PathBuf
    required: false
    help: Path to signature file

flags:
  - name: offline
    short: o
    help: Verify without network access

  - name: verbose
    short: v
    help: Show verbose output

returns:
  type: VerifyResult
  fields:
    - valid: bool
    - signature_ok: bool
    - checksum_ok: bool
    - warnings: Vec<String>
```

**Output**: 3 files generated
```
cli/src/commands/marketplace/verify.rs  (90 lines)
cli/src/domain/marketplace/verify.rs    (120 lines)
cli/tests/marketplace_verify_test.rs    (180 lines)
```

**Generated Command Wrapper** (`cli/src/commands/marketplace/verify.rs`):
```rust
//! Marketplace verify command - auto-generated

use clap::Args;
use crate::domain::marketplace::verify;
use crate::runtime;

#[derive(Args)]
pub struct VerifyArgs {
    /// Template name to verify
    #[arg(value_name = "TEMPLATE")]
    template_name: String,

    /// Path to signature file
    #[arg(short, long)]
    signature: Option<PathBuf>,

    /// Verify without network access
    #[arg(short, long)]
    offline: bool,

    /// Show verbose output
    #[arg(short, long)]
    verbose: bool,
}

pub fn run(args: VerifyArgs) -> anyhow::Result<()> {
    runtime::execute(async move {
        let result = verify::verify_template(
            &args.template_name,
            args.signature.as_ref(),
            args.offline,
        ).await?;

        if args.verbose {
            println!("{:#?}", result);
        } else {
            println!("Valid: {}", result.valid);
            if !result.warnings.is_empty() {
                println!("Warnings:");
                for w in &result.warnings {
                    println!("  - {}", w);
                }
            }
        }

        Ok(())
    })
}
```

**Generated Domain Logic** (`cli/src/domain/marketplace/verify.rs`):
```rust
//! Marketplace verify domain logic - auto-generated

use std::path::Path;
use anyhow::Result;

#[derive(Debug)]
pub struct VerifyResult {
    pub valid: bool,
    pub signature_ok: bool,
    pub checksum_ok: bool,
    pub warnings: Vec<String>,
}

/// Verify template integrity and signatures
pub async fn verify_template(
    template_name: &str,
    signature: Option<&Path>,
    offline: bool,
) -> Result<VerifyResult> {
    // TODO: Implement verification logic
    // 1. Load template metadata
    // 2. Verify checksum
    // 3. Verify signature (if provided)
    // 4. Check for tampering
    // 5. Collect warnings

    Ok(VerifyResult {
        valid: true,
        signature_ok: signature.is_some(),
        checksum_ok: true,
        warnings: vec![],
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_verify_valid_template() {
        let result = verify_template("test-template", None, false)
            .await
            .unwrap();
        assert!(result.valid);
    }

    #[tokio::test]
    async fn test_verify_with_signature() {
        // TODO: Test signature verification
    }

    #[tokio::test]
    async fn test_verify_offline() {
        // TODO: Test offline verification
    }
}
```

**Generated Tests** (`cli/tests/marketplace_verify_test.rs`):
```rust
//! Integration tests for marketplace verify - auto-generated

use assert_cmd::Command;
use predicates::prelude::*;

#[test]
fn test_verify_requires_template_name() {
    Command::cargo_bin("ggen")
        .unwrap()
        .args(&["marketplace", "verify"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("required"));
}

#[test]
fn test_verify_valid_template() {
    Command::cargo_bin("ggen")
        .unwrap()
        .args(&["marketplace", "verify", "test-template"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Valid: true"));
}

#[test]
fn test_verify_offline_flag() {
    Command::cargo_bin("ggen")
        .unwrap()
        .args(&["marketplace", "verify", "test-template", "--offline"])
        .assert()
        .success();
}

#[test]
fn test_verify_verbose_output() {
    Command::cargo_bin("ggen")
        .unwrap()
        .args(&["marketplace", "verify", "test-template", "--verbose"])
        .assert()
        .success()
        .stdout(predicate::str::contains("VerifyResult"));
}
```

### Implementation

**Phase 1**: Build generator (4 hours)
```rust
// tools/gen-command/src/main.rs
fn main() {
    let spec = load_yaml_spec("commands/specs/*.yml")?;

    for cmd_spec in specs {
        let command_file = generate_command_wrapper(&cmd_spec);
        let domain_file = generate_domain_logic(&cmd_spec);
        let test_file = generate_integration_tests(&cmd_spec);

        write_file(&command_file)?;
        write_file(&domain_file)?;
        write_file(&test_file)?;
    }
}
```

**Phase 2**: Create 47 YAML specs (8 hours)
- 1 spec per remaining command
- 10 minutes per spec
- Total: 47 × 10min = 7.8 hours

**Phase 3**: Generate all commands (1 hour)
```bash
cargo run --bin gen-command -- \
    --specs commands/specs/*.yml \
    --output cli/src
```

**Total Time**: 13 hours (vs 188 hours manual)

---

## 2. Test Generation Automation

### Tool: `ggen internal gen-tests`

**Purpose**: Auto-generate comprehensive test suites

**Input**: Existing command implementation

**Output**: 4 test files
```
tests/unit/marketplace_verify_test.rs       # Unit tests
tests/integration/marketplace_verify_test.rs # Integration tests
tests/performance/marketplace_verify_bench.rs # Benchmarks
tests/security/marketplace_verify_fuzz.rs    # Fuzzing
```

**Auto-Generated Test Cases**:
```rust
// tests/unit/marketplace_verify_test.rs
#[test]
fn test_args_parsing() { /* ... */ }

#[test]
fn test_required_args() { /* ... */ }

#[test]
fn test_optional_args() { /* ... */ }

#[test]
fn test_flag_combinations() { /* ... */ }

#[test]
fn test_error_handling() { /* ... */ }
```

**Usage**:
```bash
cargo run --bin gen-tests -- \
    --command cli/src/commands/marketplace/verify.rs \
    --output tests/
```

---

## 3. Documentation Generation

### Tool: `ggen internal gen-docs`

**Purpose**: Auto-generate documentation from code

**Input**: Command implementations + doc comments

**Output**: Multiple doc formats
```
docs/commands/marketplace/verify.md  # Markdown docs
docs/man/ggen-marketplace-verify.1   # Man page
docs/web/marketplace/verify.html     # Website docs
```

**Auto-Generated Markdown**:
```markdown
# ggen marketplace verify

Verify template integrity and signatures.

## Usage

```bash
ggen marketplace verify <TEMPLATE> [OPTIONS]
```

## Arguments

- `<TEMPLATE>` - Template name to verify (required)

## Options

- `-s, --signature <FILE>` - Path to signature file
- `-o, --offline` - Verify without network access
- `-v, --verbose` - Show verbose output
- `-h, --help` - Print help

## Examples

Verify a template:
```bash
ggen marketplace verify my-template
```

Verify with signature:
```bash
ggen marketplace verify my-template --signature signature.asc
```

Offline verification:
```bash
ggen marketplace verify my-template --offline
```

## Exit Codes

- `0` - Template verified successfully
- `1` - Verification failed
- `2` - Invalid arguments
```

**Implementation**:
```rust
// tools/gen-docs/src/main.rs
fn generate_docs(command: &Command) -> Docs {
    Docs {
        markdown: generate_markdown(&command),
        man_page: generate_man_page(&command),
        html: generate_html(&command),
    }
}
```

---

## 4. CI/CD Automation

### Continuous Command Validation

**GitHub Actions Workflow**:
```yaml
# .github/workflows/command-completeness.yml
name: Command Completeness Check

on: [push, pull_request]

jobs:
  check-commands:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Count Commands
        run: |
          EXPECTED=67
          COMMAND_FILES=$(find cli/src/commands -name "*.rs" -not -name "mod.rs" | wc -l)
          DOMAIN_FILES=$(find cli/src/domain -name "*.rs" -not -name "mod.rs" | wc -l)

          echo "Commands: $COMMAND_FILES/$EXPECTED"
          echo "Domain: $DOMAIN_FILES/$EXPECTED"

          if [ $COMMAND_FILES -lt $EXPECTED ]; then
            echo "❌ Missing command files"
            exit 1
          fi

          if [ $DOMAIN_FILES -lt $EXPECTED ]; then
            echo "❌ Missing domain files"
            exit 1
          fi

      - name: Verify Tests Exist
        run: |
          for cmd in cli/src/commands/**/*.rs; do
            TEST_FILE="tests/$(basename $cmd)"
            if [ ! -f "$TEST_FILE" ]; then
              echo "❌ Missing test for $cmd"
              exit 1
            fi
          done

      - name: Check Documentation
        run: |
          for cmd in cli/src/commands/**/*.rs; do
            DOC_FILE="docs/commands/$(basename $cmd .rs).md"
            if [ ! -f "$DOC_FILE" ]; then
              echo "❌ Missing docs for $cmd"
              exit 1
            fi
          done

      - name: Run All Tests
        run: cargo test --workspace --all-features

      - name: Check Coverage
        run: |
          cargo tarpaulin --out Xml
          COVERAGE=$(grep line-rate coverage.xml | head -1 | grep -oP '\d+\.\d+')
          if (( $(echo "$COVERAGE < 0.80" | bc -l) )); then
            echo "❌ Coverage $COVERAGE < 80%"
            exit 1
          fi
```

### Auto-Release on Milestone

**Trigger Release When All Commands Complete**:
```yaml
# .github/workflows/auto-release.yml
name: Auto Release

on:
  push:
    branches: [master]

jobs:
  check-completeness:
    runs-on: ubuntu-latest
    outputs:
      ready: ${{ steps.check.outputs.ready }}
    steps:
      - id: check
        run: |
          COMMAND_COUNT=$(find cli/src/commands -name "*.rs" -not -name "mod.rs" | wc -l)
          if [ $COMMAND_COUNT -ge 67 ]; then
            echo "ready=true" >> $GITHUB_OUTPUT
          else
            echo "ready=false" >> $GITHUB_OUTPUT
          fi

  release:
    needs: check-completeness
    if: needs.check-completeness.outputs.ready == 'true'
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - name: Create Release
        run: |
          VERSION=$(cargo metadata --no-deps --format-version 1 | jq -r '.packages[0].version')
          gh release create "v$VERSION" \
            --title "ggen v$VERSION - Complete v2 Migration" \
            --notes "All 67+ commands implemented!"
```

---

## 5. Batch Command Implementation

### Strategy: Generate All Remaining Commands at Once

**Step 1**: Create 47 YAML specs (8 hours)
```bash
# Generate specs for all remaining commands
mkdir -p commands/specs

# Marketplace (5 commands)
cat > commands/specs/marketplace-sync.yml << EOF
command:
  name: marketplace sync
  domain: marketplace
  description: Sync local templates with registry
# ... (full spec)
EOF

# Repeat for all 47 commands
```

**Step 2**: Generate all scaffolding (5 minutes)
```bash
cargo run --bin gen-command -- \
    --specs commands/specs/*.yml \
    --output cli/src \
    --tests tests/ \
    --docs docs/
```

**Step 3**: Implement business logic in parallel (20 hours)
- Use Claude Code Task tool to spawn 5 agents
- Each agent implements 9-10 commands
- Parallel execution: 20 hours → 4 hours wall time

**Step 4**: Generate tests (5 minutes)
```bash
cargo run --bin gen-tests -- \
    --commands cli/src/commands/**/*.rs \
    --output tests/
```

**Step 5**: Generate docs (5 minutes)
```bash
cargo run --bin gen-docs -- \
    --commands cli/src/commands/**/*.rs \
    --output docs/
```

**Total Time**: 8h (specs) + 4h (implementation) + 1h (automation) = **13 hours**

**vs Manual**: 47 commands × 4 hours = **188 hours**

**Savings**: 93% reduction (175 hours saved)

---

## 6. Parallel Agent Implementation

### Claude Code Task Pattern

**Spawn 5 agents in parallel, each handling 9-10 commands**:

```javascript
[Single Message - Parallel Command Implementation]:
  Task("Marketplace Agent", `
    Implement these 5 marketplace commands:
    1. marketplace sync (sync local with registry)
    2. marketplace verify (verify integrity)
    3. marketplace stats (usage statistics)
    4. marketplace author (show author info)
    5. marketplace trending (show trending templates)

    Scaffolding generated in:
    - cli/src/commands/marketplace/*.rs (command wrappers)
    - cli/src/domain/marketplace/*.rs (domain logic)
    - tests/marketplace_*_test.rs (integration tests)

    Fill in TODO sections with real implementations.
    Run tests to verify: cargo test marketplace
  `, "backend-dev")

  Task("AI Commands Agent", `
    Implement these 8 AI commands:
    1. ai gen (generate code from prompt)
    2. ai analyze (analyze code quality)
    3. ai plan (plan implementation)
    4. ai refactor (suggest refactorings)
    5. ai template create (create template from prompt)
    6. ai template optimize (optimize template)
    7. ai template validate (AI-powered validation)
    8. ai template suggest (suggest improvements)

    Use OpenAI/Anthropic APIs with caching.
    Run tests to verify: cargo test ai
  `, "backend-dev")

  Task("Graph Commands Agent", `
    Implement these 9 graph commands:
    1. graph load (load graph from file)
    2. graph query (query with Cypher)
    3. graph visualize (GraphViz output)
    4. graph export (JSON/GraphML/DOT)
    5. graph merge (merge multiple graphs)
    6. graph workflow create
    7. graph workflow run
    8. graph workflow list
    9. graph workflow validate

    Use petgraph + graphviz-rust.
    Run tests to verify: cargo test graph
  `, "backend-dev")

  Task("Shell Commands Agent", `
    Implement these 8 shell commands:
    1. shell init (initialize shell integration)
    2. shell aliases (generate aliases)
    3. shell functions (generate functions)
    4. shell completions (advanced completions)
    5. env set (set environment variable)
    6. env get (get environment variable)
    7. env list (list all variables)
    8. env export (export environment)

    Support bash, zsh, fish, PowerShell.
    Run tests to verify: cargo test shell
  `, "backend-dev")

  Task("Project/CI Commands Agent", `
    Implement these 12 commands:

    Project (6):
    1. project status
    2. project test
    3. project deploy
    4. project watch
    5. project clean
    6. project dependencies

    CI (6):
    1. ci test
    2. ci lint
    3. ci build
    4. ci deploy
    5. ci rollback
    6. ci status

    Run tests to verify: cargo test project && cargo test ci
  `, "backend-dev")

  TodoWrite { todos: [
    {content: "Generate 47 YAML specs", status: "in_progress", activeForm: "Generating YAML specs"},
    {content: "Run command generator", status: "pending", activeForm: "Running command generator"},
    {content: "Spawn 5 parallel agents", status: "pending", activeForm: "Spawning parallel agents"},
    {content: "Implement marketplace commands", status: "pending", activeForm: "Implementing marketplace commands"},
    {content: "Implement AI commands", status: "pending", activeForm: "Implementing AI commands"},
    {content: "Implement graph commands", status: "pending", activeForm: "Implementing graph commands"},
    {content: "Implement shell commands", status: "pending", activeForm: "Implementing shell commands"},
    {content: "Implement project/CI commands", status: "pending", activeForm: "Implementing project/CI commands"},
    {content: "Run integration tests", status: "pending", activeForm: "Running integration tests"},
    {content: "Generate documentation", status: "pending", activeForm: "Generating documentation"}
  ]}
```

**Result**: All 47 commands implemented in **4 hours wall time** (vs 188 hours sequential)

---

## 7. Quality Automation

### Auto-Formatting
```bash
# Format all generated code
cargo fmt --all
```

### Auto-Linting
```bash
# Fix all clippy warnings automatically
cargo clippy --fix --allow-dirty --allow-staged
```

### Auto-Testing
```bash
# Run all tests with retries
cargo test --workspace --all-features -- --test-threads=1
```

### Auto-Benchmarking
```bash
# Benchmark all new commands
cargo bench --features bench
```

---

## Timeline with Automation

### Week 1: Build Automation Tools (16 hours)
- Command generator (4h)
- Test generator (4h)
- Doc generator (4h)
- CI/CD pipelines (4h)

### Week 2: Generate Specs + Scaffolding (8 hours)
- Write 47 YAML specs (8h)
- Run generators (automated - 15 min)

### Week 2-3: Parallel Implementation (4 hours wall time)
- Spawn 5 agents via Claude Code
- Each implements 9-10 commands
- 20 hours work → 4 hours wall time

### Week 3: Quality + Documentation (8 hours)
- Run all tests (automated)
- Generate docs (automated)
- Performance benchmarks (automated)
- Security audit (manual - 4h)
- Release notes (manual - 4h)

**Total**: 36 hours (vs 188 hours manual)
**Savings**: 81% reduction (152 hours saved)

---

## ROI Analysis

### Investment
- Automation tools: 16 hours
- YAML specs: 8 hours
- CI/CD setup: 4 hours
- **Total**: 28 hours

### Returns
- Manual time: 188 hours
- Automated time: 36 hours
- **Savings**: 152 hours

### ROI
- Time saved: 152 hours
- Investment: 28 hours
- **ROI**: 543% (5.4x return)

### Break-Even
- Automation pays for itself after **10 commands**
- Remaining 37 commands are pure savings

---

## Conclusion

**Automation Strategy**: Build once, use 47 times

**Key Insight**: Scaffolding is 80% identical across commands

**Critical Success Factor**: YAML specs must be detailed

**Recommendation**: **Invest 28 hours in automation to save 152 hours**

**Next Step**: Build command generator this week

---

**Document Version**: 1.0
**Last Updated**: 2025-11-01
**Owner**: ggen Core Team
**Status**: Proposal (awaiting approval)
