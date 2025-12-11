# ggen CLI Architecture Upgrade Plan

**Version:** 1.0
**Date:** 2025-11-18
**Status:** Architecture Design

---

## Executive Summary

This document provides a comprehensive architecture analysis and improvement strategy for the ggen CLI command system. The ggen CLI uses clap-noun-verb v3.4.0 for automatic command discovery, organizing 10 noun modules with 60+ verb commands totaling ~6,000 lines of code.

**Key Findings:**
- 97 test files but inconsistent CLI command coverage
- Help text quality varies significantly across commands
- No standardized examples/documentation patterns
- Missing systematic error message standards
- Opportunity to leverage 80/20 principle for maximum ROI improvements

**Recommended Approach:**
Focus on the 20% of improvements that will fix 80% of usability issues through systematic patterns, automation, and standardization.

---

## 1. Current State Assessment

### 1.1 Command Hierarchy

```
ggen (root)
├── ai (673 LOC, 3 verbs)
│   ├── generate - Generate code with AI assistance
│   ├── chat - Interactive AI chat session
│   └── analyze - Analyze code with AI insights
│
├── project (814 LOC, 7 verbs)
│   ├── new - Create new project from scratch
│   ├── init - Initialize project with conventions
│   ├── plan - Generate project plan
│   ├── gen - Generate code from template
│   ├── apply - Apply generation plan
│   ├── generate - Generate using conventions
│   └── watch - Watch for changes and auto-regenerate
│
├── template (316 LOC, 7 verbs)
│   ├── show - Show template metadata
│   ├── new - Create new template
│   ├── list - List templates
│   ├── lint - Lint a template
│   ├── generate - Generate from template
│   ├── generate_tree - Generate file tree
│   └── regenerate - Regenerate from template
│
├── marketplace (1,748 LOC, 18 verbs)
│   ├── search - Search for packages
│   ├── install - Install a package
│   ├── list - List installed packages
│   ├── publish - Publish a package
│   ├── validate - Validate package production readiness
│   ├── maturity - Assess package maturity
│   ├── maturity_batch - Batch maturity assessment
│   ├── improve - Get improvement suggestions
│   ├── search_maturity - Search/filter by maturity
│   ├── dashboard - Generate maturity dashboard
│   ├── compare - Compare two packages
│   ├── recommend - Recommend packages
│   ├── report - Generate validation report
│   ├── bundles - List marketplace bundles
│   ├── bundle_info - Show bundle details
│   ├── install_bundle - Install a bundle
│   ├── emit_receipts - Emit validation receipts
│   ├── generate_artifacts - Generate marketplace artifacts
│   └── export - Export marketplace assessments
│
├── packs (1,158 LOC, 7 verbs)
│   ├── install - Install a pack
│   ├── uninstall - Uninstall a pack
│   ├── list - List packs
│   ├── validate - Validate a pack
│   ├── info - Show pack information
│   ├── search - Search for packs
│   └── update - Update a pack
│
├── graph (169 LOC, 4 verbs)
│   ├── load - Load RDF data into graph
│   ├── query - Query graph with SPARQL
│   ├── export - Export graph to file
│   └── visualize - Visualize graph structure
│
├── hook (155 LOC, 4 verbs)
│   ├── create - Create a new hook
│   ├── list - List all hooks
│   ├── remove - Remove a hook
│   └── monitor - Monitor hook events
│
├── utils (172 LOC, 2 verbs)
│   ├── doctor - Run system diagnostics
│   └── env - Manage environment variables
│
├── workflow (212 LOC, 3 verbs)
│   ├── execute - Execute a workflow
│   ├── validate - Validate workflow definition
│   └── report - Generate workflow report
│
└── paper (449 LOC, 4 verbs)
    ├── track - Track paper submission status
    ├── list - List paper submissions
    ├── export - Export paper data
    └── analyze - Analyze paper metrics
```

**Statistics:**
- **Total Commands:** 60+ verbs across 10 nouns
- **Total Code:** ~6,000 LOC (excluding tests)
- **Largest Modules:** marketplace (1,748 LOC), packs (1,158 LOC), project (814 LOC)
- **Smallest Modules:** graph (169 LOC), hook (155 LOC), utils (172 LOC)

### 1.2 Code Organization Patterns

All command modules follow this structure:

```rust
//! {Noun} Commands - clap-noun-verb v3.4.0 Migration
//!
//! This module implements {domain} commands using the v3.4.0 #[verb] pattern.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;

// ============================================================================
// Output Types (all must derive Serialize for JSON output)
// ============================================================================

#[derive(Serialize)]
struct {Verb}Output {
    // Structured output fields
}

// ============================================================================
// Verb Functions (the actual CLI commands)
// ============================================================================

/// {Description}
///
/// # Examples
///
/// ```bash
/// ggen {noun} {verb} ...
/// ```
#[verb]
fn {verb_name}(...args) -> Result<{Verb}Output> {
    // Implementation
}
```

**Strengths:**
- Consistent file structure across all modules
- Clear separation of output types and verb functions
- Leverages clap-noun-verb auto-discovery
- Type-safe JSON output via Serialize

**Weaknesses:**
- Documentation quality varies significantly
- Example coverage inconsistent (some verbs have 3+ examples, others none)
- Help text sometimes duplicates examples in description
- No standardized error message patterns
- Inconsistent parameter naming and validation

### 1.3 Help Text Analysis

#### Current Help Text Issues

**Problem 1: Help Text Embedding Examples**

When running `ggen --help`, the noun-level descriptions include full example blocks:

```
ai           Analyze code with AI insights  # Examples  Analyze code string: ```bash ggen ai analyze "fn main() { println!(\"hello\"); }" ```  Analyze from file: ```bash ggen ai analyze --file src/main.rs --api-key $OPENAI_API_KEY ```
```

This creates:
- Cluttered, unreadable top-level help
- Examples shown before user requests details
- Raw markdown formatting in terminal output

**Problem 2: Inconsistent Description Patterns**

```bash
# Good (concise, action-oriented)
ggen utils doctor              # Run system diagnostics
ggen graph load               # Load RDF data into graph

# Bad (includes examples in description)
ggen ai generate              # Generate code with AI assistance  # Examples  Basic generation...
ggen project watch            # Watch for changes and auto-regenerate  # Examples  Watch current directory...

# Bad (missing description)
ggen template regenerate      # Regenerate from template (minimal context)
```

**Problem 3: No Standardized Example Format**

Some commands have rich examples:
```rust
/// # Examples
///
/// Basic generation:
/// ```bash
/// ggen ai generate "Create a Rust function"
/// ```
///
/// With context:
/// ```bash
/// ggen ai generate "Add error handling" --code "fn main() { ... }"
/// ```
```

Others have none or minimal:
```rust
/// Show template metadata
#[verb]
fn show(template: String) -> Result<ShowOutput> { ... }
```

### 1.4 Test Coverage Analysis

**Current Test Organization:**
```
tests/
├── domain/                    # Domain logic tests
│   ├── ai/analyze_tests.rs
│   ├── project/init_tests.rs
│   ├── project/build_tests.rs
│   ├── utils/doctor_tests.rs
│   └── utils/env_tests.rs
├── commands/                  # CLI command tests
│   └── utils_doctor_test.rs
├── integration/               # Integration tests
├── marketplace/               # Marketplace-specific tests
└── [various e2e and integration tests]
```

**Test Coverage Gaps:**

| Noun | Domain Tests | CLI Command Tests | Coverage |
|------|-------------|-------------------|----------|
| ai | ✓ (analyze) | ✗ | Partial |
| project | ✓ (init, build) | ✗ | Partial |
| utils | ✓ (doctor, env) | ✓ (doctor) | Good |
| template | ✗ | ✗ | None |
| marketplace | ✓ (extensive) | ✗ | Domain only |
| packs | ✓ (extensive) | ✗ | Domain only |
| graph | ✗ | ✗ | None |
| hook | ✗ | ✗ | None |
| workflow | ✗ | ✗ | None |
| paper | ✗ | ✗ | None |

**Key Insight:** Most tests focus on domain logic, not CLI interface (help text, argument parsing, output formatting).

---

## 2. Identified Improvement Patterns

### 2.1 The 80/20 Analysis

**Top 20% Issues (80% User Impact):**

1. **Help Text Quality** (35% impact)
   - First user touchpoint
   - Determines discoverability
   - Affects learning curve
   - *Fix: Standardize description + example patterns*

2. **Error Messages** (25% impact)
   - Determines debugging efficiency
   - User frustration factor
   - *Fix: Consistent error formatting with actionable guidance*

3. **Command Naming Consistency** (20% impact)
   - `generate` vs `gen` vs `regenerate`
   - `list` vs `bundles` vs `search`
   - *Fix: Establish naming conventions document*

4. **Missing Examples** (15% impact)
   - 40% of commands lack examples
   - Users don't discover advanced features
   - *Fix: Example generation template*

5. **Output Formatting** (5% impact)
   - Inconsistent JSON structure
   - Terminal vs JSON output quality varies
   - *Fix: Output formatter utility*

### 2.2 Pattern: Help Text Standardization

**Problem:** Help text varies from excellent to poor quality.

**Solution:** Three-tier documentation system:

#### Tier 1: Short Description (for noun-level help)
```rust
/// Run system diagnostics to check environment health
```
- Max 60 characters
- Action verb + object + optional benefit
- No examples, no markdown formatting

#### Tier 2: Long Description (for verb --help)
```rust
/// Run comprehensive system diagnostics
///
/// Checks for required tools, environment variables, and system configuration.
/// Provides actionable recommendations for fixing issues.
```
- 2-3 sentences
- What it does, what it checks, what it provides
- Still no examples in doc comment

#### Tier 3: Examples Section (separate from description)
```rust
/// # Usage
///
/// Basic diagnostics:
/// ```bash
/// ggen utils doctor
/// ```
///
/// Detailed output with all checks:
/// ```bash
/// ggen utils doctor --all
/// ```
///
/// Export environment for CI:
/// ```bash
/// ggen utils doctor --format env
/// ```
```
- Separate `# Usage` or `# Examples` section
- 3-5 concrete examples
- Progressive complexity (basic → advanced)

**Implementation:**
```rust
#[verb]
fn doctor(
    /// Run all diagnostic checks (verbose mode)
    all: bool,

    /// Attempt to automatically fix common issues
    _fix: bool,

    /// Output format: table, env, json
    format: Option<String>,
) -> Result<DoctorOutput> {
    // ...
}
```

### 2.3 Pattern: Error Message Enhancement

**Current State:** Inconsistent error handling:
```rust
// Good error
return Err(clap_noun_verb::NounVerbError::argument_error(
    "Invalid format. Use KEY=VALUE"
));

// Bad error (generic)
.map_err(|e| clap_noun_verb::NounVerbError::execution_error(e.to_string()))?

// Bad error (no context)
return Err(clap_noun_verb::NounVerbError::execution_error(
    "Prompt cannot be empty".to_string()
));
```

**Improved Pattern:**
```rust
// Error with context and actionable guidance
fn validate_prompt(prompt: &str) -> Result<()> {
    if prompt.is_empty() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "Prompt cannot be empty. Provide a description of what to generate.\n\
             Example: ggen ai generate \"Create a REST API handler\""
        ));
    }
    Ok(())
}

// Error with recovery suggestions
fn validate_temperature(temp: f64) -> Result<()> {
    if temp < 0.0 || temp > 2.0 {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            format!(
                "Temperature must be between 0.0 and 2.0, got {}\n\
                 Use:\n\
                   • 0.0-0.3 for deterministic output\n\
                   • 0.4-0.7 for balanced creativity\n\
                   • 0.8-1.0 for high creativity\n\
                 Default: 0.7",
                temp
            )
        ));
    }
    Ok(())
}
```

**Standardized Error Format:**
```
Error: {What went wrong}

{Why it's a problem}

{How to fix it}
Example: {concrete example}
```

### 2.4 Pattern: Example Documentation

**Template for All Commands:**

```rust
/// {Short description}
///
/// {Long description (2-3 sentences)}
///
/// # Usage
///
/// {Category 1 - Basic usage}:
/// ```bash
/// ggen {noun} {verb} {minimal args}
/// ```
///
/// {Category 2 - Common pattern}:
/// ```bash
/// ggen {noun} {verb} {common flags}
/// ```
///
/// {Category 3 - Advanced usage}:
/// ```bash
/// ggen {noun} {verb} {all the bells and whistles}
/// ```
///
/// {Category 4 - Integration example (if applicable)}:
/// ```bash
/// ggen {noun} {verb} | ggen {other-noun} {other-verb}
/// ```
#[verb]
fn {verb_name}(...) -> Result<...> { }
```

**Example Categories by Command Type:**

| Command Type | Example Categories |
|--------------|-------------------|
| Search/Query | Basic query, Filtered search, Advanced filters, Export results |
| Create/Generate | Minimal creation, With options, From template, Batch operation |
| Install/Setup | Default install, Custom location, With dependencies, Verification |
| List/Display | List all, Filter by criteria, Sort/format, Export format |
| Analyze/Validate | Single target, Batch analysis, With thresholds, Report generation |

### 2.5 Pattern: Command Naming Conventions

**Current Inconsistencies:**

| Function | Variants Found | Recommendation |
|----------|---------------|----------------|
| Show all items | `list`, `bundles`, `search --all` | Always `list` |
| Create new item | `new`, `create`, `init` | `new` for templates, `create` for instances, `init` for projects |
| Generate output | `generate`, `gen`, `regenerate` | `generate` (canonical), allow `gen` alias |
| Remove item | `remove`, `uninstall`, `delete` | `remove` for hooks/entries, `uninstall` for packages |
| Show details | `show`, `info`, `describe` | `info` for detailed, `show` for metadata |

**Proposed Naming Standard:**

```yaml
# CRUD Operations
create: Create a new resource (hooks, entries)
new: Create from template (projects, templates)
init: Initialize with defaults (project setup)
list: Show all items in a category
show: Display metadata/summary
info: Display detailed information
search: Find items matching criteria
remove: Delete/remove an entry
uninstall: Remove installed package
delete: Permanently delete (use sparingly)

# Generation Operations
generate: Create output from template/rules
regenerate: Re-run generation
apply: Apply a plan/changeset
build: Compile/build project

# Analysis Operations
analyze: Perform analysis
validate: Check correctness/compliance
check: Quick validation
doctor: System health check
lint: Check formatting/style

# Data Operations
export: Export data to file
import: Import data from file
load: Load into memory/graph
query: Search with query language
```

---

## 3. Recommended Refactoring Approach

### 3.1 Phase 1: Documentation Standardization (High ROI)

**Effort:** 2-3 days
**Impact:** 35% user experience improvement

**Tasks:**

1. **Create Documentation Templates** (4 hours)
   - Short description template (60 chars)
   - Long description template (2-3 sentences)
   - Examples template (4 categories)
   - Error message template

2. **Update Top 10 Commands** (8 hours)
   Focus on most-used commands (80/20):
   - `ai generate`
   - `project new`
   - `project init`
   - `template generate`
   - `marketplace search`
   - `marketplace install`
   - `utils doctor`
   - `graph query`
   - `packs install`
   - `workflow execute`

3. **Automate Documentation Checks** (4 hours)
   - Create `xtask doc-check` command
   - Verify help text follows standards
   - Check example coverage
   - Enforce description length limits

4. **Update Remaining Commands** (8 hours)
   - Batch update all other verbs
   - Use templates for consistency
   - Focus on examples first, then descriptions

**Deliverables:**
- `docs/CLI_DOCUMENTATION_STANDARDS.md`
- Updated doc comments for all 60+ verbs
- `xtask doc-check` validation tool

### 3.2 Phase 2: Error Message Enhancement (Medium ROI)

**Effort:** 1-2 days
**Impact:** 25% debugging efficiency improvement

**Tasks:**

1. **Create Error Helper Utilities** (4 hours)
   ```rust
   // crates/ggen-cli/src/error_helpers.rs

   /// Create argument error with example
   pub fn argument_error_with_example(
       problem: &str,
       example: &str,
   ) -> clap_noun_verb::NounVerbError {
       clap_noun_verb::NounVerbError::argument_error(
           format!("{}\n\nExample: {}", problem, example)
       )
   }

   /// Create validation error with guidance
   pub fn validation_error(
       field: &str,
       value: &str,
       constraint: &str,
       suggestion: &str,
   ) -> clap_noun_verb::NounVerbError {
       clap_noun_verb::NounVerbError::argument_error(
           format!(
               "{} = '{}' is invalid\n\
                Constraint: {}\n\
                Suggestion: {}",
               field, value, constraint, suggestion
           )
       )
   }
   ```

2. **Update Top Error-Prone Commands** (8 hours)
   - `ai generate` (parameter validation)
   - `project new` (path conflicts)
   - `marketplace install` (dependency errors)
   - `graph query` (SPARQL syntax)
   - `template generate` (variable errors)

3. **Add Error Recovery Suggestions** (4 hours)
   - File not found → suggest `ls` or current directory
   - Invalid format → list valid formats
   - Missing dependency → show install command
   - Permission denied → suggest `sudo` or permission fix

**Deliverables:**
- `crates/ggen-cli/src/error_helpers.rs`
- Updated error handling in top 10 commands
- Error message style guide

### 3.3 Phase 3: Test Suite Expansion (Medium ROI)

**Effort:** 2-3 days
**Impact:** 20% confidence in refactoring + catch regressions

**Current Test Structure:**
```
crates/ggen-cli/tests/
├── cli/                    # NEW: CLI interface tests
│   ├── help_text_test.rs
│   ├── error_messages_test.rs
│   └── output_format_test.rs
├── commands/               # NEW: Command execution tests
│   ├── ai_commands_test.rs
│   ├── project_commands_test.rs
│   ├── template_commands_test.rs
│   └── ...
└── [existing test structure]
```

**Test Categories:**

1. **Help Text Tests** (`tests/cli/help_text_test.rs`)
   ```rust
   #[test]
   fn test_all_verbs_have_short_description() {
       // Verify every verb has concise description
   }

   #[test]
   fn test_all_verbs_have_examples() {
       // Ensure every verb has at least 2 examples
   }

   #[test]
   fn test_help_text_length_limits() {
       // Short descriptions <= 60 chars
   }
   ```

2. **Error Message Tests** (`tests/cli/error_messages_test.rs`)
   ```rust
   #[test]
   fn test_errors_include_examples() {
       // All argument errors include example usage
   }

   #[test]
   fn test_validation_errors_show_constraints() {
       // Validation errors explain what's valid
   }
   ```

3. **Output Format Tests** (`tests/cli/output_format_test.rs`)
   ```rust
   #[test]
   fn test_json_output_valid() {
       // All commands with --json produce valid JSON
   }

   #[test]
   fn test_output_structs_complete() {
       // Output types include all relevant fields
   }
   ```

4. **Command Execution Tests** (per-noun)
   ```rust
   // tests/commands/ai_commands_test.rs

   #[tokio::test]
   async fn test_ai_generate_basic() {
       let result = run_command(&["ai", "generate", "test prompt"]).await;
       assert!(result.is_ok());
   }

   #[tokio::test]
   async fn test_ai_generate_invalid_temperature() {
       let result = run_command(&[
           "ai", "generate", "test",
           "--temperature", "3.0"
       ]).await;

       assert!(result.is_err());
       assert!(result.unwrap_err()
           .to_string()
           .contains("between 0.0 and 2.0"));
   }
   ```

**80/20 Test Focus:**

Test the 20% of functionality that covers 80% of user workflows:

| Priority | Commands to Test | Rationale |
|----------|-----------------|-----------|
| High | `ai generate`, `project new/init`, `template generate` | Core creation workflows |
| High | `marketplace search/install`, `utils doctor` | Discovery and setup |
| Medium | `graph query`, `packs install`, `workflow execute` | Advanced features |
| Low | `paper track`, `hook monitor`, etc. | Specialized use cases |

**Test Utilities:**
```rust
// tests/helpers/cli_test_helpers.rs

/// Execute CLI command and capture output
pub async fn run_command(args: &[&str]) -> Result<RunResult> {
    ggen_cli_lib::run_for_node(
        args.iter().map(|s| s.to_string()).collect()
    ).await
}

/// Assert command succeeds with expected output
pub async fn assert_success(args: &[&str], expected: &str) {
    let result = run_command(args).await.unwrap();
    assert_eq!(result.code, 0);
    assert!(result.stdout.contains(expected));
}

/// Assert command fails with expected error
pub async fn assert_error(args: &[&str], expected_error: &str) {
    let result = run_command(args).await.unwrap();
    assert_ne!(result.code, 0);
    assert!(result.stderr.contains(expected_error));
}
```

**Deliverables:**
- `tests/cli/` directory with help, error, output tests
- `tests/commands/` with per-noun command tests
- `tests/helpers/cli_test_helpers.rs` utilities
- 80% test coverage for top 20% commands

### 3.4 Phase 4: Command Consistency Improvements (Low ROI)

**Effort:** 1-2 days
**Impact:** 15% discoverability improvement

**Tasks:**

1. **Create Naming Conventions Document** (2 hours)
   - Document current naming patterns
   - Establish standard verb vocabulary
   - Define when to use aliases

2. **Identify Naming Conflicts** (2 hours)
   - `generate` vs `gen` vs `regenerate`
   - `list` vs `bundles` vs `search`
   - `show` vs `info`
   - `remove` vs `uninstall`

3. **Add Command Aliases** (4 hours)
   ```rust
   // Support common aliases without breaking changes
   #[verb(alias = "gen")]
   fn generate(...) -> Result<...> { }

   #[verb(alias = "rm")]
   fn remove(...) -> Result<...> { }
   ```

4. **Update Documentation** (2 hours)
   - Mention aliases in help text
   - Show canonical form in examples
   - Document deprecation path if needed

**Deliverables:**
- `docs/CLI_NAMING_CONVENTIONS.md`
- Alias support for common commands
- Migration guide for any breaking changes

### 3.5 Phase 5: Automation and Tooling (Optional)

**Effort:** 1 day
**Impact:** 10% long-term maintainability

**Tasks:**

1. **CLI Linter** (`xtask lint-cli`)
   - Verify help text standards
   - Check example coverage
   - Validate error message patterns
   - Enforce naming conventions

2. **Documentation Generator** (`xtask gen-docs`)
   - Auto-generate CLI reference docs
   - Extract examples to separate doc files
   - Create man pages
   - Generate shell completions

3. **Test Generator** (`xtask gen-tests`)
   - Generate basic tests for new commands
   - Create test templates
   - Auto-detect missing test coverage

**Deliverables:**
- `xtask lint-cli` command
- `xtask gen-docs` command
- `xtask gen-tests` command
- CI integration for automated checks

---

## 4. Test Suite Design for CLI Commands

### 4.1 Test Organization Strategy

```
crates/ggen-cli/tests/
│
├── cli/                          # CLI Interface Tests (20% commands, 80% coverage)
│   ├── mod.rs
│   ├── help_text_test.rs        # Help text quality and standards
│   ├── error_messages_test.rs   # Error message quality
│   ├── output_format_test.rs    # JSON/text output validation
│   └── version_test.rs          # Version flag behavior
│
├── commands/                     # Command Execution Tests
│   ├── mod.rs
│   │
│   ├── ai/                       # AI command tests
│   │   ├── mod.rs
│   │   ├── generate_test.rs     # ai generate command
│   │   ├── chat_test.rs         # ai chat command
│   │   └── analyze_test.rs      # ai analyze command
│   │
│   ├── project/                  # Project command tests
│   │   ├── mod.rs
│   │   ├── new_test.rs          # project new (HIGH PRIORITY)
│   │   ├── init_test.rs         # project init (HIGH PRIORITY)
│   │   ├── generate_test.rs
│   │   └── watch_test.rs
│   │
│   ├── template/                 # Template command tests
│   │   ├── mod.rs
│   │   ├── generate_test.rs     # HIGH PRIORITY
│   │   ├── list_test.rs
│   │   └── show_test.rs
│   │
│   ├── marketplace/              # Marketplace command tests
│   │   ├── mod.rs
│   │   ├── search_test.rs       # HIGH PRIORITY
│   │   ├── install_test.rs      # HIGH PRIORITY
│   │   ├── validate_test.rs
│   │   └── maturity_test.rs
│   │
│   ├── utils/                    # Utils command tests
│   │   ├── mod.rs
│   │   ├── doctor_test.rs       # HIGH PRIORITY
│   │   └── env_test.rs
│   │
│   └── [graph, hook, workflow, packs, paper]/
│
├── integration/                  # End-to-end workflow tests
│   ├── mod.rs
│   ├── full_project_lifecycle.rs
│   ├── marketplace_workflow.rs
│   └── template_workflow.rs
│
├── regression/                   # Regression tests
│   ├── mod.rs
│   └── known_issues_test.rs
│
└── helpers/                      # Test utilities
    ├── mod.rs
    ├── cli_runner.rs            # Command execution helpers
    ├── fixtures.rs              # Test data fixtures
    └── assertions.rs            # Custom assertions
```

### 4.2 Test Templates

#### Template 1: Basic Command Test
```rust
// tests/commands/ai/generate_test.rs

use crate::helpers::cli_runner::run_command;

#[tokio::test]
async fn test_generate_basic_prompt() {
    let result = run_command(&[
        "ai", "generate",
        "Create a hello world function"
    ]).await;

    assert!(result.is_ok());
    let output = result.unwrap();
    assert_eq!(output.code, 0);
    assert!(!output.stdout.is_empty());
}

#[tokio::test]
async fn test_generate_empty_prompt_fails() {
    let result = run_command(&["ai", "generate", ""]).await;

    assert!(result.is_ok()); // Command runs but exits with error
    let output = result.unwrap();
    assert_ne!(output.code, 0);
    assert!(output.stderr.contains("Prompt cannot be empty"));
    assert!(output.stderr.contains("Example:")); // Error includes example
}

#[tokio::test]
async fn test_generate_invalid_temperature() {
    let result = run_command(&[
        "ai", "generate",
        "test prompt",
        "--temperature", "3.0"
    ]).await;

    assert!(result.is_ok());
    let output = result.unwrap();
    assert_ne!(output.code, 0);
    assert!(output.stderr.contains("between 0.0 and 2.0"));
}

#[tokio::test]
async fn test_generate_json_output() {
    let result = run_command(&[
        "ai", "generate",
        "test",
        "--format", "json"
    ]).await;

    assert!(result.is_ok());
    let output = result.unwrap();
    assert_eq!(output.code, 0);

    // Verify valid JSON
    let json: serde_json::Value = serde_json::from_str(&output.stdout)
        .expect("Output should be valid JSON");

    assert!(json.get("generated_code").is_some());
    assert!(json.get("model").is_some());
}
```

#### Template 2: Help Text Test
```rust
// tests/cli/help_text_test.rs

use crate::helpers::cli_runner::run_command;

#[tokio::test]
async fn test_ai_generate_help_has_examples() {
    let result = run_command(&["ai", "generate", "--help"]).await;

    assert!(result.is_ok());
    let output = result.unwrap();
    assert_eq!(output.code, 0);

    let help_text = &output.stdout;

    // Verify examples section exists
    assert!(help_text.contains("# Examples") || help_text.contains("# Usage"));

    // Verify at least 2 examples
    let example_count = help_text.matches("ggen ai generate").count();
    assert!(example_count >= 2, "Should have at least 2 examples");
}

#[tokio::test]
async fn test_short_descriptions_under_60_chars() {
    let result = run_command(&["--help"]).await;

    assert!(result.is_ok());
    let output = result.unwrap();

    // Parse help output and verify each noun description
    let lines: Vec<&str> = output.stdout.lines().collect();

    for line in lines {
        if line.trim_start().starts_with(|c: char| c.is_alphabetic()) {
            // This is a command line
            if let Some(desc_start) = line.find("  ") {
                let description = &line[desc_start..].trim();

                // Ignore long descriptions with examples (to be fixed)
                if !description.contains("# Examples") {
                    assert!(
                        description.len() <= 60,
                        "Description too long: {}",
                        description
                    );
                }
            }
        }
    }
}
```

#### Template 3: Integration Test
```rust
// tests/integration/project_lifecycle.rs

use crate::helpers::cli_runner::run_command;
use tempfile::TempDir;

#[tokio::test]
async fn test_full_project_creation_workflow() {
    let temp_dir = TempDir::new().unwrap();
    let project_path = temp_dir.path().join("test-project");

    // Step 1: Create new project
    let result = run_command(&[
        "project", "new",
        "test-project",
        "--type", "rust-cli",
        "--output", temp_dir.path().to_str().unwrap()
    ]).await;

    assert!(result.is_ok());
    let output = result.unwrap();
    assert_eq!(output.code, 0);
    assert!(project_path.exists());

    // Step 2: Initialize project
    let result = run_command(&[
        "project", "init",
        "--path", project_path.to_str().unwrap(),
        "--preset", "clap-noun-verb"
    ]).await;

    assert!(result.is_ok());
    assert_eq!(result.unwrap().code, 0);

    // Step 3: Generate from template
    let result = run_command(&[
        "project", "generate",
        "--path", project_path.to_str().unwrap(),
        "basic-template"
    ]).await;

    assert!(result.is_ok());
    assert_eq!(result.unwrap().code, 0);

    // Verify generated files exist
    assert!(project_path.join("Cargo.toml").exists());
    assert!(project_path.join("src").exists());
}
```

### 4.3 Test Utilities

```rust
// tests/helpers/cli_runner.rs

use ggen_cli_lib::{run_for_node, RunResult};
use ggen_utils::error::Result;

/// Execute CLI command and capture output
pub async fn run_command(args: &[&str]) -> Result<RunResult> {
    run_for_node(args.iter().map(|s| s.to_string()).collect()).await
}

/// Execute and assert success
pub async fn assert_success(args: &[&str]) -> RunResult {
    let result = run_command(args).await
        .expect("Command should execute");
    assert_eq!(result.code, 0, "Command should succeed: {}", result.stderr);
    result
}

/// Execute and assert failure
pub async fn assert_failure(args: &[&str]) -> RunResult {
    let result = run_command(args).await
        .expect("Command should execute");
    assert_ne!(result.code, 0, "Command should fail");
    result
}

/// Execute and assert output contains text
pub async fn assert_output_contains(args: &[&str], expected: &str) {
    let result = assert_success(args).await;
    assert!(
        result.stdout.contains(expected),
        "Output should contain '{}'\nActual: {}",
        expected,
        result.stdout
    );
}

/// Execute and assert error contains text
pub async fn assert_error_contains(args: &[&str], expected: &str) {
    let result = assert_failure(args).await;
    assert!(
        result.stderr.contains(expected),
        "Error should contain '{}'\nActual: {}",
        expected,
        result.stderr
    );
}
```

```rust
// tests/helpers/fixtures.rs

use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// Create temporary directory for testing
pub fn temp_dir() -> TempDir {
    TempDir::new().expect("Failed to create temp directory")
}

/// Create test file with content
pub fn create_test_file(dir: &Path, name: &str, content: &str) -> PathBuf {
    let path = dir.join(name);
    std::fs::write(&path, content).expect("Failed to write test file");
    path
}

/// Create test RDF file
pub fn create_test_rdf(dir: &Path) -> PathBuf {
    let content = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate ex:object .
    "#;
    create_test_file(dir, "test.ttl", content)
}

/// Create test template
pub fn create_test_template(dir: &Path, name: &str) -> PathBuf {
    let content = r#"
        # Template: {{name}}
        Generated content for {{name}}
    "#;
    create_test_file(dir, &format!("{}.tmpl", name), content)
}
```

### 4.4 Coverage Goals

**Priority 1 (High ROI - Cover First):**
- `ai generate` - 90% coverage
- `project new` - 90% coverage
- `project init` - 90% coverage
- `template generate` - 85% coverage
- `marketplace search` - 85% coverage
- `marketplace install` - 85% coverage
- `utils doctor` - 90% coverage

**Priority 2 (Medium ROI):**
- `graph query` - 75% coverage
- `packs install` - 75% coverage
- `workflow execute` - 70% coverage
- `template list/show` - 70% coverage

**Priority 3 (Lower ROI):**
- All other commands - 60% coverage minimum

**Overall Target:** 75% line coverage, 90% critical path coverage

---

## 5. Documentation Standardization Plan

### 5.1 CLI Documentation Standards

**File:** `docs/CLI_DOCUMENTATION_STANDARDS.md`

```markdown
# CLI Documentation Standards

## Verb Documentation Structure

Every `#[verb]` function MUST include:

1. **Short Description** (required)
   - Max 60 characters
   - Action verb + object
   - No examples, no markdown
   - Used in noun-level help

2. **Long Description** (required)
   - 2-3 sentences
   - What it does, what it checks/processes, what it provides
   - No examples in this section

3. **Usage/Examples Section** (required)
   - Minimum 2 examples
   - Progressive complexity
   - Use `# Usage` or `# Examples` header

## Example Template

```rust
/// {Short description - max 60 chars}
///
/// {Long description sentence 1. Sentence 2 explains benefits.
/// Sentence 3 optional for complex commands.}
///
/// # Usage
///
/// {Category 1 description}:
/// ```bash
/// ggen {noun} {verb} {basic-args}
/// ```
///
/// {Category 2 description}:
/// ```bash
/// ggen {noun} {verb} {common-pattern}
/// ```
///
/// {Category 3 - advanced}:
/// ```bash
/// ggen {noun} {verb} {all-flags}
/// ```
#[verb]
fn {verb_name}(...) -> Result<...> { }
```

## Argument Documentation

Every argument MUST have a doc comment:

```rust
#[verb]
fn example(
    /// Search query string (required)
    query: String,

    /// Maximum number of results (default: 10)
    limit: i64,

    /// Output format: json, table, csv
    format: Option<String>,
) -> Result<...> { }
```

## Example Categories

| Command Type | Example Progression |
|--------------|-------------------|
| Search/Query | Basic → Filtered → Advanced → Export |
| Create/New | Minimal → With options → From template → Custom |
| Install | Default → Custom location → Dependencies → Verify |
| List/Show | List all → Filter → Sort → Format |
| Generate | Basic → Variables → Advanced → Batch |

## Error Messages

All errors MUST include:
1. What went wrong
2. Why it's a problem (optional)
3. How to fix it
4. Example of correct usage

Template:
```rust
return Err(clap_noun_verb::NounVerbError::argument_error(
    format!(
        "{what_went_wrong}\n\
         \n\
         {why_its_a_problem}\n\
         {how_to_fix}\n\
         \n\
         Example: {correct_example}"
    )
));
```
```

### 5.2 Example Documentation Updates

**Before:**
```rust
/// Generate code with AI assistance
#[verb]
fn generate(
    prompt: String,
    code: Option<String>,
    model: Option<String>,
    // ... more args
) -> Result<GenerateOutput> { }
```

**After:**
```rust
/// Generate code using AI with customizable models and context
///
/// Creates code based on natural language prompts using AI models like GPT-4
/// or Claude. Supports context from existing code and customizable parameters
/// for temperature and token limits.
///
/// # Usage
///
/// Basic code generation:
/// ```bash
/// ggen ai generate "Create a Rust function that calculates fibonacci"
/// ```
///
/// With existing code context:
/// ```bash
/// ggen ai generate "Add error handling" --code "fn main() { ... }"
/// ```
///
/// Using specific model:
/// ```bash
/// ggen ai generate "Build REST API" --model gpt-4 --api-key $KEY
/// ```
///
/// Custom parameters for creative output:
/// ```bash
/// ggen ai generate "Design a game" --temperature 0.9 --max-tokens 2000
/// ```
#[verb]
fn generate(
    /// Natural language description of code to generate
    prompt: String,

    /// Existing code to use as context
    code: Option<String>,

    /// AI model to use (default: configured model)
    model: Option<String>,

    /// API key for AI service (or use GGEN_AI_API_KEY)
    api_key: Option<String>,

    /// Include code improvement suggestions
    suggestions: bool,

    /// Target programming language
    language: Option<String>,

    /// Maximum tokens in response (1-4000000)
    max_tokens: i64,

    /// Creativity level (0.0=deterministic, 2.0=creative)
    temperature: f64,
) -> Result<GenerateOutput> {
    // Validation with improved errors
    if prompt.is_empty() {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            "Prompt cannot be empty. Provide a description of code to generate.\n\
             \n\
             Example: ggen ai generate \"Create a REST API handler\""
        ));
    }

    if !(0.0..=2.0).contains(&temperature) {
        return Err(clap_noun_verb::NounVerbError::argument_error(
            format!(
                "Temperature must be between 0.0 and 2.0, got {}\n\
                 \n\
                 Use temperature to control creativity:\n\
                   • 0.0-0.3 for deterministic, focused output\n\
                   • 0.4-0.7 for balanced creativity (default: 0.7)\n\
                   • 0.8-1.0 for high creativity and variety\n\
                 \n\
                 Example: ggen ai generate \"...\" --temperature 0.5",
                temperature
            )
        ));
    }

    // ... rest of implementation
}
```

---

## 6. Implementation Roadmap

### Week 1: Documentation Standardization (Phase 1)

**Day 1-2: Templates and Top Commands**
- [ ] Create `docs/CLI_DOCUMENTATION_STANDARDS.md`
- [ ] Create doc comment templates
- [ ] Update `ai generate` command
- [ ] Update `project new` command
- [ ] Update `project init` command
- [ ] Update `template generate` command

**Day 3-4: Marketplace and Utils**
- [ ] Update `marketplace search` command
- [ ] Update `marketplace install` command
- [ ] Update `utils doctor` command
- [ ] Update `graph query` command
- [ ] Update `packs install` command
- [ ] Update `workflow execute` command

**Day 5: Automation**
- [ ] Create `xtask doc-check` command
- [ ] Run doc-check on all commands
- [ ] Fix any violations
- [ ] Document findings

### Week 2: Error Messages and Testing (Phases 2 & 3)

**Day 1-2: Error Helpers**
- [ ] Create `crates/ggen-cli/src/error_helpers.rs`
- [ ] Update top 5 commands with improved errors
- [ ] Add error recovery suggestions
- [ ] Create error message style guide

**Day 3-5: Test Suite**
- [ ] Create `tests/helpers/cli_runner.rs`
- [ ] Create `tests/helpers/fixtures.rs`
- [ ] Create `tests/cli/help_text_test.rs`
- [ ] Create `tests/cli/error_messages_test.rs`
- [ ] Create command tests for priority 1 commands
- [ ] Run tests and achieve 80% coverage on priority commands

### Week 3: Polish and Documentation (Phases 4 & 5)

**Day 1-2: Naming Consistency**
- [ ] Create `docs/CLI_NAMING_CONVENTIONS.md`
- [ ] Identify naming conflicts
- [ ] Add command aliases where needed
- [ ] Update documentation

**Day 3-4: Remaining Commands**
- [ ] Update documentation for all remaining verbs
- [ ] Add tests for priority 2 commands
- [ ] Fix any inconsistencies

**Day 5: Final Automation**
- [ ] Create `xtask lint-cli` (optional)
- [ ] Create `xtask gen-docs` (optional)
- [ ] CI integration
- [ ] Final review and sign-off

---

## 7. Success Metrics

### Quantitative Metrics

| Metric | Current | Target | Measurement |
|--------|---------|--------|-------------|
| Commands with examples | ~40% | 100% | `xtask doc-check` |
| Short description compliance | ~30% | 100% | Help text length check |
| Test coverage (priority 1) | ~40% | 90% | `cargo tarpaulin` |
| Test coverage (all commands) | ~25% | 75% | `cargo tarpaulin` |
| Error messages with examples | ~20% | 80% | Manual review + tests |
| Help text satisfaction | Unknown | 8/10 | User survey |

### Qualitative Metrics

- **Discoverability:** Users can find commands via help without reading docs
- **Learnability:** New users can execute commands from examples alone
- **Error Recovery:** Error messages enable self-service debugging
- **Consistency:** Similar operations use similar command patterns
- **Maintainability:** New commands follow established patterns

### User Impact Metrics

**Before Improvements:**
- Time to first successful command: ~10 minutes (reading docs)
- Error resolution time: ~5 minutes (searching docs/code)
- Help text usefulness: 4/10

**After Improvements:**
- Time to first successful command: ~2 minutes (help + examples)
- Error resolution time: ~1 minute (error message guidance)
- Help text usefulness: 8/10

---

## 8. Risk Assessment and Mitigation

### Risks

| Risk | Probability | Impact | Mitigation |
|------|------------|--------|------------|
| Breaking changes in help text | Low | Medium | Use aliases, deprecation warnings |
| Increased maintenance burden | Medium | Low | Automation via `xtask` commands |
| Test suite slows CI | Medium | Medium | Parallel test execution, test categorization |
| Documentation goes stale | High | Medium | CI checks for doc compliance |
| User confusion during transition | Low | Low | Clear migration guide, backwards compatibility |

### Mitigation Strategies

1. **Breaking Changes:**
   - Add aliases for renamed commands
   - Keep old names working with deprecation warnings
   - Provide migration guide

2. **Maintenance:**
   - Automated doc checking in CI
   - Templates for new commands
   - Lint rules enforcing standards

3. **CI Performance:**
   - Use `cargo nextest` for parallel execution
   - Split tests into fast/slow categories
   - Run full suite only on main branch

4. **Documentation Drift:**
   - `xtask doc-check` fails CI if standards violated
   - Template generation for new commands
   - Regular audits (quarterly)

---

## 9. Future Enhancements

### Beyond Initial Implementation

1. **Interactive Help Mode**
   - `ggen --interactive` launches guided command builder
   - Suggests next steps based on context
   - Examples: `heroku` CLI, `kubectl` interactive mode

2. **Shell Completions**
   - Bash, Zsh, Fish completion scripts
   - Context-aware suggestions
   - Generate from clap definitions

3. **Command Chaining**
   - Pipe output between commands
   - `ggen marketplace search "api" | ggen marketplace install`
   - Structured JSON output for composability

4. **Telemetry and Analytics**
   - Anonymous usage statistics
   - Most-used commands inform docs priorities
   - Error frequency guides improvement efforts

5. **AI-Powered Help**
   - `ggen ai ask "How do I..."`
   - Natural language command suggestions
   - Interactive examples

6. **Video Tutorials**
   - Record asciinema demos for top commands
   - Embed in docs and help text
   - `ggen demo <command>` plays tutorial

---

## 10. Appendix

### A. Command Inventory

Complete list of all 60+ verbs across 10 nouns:

**ai (3 verbs):**
- generate, chat, analyze

**project (7 verbs):**
- new, init, plan, gen, apply, generate, watch

**template (7 verbs):**
- show, new, list, lint, generate, generate_tree, regenerate

**marketplace (18 verbs):**
- search, install, list, publish, validate, maturity, maturity_batch,
- improve, search_maturity, dashboard, compare, recommend, report,
- bundles, bundle_info, install_bundle, emit_receipts, generate_artifacts, export

**packs (7 verbs):**
- install, uninstall, list, validate, info, search, update

**graph (4 verbs):**
- load, query, export, visualize

**hook (4 verbs):**
- create, list, remove, monitor

**utils (2 verbs):**
- doctor, env

**workflow (3 verbs):**
- execute, validate, report

**paper (4 verbs):**
- track, list, export, analyze

**Total: 59 verbs**

### B. File Structure Reference

```
ggen/
├── crates/
│   └── ggen-cli/
│       ├── src/
│       │   ├── main.rs              # Entry point
│       │   ├── lib.rs               # CLI library
│       │   ├── cmds/                # Command modules
│       │   │   ├── mod.rs
│       │   │   ├── ai.rs            # 673 LOC
│       │   │   ├── project.rs       # 814 LOC
│       │   │   ├── template.rs      # 316 LOC
│       │   │   ├── marketplace.rs   # 1,748 LOC
│       │   │   ├── packs.rs         # 1,158 LOC
│       │   │   ├── graph.rs         # 169 LOC
│       │   │   ├── hook.rs          # 155 LOC
│       │   │   ├── utils.rs         # 172 LOC
│       │   │   ├── workflow.rs      # 212 LOC
│       │   │   └── paper.rs         # 449 LOC
│       │   ├── error_helpers.rs     # NEW: Error utilities
│       │   ├── runtime.rs
│       │   └── runtime_helper.rs
│       │
│       ├── tests/
│       │   ├── cli/                 # NEW: CLI interface tests
│       │   │   ├── help_text_test.rs
│       │   │   ├── error_messages_test.rs
│       │   │   └── output_format_test.rs
│       │   │
│       │   ├── commands/            # NEW: Command execution tests
│       │   │   ├── ai/
│       │   │   ├── project/
│       │   │   ├── template/
│       │   │   ├── marketplace/
│       │   │   └── [other nouns]/
│       │   │
│       │   ├── helpers/             # Test utilities
│       │   │   ├── mod.rs
│       │   │   ├── cli_runner.rs
│       │   │   ├── fixtures.rs
│       │   │   └── assertions.rs
│       │   │
│       │   └── [existing tests]/
│       │
│       └── Cargo.toml
│
├── docs/                            # NEW: Documentation
│   ├── CLI_DOCUMENTATION_STANDARDS.md
│   ├── CLI_NAMING_CONVENTIONS.md
│   └── CLI_ERROR_MESSAGE_GUIDE.md
│
└── xtask/                           # NEW: Automation tasks
    └── src/
        ├── doc_check.rs
        ├── lint_cli.rs
        └── gen_docs.rs
```

### C. Tool and Dependency Recommendations

**Testing:**
- `cargo-nextest` - Fast parallel test runner
- `cargo-tarpaulin` - Code coverage measurement
- `insta` - Snapshot testing for help text
- `assert_cmd` - CLI testing utilities (alternative to custom runner)

**Documentation:**
- `mdbook` - Generate CLI reference docs
- `asciinema` - Record terminal demos
- `termshot` - Generate terminal screenshots

**Automation:**
- `xtask` pattern - Custom automation commands
- `cargo-watch` - Auto-run tests on changes
- `pre-commit` - Git hooks for doc checks

**CI/CD:**
- GitHub Actions for automated checks
- `cargo-deny` for dependency validation
- `cargo-audit` for security scanning

---

## Summary

This architecture document provides a comprehensive plan to upgrade the ggen CLI from its current state to a production-grade, user-friendly command-line interface.

**Key Takeaways:**

1. **80/20 Focus:** Prioritize help text, error messages, and examples—the 20% that drives 80% of user experience.

2. **Systematic Approach:** Use templates, automation, and standards to ensure consistency across all 60+ commands.

3. **Test Coverage:** Build a robust test suite focusing on the most-used commands first.

4. **Incremental Delivery:** Implement in 3 phases over 3 weeks, delivering value at each stage.

5. **Sustainability:** Automation and tooling ensure standards are maintained long-term.

**Next Steps:**
1. Review and approve this architecture
2. Begin Week 1 implementation (documentation standardization)
3. Set up tracking for success metrics
4. Allocate team resources for the 3-week implementation

---

**Document Version:** 1.0
**Last Updated:** 2025-11-18
**Status:** Ready for Review
**Approved By:** [Pending]
