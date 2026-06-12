# Ggen Usability & Architecture Audit Report
**Date**: June 11, 2026  
**Workspace Version**: 26.6.9  
**Auditor**: Teamwork Usability Agent (teamwork_preview_worker)

---

## 1. Executive Summary & Overview

This report provides a comprehensive synthesis of the developer onboarding, local environment setup, CLI usability, documentation accuracy, and codebase architecture of the `ggen` RDF-driven code generator framework. Over the course of three independent explorer usability audits, the system's developer experience (DX) was thoroughly analyzed through direct environment setup, CLI execution testing, documentation verification, and structural codebase review.

### Key Insights & Findings
1. **Onboarding & Compiling Bottlenecks**: macOS users face an undocumented dependency on GNU coreutils (specifically the `timeout` command). Furthermore, running the build verification task (`cargo make check`) on clean checkouts consistently times out (exit code `124`) because heavy C++ dependencies (e.g., `oxrocksdb-sys`) require longer than the hardcoded 60-second limit to compile from scratch.
2. **Scaffolding & Validation Friction**: The `ggen init` command scaffolds a configuration manifest (`ggen.toml`) that is incompatible with the validator in `ggen sync`. Out of the box, `ggen sync` rejects the generated file due to strict parsing checks (`#[serde(deny_unknown_fields)]`) and schema mismatches between internal configuration types.
3. **CLI Argument Casing & Error Handling Errors**: The CLI parser does not support POSIX-standard kebab-case options (like `--skip-hooks`), requiring snake_case options (like `--skip_hooks true`) with explicit values. Moreover, application errors that output JSON fail to report failures to the shell, returning exit code `0` and breaking CI/CD orchestration.
4. **Documentation Drift & Outdated Walkthroughs**: Core user guides reference outdated versions (e.g., `26.5.28` vs the actual `26.6.9`), instruct users to execute non-existent or deleted commands (such as `ggen ai` and `ggen template`), and leak internal Tera templating comments in reference documents.
5. **Code Portability Violations**: The workspace root `Cargo.toml` contains a hardcoded absolute directory path (`/Users/sac/wasm4pm-compat`) for a local dependency, preventing clean compilation on any system other than the original developer's machine.
6. **Monolithic Architecture**: The core package (`ggen-core`) suffers from massive dependency bloat, incorporating heavy web servers, HTTP clients, and duplicate logging backends (`slog` and `tracing`), causing slow clean compilations and long feedback loops.

---

## 2. Installation & Local Environment Setup Audit

Setting up the local environment revealed several critical developer friction points that prevent smooth "zero-friction" onboarding.

### Onboarding Steps Attempted
The onboarding process was initiated on a macOS system with the following baseline environment:
- **Rust Toolchain**: `rustup` automatically resolves the compiler version to `1.97.0-nightly (a5c825cd8 2026-04-14)` based on `/Users/sac/ggen/rust-toolchain.toml`, which specifies the `nightly-2026-04-15` channel.
- **Tools Installed**: `cargo-make` was installed to run tasks defined in `Makefile.toml`.

### GNU Coreutils & macOS Timeout Dependency
The project’s task runner commands depend directly on the GNU `timeout` utility. 
* **The Problem**: The standard macOS shell does not include `timeout`. When running `cargo make check`, the task fails immediately if GNU coreutils is missing.
* **Workaround**: macOS developers must run `brew install coreutils` to place `timeout` (often aliased or linked) into their executable path. There is no automated fallback or documentation warning for macOS users.

### Makefile 60s Check Timeout (Exit Code 124)
The `check` task in `Makefile.toml` is defined as:
```toml
[tasks.check]
description = "Check code without building (60s timeout for 30-crate workspace + lock contention)"
workspace = false
command = "timeout"
args = ["60s", "cargo", "check", "--workspace"]
```
* **The Problem**: On a clean clone with an empty target cache, running `cargo make check` is guaranteed to fail. The project relies on deep dependencies like `oxrocksdb-sys` (which builds the RocksDB C++ engine) and `bindgen`. Compiling these crates from scratch takes far longer than 60 seconds on standard developer laptops.
* **The Output**: The command terminates mid-compile with exit code `124` (the standard exit status returned by `timeout` when a command is aborted due to duration limits).
* **Workaround**: Developers must bypass the makefile and run a manual `cargo check --workspace` to populate the target cache before running `cargo make check`.

### Interactive Prompts in Startup Script
The script `scripts/startup.sh` (run during setup) includes 5 separate interactive questions requesting user confirmation:
```bash
read -r q1
read -r q2
read -r q3
read -r q4_confirm
read -r q5_confirm
```
* **The Problem**: If questions 1 through 3 are not answered with the exact string `"yes"`, the script terminates with `exit 1`. The presence of interactive `read` blocks prevents the project from being bootstrapped or verified in headless CI/CD runners (like GitHub Actions or GitLab CI) without complex input piping or mocking.

### Target Directory Lock Contention
When running multiple verification commands or concurrent agent tasks, the Rust compiler locks the `target/` directory:
```
Blocking waiting for file lock on build directory
```
This lock contention causes parallel checks to stall, directly contributing to tasks exceeding the 60-second limit and failing with exit code 124.

---

## 3. CLI Interface & Error Handling Evaluation

A detailed inspection of the command-line interface (CLI) exposed severe usability discrepancies and error-reporting bugs.

### Parameter Casing Mismatch
The CLI router is constructed using the `clap-noun-verb` macro. Because this macro processes Rust function parameter names directly without standard POSIX command-line option normalization:
* **The Problem**: Options are exposed using **snake_case** rather than POSIX-standard **kebab-case**. Passing `--skip-hooks` or `--dry-run` yields a parser error:
  ```
  error: unexpected argument '--skip-hooks' found
    tip: a similar argument exists: '--skip_hooks'
  Usage: ggen init --path <PATH> --skip_hooks <SKIP_HOOKS>
  ```
* **Documentation Discrepancy**: The CLI's manual help text (written in the function's markdown doc comments) describes these parameters using kebab-case (e.g. `--skip-hooks`). However, the auto-generated usage block below it displays snake_case fields that require values (e.g. `--skip_hooks <SKIP_HOOKS>`), forcing developers to pass `--skip_hooks true` instead of a simple boolean flag.

### Scaffolded Manifest (`ggen.toml`) Validation Failures
When running `ggen init`, the CLI generates a default `ggen.toml` configuration file. However, executing `ggen sync` immediately afterward on this clean scaffold fails:
```
unknown field `standard_only`, expected one of `source`, `imports`, `base_iri`, `prefixes`
```
* **Why it occurs**: The CLI's init command hardcodes a template for `ggen.toml` containing properties such as:
  - `ontology.standard_only = true`
  - `project.authors = ["ggen init"]`
  - `project.license = "MIT"`
  - Sections like `[sync]`, `[rdf]`, `[templates]`, and `[output]`.
* **The Parser Bug**: The sync subcommand parses `ggen.toml` into a `GgenManifest` struct (located in `crates/ggen-core/src/manifest/types.rs`), which is decorated with `#[serde(deny_unknown_fields)]`. Since `GgenManifest` does not define these metadata fields and sections (which only exist in a separate config crate), the parser rejects the out-of-the-box scaffolded file, preventing successful project syncs without manual text editing.

### Zero Exit Codes on Application Errors
When calling subcommands (like `ggen init` or `ggen sync`) with the JSON output format requested, application-level errors do not propagate to the OS.
* **The Problem**: If `ggen init` is run in an already initialized directory, or if `ggen sync` is run with a missing manifest file, the CLI outputs a JSON block with `"status": "error"`, but terminates with exit code `0`.
* **Why it occurs**: The CLI router catches these errors and serializes them into a successful `VerbResult::Ok(...)` JSON response wrapper instead of returning `VerbResult::Err(...)`, rendering wrapper scripts and automated pipelines blind to execution failures.

### RDF Syntax Errors in `ontology.ttl`
The root specification file `ontology.ttl` contains syntax errors that break standard RDF parsers:
```turtle
a2a:TestScenario1 a ggen:TestScenario ;
  ggen:name "Domain Class Validation" ;
  ggen:description "Validate that all domain classes are properly defined with inheritance" ;
  ggen:steps [
    "Load A2A ontology file",
    "Check existence of 7 domain module classes",
    "Verify inheritance from abstract base classes",
    "Validate property definitions for each class"
  ] ;
  ggen:expectedResult "All classes defined, no inheritance violations" .
```
* **The Problem**: In RDF Turtle syntax, square brackets `[ ... ]` represent a blank node and must contain predicate-object pairs. A comma-separated list of strings inside brackets is invalid. Standard Turtle arrays (RDF Collections) must be enclosed in parentheses `( ... )`. Running `ggen graph validate` on this file produces a parser error:
  ```
  Parser error at line 100 between columns 5 and 29: "Load A2A ontology file" is not a valid predicate
  ```

---

## 4. Documentation & Walkthrough Verification

A review of the project documentation against the actual codebase state exposed significant version drift and outdated command instructions.

### Version Drift Across Files
There is a lack of synchronization regarding the active version of the software:
* **Actual Version**: Root `Cargo.toml` specifies `version = "26.6.9"`.
* **README.md**: Mentions version `26.5.28` (Line 50) and references Rust `1.94.0` (Line 520).
* **CLAUDE.md**: Mentions version `26.5.28` (Line 1).
* **docs/README.md**: Mentions version `26.5.28` (Line 41) and references outdated release tags (e.g., `v0.2.0`, `v2.0.0`).
* **Tutorials**: `getting-started.md` references version `2.7.0` (Line 57), while `01-getting-started.md` references `26.5.4` (Line 56).

### Outdated CLI Commands in User Guides
The onboarding walkthroughs direct users to execute subcommands that have been removed:
* **The Reality**: As of version `26.5.19` (documented in `crates/ggen-cli/src/cmds/mod.rs`), the subcommands `ggen generate`, `ggen validate`, `ggen template`, and `ggen ai` were removed or consolidated into `ggen sync`.
* **The Walkthroughs**: 
  - `getting-started.md` instructs users to run `ggen ai generate-ontology ...` (Step 2) and `ggen template generate-rdf ...` (Step 3). Both fail immediately.
  - `docs/reference/01-commands.md` documents `ggen validate`, `ggen inspect`, `ggen add`, `ggen publish`, and `ggen info`, none of which exist in the CLI router.

### Typographical Errors in Type Mapping Reference
The document `docs/reference/type-mapping.md` contains several layout and generation errors:
1. **Duplicate Columns**: The type mapping tables on lines 34 and 264 repeat the column header `Rust` twice. The second column actually lists Python types (e.g. `str`, `int`, `Decimal`).
2. **Incorrect Code Block Labels**: On line 120, a Python `@dataclass` block is labeled as `**Generated Rust:**`.
3. **Tera Comment Leaks**: Internal templating comments have leaked directly into the document text on lines 213, 243, and 298:
   - `{# Presence of comment with "Optional" triggers nullable #}`
   - `{# "List" triggers Vec/[] #}`
   - `{# Generates enum #}`

---

## 5. Code Architecture & Developer Onboarding Assessment

An audit of the codebase structure and build systems highlights portability issues and structural complexity.

### Workspace Structure & Crate Count
The repository contains **21 directories** under `crates/`. However, the workspace root `Cargo.toml` only declares **15 active members**:
```toml
members = [
    "crates/genesis-core",
    "crates/genesis-lockchain",
    "crates/ggen-cli",
    "crates/ggen-config",
    "crates/ggen-core",
    "crates/ggen-graph",
    "crates/ggen-marketplace",
    "crates/ggen-projection",
    "crates/ggen-lsp",
    "crates/ggen-lsp-mcp",
    "crates/ggen-a2a-mcp",
    "crates/playground",
    "crates/cpmp",
    "crates/stpnt",
    "crates/ggen-membrane"
]
```
The remaining 6 directories (`genesis-construct8`, `genesis-core-v2`, `genesis-schema-v2`, `genesis-types-v2`, `genesis-wasm-shell`, and `ggen-lsp-a2a`) are orphaned or dormant on disk, adding clutter for developers scanning the source tree.

### Non-Portable Hardcoded Dependency Path
The root `Cargo.toml` contains a critical compile-blocking line (Line 105):
```toml
wasm4pm-compat = { version = "26.6.9", path = "/Users/sac/wasm4pm-compat" }
```
This hardcoded absolute path points to a local directory belonging to the user `sac`. Any other developer attempting to compile the project will encounter a cargo resolution error because `/Users/sac/wasm4pm-compat` does not exist on their machine.

### Dependency Bloat & Compile Overhead
The core package `ggen-core` serves as a monolithic dependency hub rather than a clean kernel library:
* It directly imports over 100 crates, including heavy-duty web frameworks (`axum`), network runtimes (`hyper`, `reqwest`), source control bindings (`git2`), and compilation parsing tools (`pest`, `syn` with full parsing features).
* **Logging Duplication**: It loads both `slog` (with async, stdlog, and scope adapters) and `tracing` (with tracing-subscriber), introducing duplicate paths for output processing and bloating compile times.

---

## 6. Rails-Inspired Critique & Lessons

To elevate `ggen` into a developer-friendly, high-productivity framework, we look to the design principles of Ruby on Rails.

### 5-Theme Comparative Review

#### 1. Convention over Configuration (CoC)
* **Ggen Current State**: Ggen relies heavily on verbose manual configuration in `ggen.toml`. Every SPARQL-to-template mapping must be explicitly registered under `[[generation.rules]]`.
* **Rails Contrast**: Rails requires zero configuration for standard routing and database models. Naming a file or class automatically establishes associations and paths.
* **The Lesson**: Provide auto-mapping conventions. By default, an RDF class like `schema:Person` should automatically resolve to templates named `Person.rs.tera` and output to `src/models/person.rs` without requiring explicit TOML declarations.

#### 2. Generators & Scaffolding
* **Ggen Current State**: `ggen init` scaffolds basic directories but leaves the developer with empty files. Adding models requires manually writing Turtle syntax and template structures.
* **Rails Contrast**: Running `rails generate scaffold User name:string` instantly creates the database migration, model, controller, views, and test suites.
* **The Lesson**: Implement model generators (e.g., `ggen generate model Person name:string age:integer`). This command should append the appropriate RDF statements to `schema/domain.ttl` and generate baseline templates, sparing developers from writing raw Turtle text.

#### 3. Ontology Migrations
* **Ggen Current State**: Domain specifications are modified in-place inside `domain.ttl`. There is no versioned history of changes, making it difficult to trace or roll back updates.
* **Rails Contrast**: ActiveRecord migrations (`db/migrate`) provide a sequential, version-controlled history of schema changes that can be applied, rolled back, and audited.
* **The Lesson**: Introduce "Ontology Migrations". Developers should write discrete SPARQL UPDATE scripts or schema patches in a `migrations/` folder. Ggen can execute them in sequence, track their run state inside the Oxigraph store, and auto-generate corresponding downstream database migrations.

#### 4. Developer Happiness (DX) & Consoles
* **Ggen Current State**: Testing a SPARQL query or template modification requires executing the full `ggen sync` pipeline. Syntax errors are reported in flat console streams.
* **Rails Contrast**: The `rails console` REPL allows live database queries and code execution. Failures display rich error pages showing the exact line and context of the code crash.
* **The Lesson**: Introduce `ggen console` (or `ggen repl`). This interactive shell should load the Oxigraph triplestore, allowing developers to execute live SPARQL queries and test template renders in real-time. In addition, CLI validation errors should display code snippets highlighting the exact line of failure in `.ttl` or `.tera` files.

#### 5. The Golden Path
* **Ggen Current State**: Ggen is a flexible toolkit that forces the user to make all architectural decisions (defining custom schemas, query files, and templating systems) from day one.
* **Rails Contrast**: Rails is opinionated, defining a "Golden Path" stack (SQLite/PostgreSQL, Hotwire, MVC) that works out of the box.
* **The Lesson**: Provide pre-configured application presets during initialization (e.g. `ggen init --preset axum-sqlx` or `ggen init --preset typescript-express`), giving developers a running application immediately.

---

## 7. Verification Log

This log documents the terminal transcripts, inputs, outputs, and exit codes for all 10 CLI execution stages performed on the `ggen` CLI (binary path: `target/debug/ggen` compiled under Rust `1.97.0-nightly` in `/Users/sac/ggen`).

### Command 1: CLI Version Check
* **Command**: `target/debug/ggen --version`
* **Directory**: `/Users/sac/ggen`
* **Exit Code**: `0`
* **Stdout**:
  ```
  ggen 26.6.9
  ```
* **Stderr**: (empty)

---

### Command 2: Subcommand Help Discovery
* **Command**: `target/debug/ggen doctor --help`
* **Directory**: `/Users/sac/ggen`
* **Exit Code**: `0`
* **Stdout**:
  ```
  Doctor Commands

  This module provides health-check and diagnostic commands for the ggen workspace.

  Usage: ggen doctor [COMMAND]

  Commands:
    check  Quick validation: verifies the workspace can be found and ggen.toml is present
    run    Run a full health check: ggen.toml presence, binary version, workspace state, and toolchain
    help   Print this message or the help of the given subcommand(s)

  Options:
    -h, --help  Print help
  ```
* **Stderr**: (empty)

---

### Command 3: Full Diagnostic Execution
* **Command**: `target/debug/ggen doctor run`
* **Directory**: `/Users/sac/ggen`
* **Exit Code**: `0`
* **Stdout**:
  ```json
  {"binary_version":"26.6.9","checks":[{"detail":"Found ggen.toml in current directory","name":"ggen.toml","passed":true,"recovery":null},{"detail":"Found Cargo.toml — likely in a Rust workspace","name":"Cargo.toml","passed":true,"recovery":null},{"detail":"Found .specify directory — RDF specs present","name":".specify directory","passed":true,"recovery":null},{"detail":"Installed: rustc 1.97.0-nightly (a5c825cd8 2026-04-14)","name":"Rust","passed":true,"recovery":null},{"detail":"Installed: cargo 1.97.0-nightly (eb94155a9 2026-04-09)","name":"Cargo","passed":true,"recovery":null},{"detail":"Installed: git version 2.50.1 (Apple Git-155)","name":"Git","passed":true,"recovery":null},{"detail":"RDF store healthy: /Users/sac/Library/Caches/ggen/packs/marketplace.db","name":"Marketplace DB","passed":true,"recovery":null},{"detail":"Found 17 packs in global user cache (~/.ggen/packs)","name":"User Cache","passed":true,"recovery":null},{"detail":"No observability services found (tried: Tempo, OTel, Jaeger). Errors: Tempo unreachable (port 3200), OTel Collector unreachable (port 13133), Jaeger unreachable (port 16686)","name":"Observability Stack","passed":false,"recovery":"docker compose -f docker-compose.otel.yml up -d"},{"detail":"SLO Violations: Template Rendering slow (252.12125ms). Total: 290.474334ms.","name":"SLO Performance","passed":false,"recovery":"cargo build --release; reduce background CPU load"}],"ggen_toml_found":true,"healthy":true,"message":"All critical health checks passed","workspace_root":"/Users/sac/ggen"}
  ```
* **Stderr**: (empty)

---

### Command 4: Argument Parsing Error (Kebab-case vs. Snake-case)
* **Command**: `target/debug/ggen init --path /Users/sac/ggen/.agents/teamwork_preview_explorer_usability_audit_2/test_init_project --skip-hooks`
* **Directory**: `/Users/sac/ggen`
* **Exit Code**: `1`
* **Stdout**:
  ```
  error: unexpected argument '--skip-hooks' found

    tip: a similar argument exists: '--skip_hooks'

  Usage: ggen init --path <PATH> --skip_hooks <SKIP_HOOKS>

  For more information, try '--help'.
  ERROR: CLI execution failed: Argument parsing failed: error: unexpected argument '--skip-hooks' found

    tip: a similar argument exists: '--skip_hooks'

  Usage: ggen init --path <PATH> --skip_hooks <SKIP_HOOKS>

  For more information, try '--help'.
  ```
* **Stderr**: (empty)

---

### Command 5: Successful Scaffold Initialization
* **Command**: `target/debug/ggen init --path /Users/sac/ggen/.agents/teamwork_preview_explorer_usability_audit_2/test_init_project --skip_hooks true`
* **Directory**: `/Users/sac/ggen`
* **Exit Code**: `0`
* **Stdout**:
  ```json
  {"directories_created":["schema","templates","scripts"],"files_created":["ggen.toml","schema/domain.ttl","Makefile","templates/example.txt.tera","scripts/startup.sh",".gitignore","README.md"],"git_hooks":{"git_repo_detected":false,"hooks_installed":[],"warnings":["Git hooks installation skipped (--skip-hooks flag)"]},"next_steps":["Run 'make setup' to initialize your project","Edit schema/domain.ttl to define your domain model","Create Tera templates in templates/ for your target languages","Run 'make build' to generate code from your ontology"],"project_dir":"/Users/sac/ggen/.agents/teamwork_preview_explorer_usability_audit_2/test_init_project","status":"success","transaction":{"backups_created":0,"committed":true,"total_files":7}}
  ```
* **Stderr**: (empty)

---

### Command 6: Exit Code for Duplicate Initialization Error
* **Command**: `target/debug/ggen init --path /Users/sac/ggen/.agents/teamwork_preview_explorer_usability_audit_2/test_init_project --skip_hooks true`
* **Directory**: `/Users/sac/ggen`
* **Exit Code**: `0`
* **Stdout**:
  ```json
  {"directories_created":[],"error":"ggen project already initialized here. Use --force to reinitialize.","files_created":[],"next_steps":["Run 'make build' to regenerate code"],"project_dir":"/Users/sac/ggen/.agents/teamwork_preview_explorer_usability_audit_2/test_init_project","status":"error"}
  ```
* **Stderr**: (empty)

---

### Command 7: Sync Manifest Parse Error on Raw Scaffolded `ggen.toml`
* **Command**: `target/debug/ggen sync --manifest /Users/sac/ggen/.agents/teamwork_preview_explorer_usability_audit_2/test_init_project/ggen.toml --dry_run true`
* **Directory**: `/Users/sac/ggen`
* **Exit Code**: `1`
* **Stdout**:
  ```
  ERROR: CLI execution failed: Command execution failed: error[E0001]: Manifest parse error
    --> /Users/sac/ggen/.agents/teamwork_preview_explorer_usability_audit_2/test_init_project/ggen.toml
    |
    = error: TOML parse error: TOML parse error at line 26, column 1
     |
  26 | standard_only = true
     | ^^^^^^^^^^^^^
  unknown field `standard_only`, expected one of `source`, `imports`, `base_iri`, `prefixes`

    = help: Check ggen.toml syntax and required fields
  ```
* **Stderr**: (empty)

---

### Command 8: Successful Sync After Correcting `ggen.toml` Schema Discrepancies
* **Command**: `target/debug/ggen sync --manifest /Users/sac/ggen/.agents/teamwork_preview_explorer_usability_audit_2/test_init_project/ggen.toml --dry_run true`
* **Directory**: `/Users/sac/ggen`
* **Exit Code**: `0`
* **Stdout**:
  ```
  [Quality Gate: Manifest Schema] ✓
  [Quality Gate: Ontology Dependencies] ✓
  [Quality Gate: SPARQL Validation] ✓
  [Quality Gate: Template Validation] ✓
  [Quality Gate: File Permissions] ✓
  [Quality Gate: Rule Validation] ✓
  [Quality Gate: DMAIC Phase 1: Define] ✓
  [Quality Gate: DMAIC Phase 2: Measure] ✓
  [Quality Gate: DMAIC Phase 3: Analyze] ✓
  [Quality Gate: DMAIC Phase 4: Improve] ✓
  [Quality Gate: DMAIC Phase 5: Control] ✓

  All Gates: ✅ PASSED → Proceeding to generation phase

  [DRY RUN] Would sync 1 files:
    ontology-summary.txt (would create)

  Inference rules: ["standard-normalization (order: 0)"]
  Generation rules: ["example-rule -> ontology-summary.txt"]
  {"duration_ms":3,"files":[{"action":"would create","path":"ontology-summary.txt","rule":"example-rule","size_bytes":0}],"files_synced":0,"generation_rules_executed":0,"inference_rules_executed":0,"status":"success"}
  ```
* **Stderr**: (empty)

---

### Command 9: Verification of Environment Utility
* **Command**: `target/debug/ggen utils env --list`
* **Directory**: `/Users/sac/ggen`
* **Exit Code**: `0`
* **Stdout**:
  ```json
  {"total":2,"variables":{"HOME":"/Users/sac","PATH":"/Users/sac/.gemini/antigravity-cli/bin:/bin:/usr/bin:/usr/sbin:/sbin:..."}}
  ```
* **Stderr**: (empty)

---

### Command 10: Verification of Policy Subcommand
* **Command**: `target/debug/ggen policy list`
* **Directory**: `/Users/sac/ggen`
* **Exit Code**: `0`
* **Stdout**:
  ```json
  {"profiles":[{"description":"Enterprise profile with no defaults, all values explicit","id":"enterprise-strict","name":"Enterprise Strict","policy_count":4,"receipt_requirement":"Signed","trust_requirement":"EnterpriseApproved"},{"description":"Regulated finance profile with maximum security requirements","id":"regulated-finance","name":"Regulated Finance","policy_count":8,"receipt_requirement":"SignedAndChained","trust_requirement":"EnterpriseCertified"},{"description":"Development profile with relaxed requirements","id":"development","name":"Development","policy_count":0,"receipt_requirement":"DigestOnly","trust_requirement":"Experimental"}],"total":3}
  ```
* **Stderr**: (empty)
