# Handoff Report: Usability and Onboarding Audit (Ggen v26.6.9)

## 1. Observation
Here are the direct observations from the codebase, configuration files, and documentation in `/Users/sac/ggen`:

### A. Version Inconsistencies & Typos in Documentation
1. **Root README.md Version Drift**:
   * File: `/Users/sac/ggen/README.md`
   * Line 50: `# ggen v26.5.28`
   * Line 519: `**Version**: 26.5.28`
   * Line 520: `**Stack**: Rust 1.94.0 | Tokio | Oxigraph | Tera | Clap | 15 crates`
   * But the root `Cargo.toml` specifies:
     ```toml
     [workspace.package]
     version = "26.6.9"
     ```
2. **CLAUDE.md Version Drift**:
   * File: `/Users/sac/ggen/CLAUDE.md`
   * Line 1: `# ggen v26.5.28 - Rust Code Generation CLI`
3. **Docs README.md Version Drift**:
   * File: `/Users/sac/ggen/docs/README.md`
   * Line 41: `## 📐 Canonical documentation map — ggen v26.5.28`
   * Lines 303-307: References release `v0.2.0` (stale and pending reconciliation)
   * Lines 311-314: References historical checklist files like `v2.6.0`, `v2.5.1`, and `v2.0.0`
4. **Tutorials Version Drift**:
   * File: `/Users/sac/ggen/docs/tutorials/getting-started.md`
   * Line 57: `# Should output: ggen 2.7.0`
   * File: `/Users/sac/ggen/docs/tutorials/01-getting-started.md`
   * Line 56: `# Output: ggen 26.5.4`
5. **Crate Count Discrepancies**:
   * File: `/Users/sac/ggen/README.md` Line 69: `- ✅ **15-Crate Workspace** — Modular architecture with Rust`
   * In `/Users/sac/ggen/crates/`, there are actually 21 directories:
     `cpmp`, `genesis-construct8`, `genesis-core`, `genesis-core-v2`, `genesis-lockchain`, `genesis-schema-v2`, `genesis-types-v2`, `genesis-wasm-shell`, `ggen-a2a-mcp`, `ggen-cli`, `ggen-config`, `ggen-core`, `ggen-graph`, `ggen-lsp`, `ggen-lsp-a2a`, `ggen-lsp-mcp`, `ggen-marketplace`, `ggen-membrane`, `ggen-projection`, `playground`, `stpnt`.
   * Root `Cargo.toml` defines 15 active members under `members = [...]`. The remaining 6 are dormant/archived.
6. **type-mapping.md Typos & Delimiter Gaps**:
   * File: `/Users/sac/ggen/docs/reference/type-mapping.md`
   * Line 34: `| RDF Type | Rust | TypeScript | Rust | Go | Java |` (Rust is listed twice; the second "Rust" column contains Python types like `str`, `int`, `Decimal`).
   * Line 120: `**Generated Rust:**` (labels Python `@dataclass class User` code block as Rust).
   * Line 264: `| Constraint | Rust | TypeScript | Rust |` (Rust listed twice, 4th column is Python).
   * Lines 213, 243, 298: Direct leaks of Tera comments in the final markdown file:
     * Line 213: `rdfs:comment "Optional middle name" .  {# Presence of comment with "Optional" triggers nullable #}`
     * Line 243: `rdfs:comment "List of products in order" .  {# "List" triggers Vec/[] #}`
     * Line 298: `rdfs:comment "active, inactive, pending" .  {# Generates enum #}`

### B. Outdated/Removed CLI Commands in Documentation
1. **Removed Commands in CLI Router**:
   * File: `/Users/sac/ggen/crates/ggen-cli/src/cmds/mod.rs`
   * Lines 13-25:
     ```rust
     //! The following commands were removed in v26.5.19:
     //! - `ggen generate` → Use `ggen sync`
     //! - `ggen validate` → Use `ggen sync --validate-only`
     //! - `ggen template *` → Use `ggen sync`
     //! - `ggen ai *` → Add back in v26.5.19+
     ```
2. **Tutorial Instructs Stale Commands**:
   * File: `/Users/sac/ggen/docs/tutorials/getting-started.md`
   * Step 2 (Line 65): Instructs to run `ggen ai generate-ontology ...` (command does not exist).
   * Step 3 (Line 77): Instructs to run `ggen template generate-rdf ...` (command does not exist).
3. **Reference Catalogues Stale Commands**:
   * File: `/Users/sac/ggen/docs/reference/01-commands.md`
   * Line 147: Documents `ggen validate` as a top-level command.
   * Line 181: Documents `ggen inspect` (not registered in `cmds/mod.rs`).
   * Line 214: Documents `ggen add` (not registered in `cmds/mod.rs`).
   * Line 248: Documents `ggen publish` (not registered in `cmds/mod.rs`).
   * Line 280: Documents `ggen info` (not registered in `cmds/mod.rs`).

### C. CLI Installation Commands Misalignment
1. **Stale Installation Paths**:
   * File: `/Users/sac/ggen/README.md` Line 75: `cargo install ggen-cli`
   * File: `/Users/sac/ggen/docs/tutorials/getting-started.md` Line 44: `cargo install ggen`
   * In root `Cargo.toml`, the workspace package name is `ggen` (which is a library, no binary).
   * In `/Users/sac/ggen/crates/ggen-cli/Cargo.toml` Line 26, the crate name is `ggen-cli-lib` (binary name is `ggen`).
   * Attempting `cargo install ggen` or `cargo install ggen-cli` from crates.io will fail or install incorrect packages. The correct source command is `cargo install --path crates/ggen-cli`.

### D. Systemic CLI Flag Mismatch (kebab-case vs snake_case)
1. **CLI Parameter Definitions**:
   * File: `/Users/sac/ggen/crates/ggen-cli/src/cmds/sync.rs` Line 338:
     ```rust
     pub fn sync(
         manifest: Option<String>,
         output_dir: Option<String>,
         dry_run: Option<bool>,
         force: Option<bool>,
         audit: Option<bool>,
         ...
         validate_only: Option<bool>,
         ...
     )
     ```
   * File: `/Users/sac/ggen/crates/ggen-cli/src/cmds/init.rs` Line 439:
     ```rust
     pub fn init(
         path: Option<String>,
         force: Option<bool>,
         skip_hooks: Option<bool>,
         ...
     )
     ```
2. **Parser Limitations**:
   * Root `Cargo.toml` Line 778 note: `the local checkout (26.5.29...) works today on published 26.5.19 (snake flags)`.
   * Due to `clap-noun-verb` v26.5.19 limitations, all parameters are mapped using snake_case flags: `--dry_run`, `--output_dir`, `--validate_only`, `--skip_hooks`.
   * But documentation (README.md, commands.md, getting-started.md) references standard kebab-case flags: `--dry-run`, `--output-dir`, `--validate-only`, `--skip-hooks`, leading to parser failures for users copy-pasting.

### E. Redundancies and Code Smells
1. **Stale/Duplicate Doctor Command**:
   * `doctor` is registered as a noun in `/Users/sac/ggen/crates/ggen-cli/src/cmds/doctor.rs` (`ggen doctor run` / `ggen doctor check`).
   * `doctor` is also registered as a verb under the `utils` noun in `/Users/sac/ggen/crates/ggen-cli/src/cmds/utils.rs` (`ggen utils doctor`).
2. **Underscore CLI options**:
   * Under `utils.rs`, `_fix` and `_system` flags map to `--_fix` and `--_system` in the CLI.
3. **Hardcoded User Path in Cargo.toml**:
   * Root `Cargo.toml` Line 105:
     ```toml
     wasm4pm-compat = { version = "26.6.9", path = "/Users/sac/wasm4pm-compat" }
     ```
   * Any developer trying to compile the project on a non-`sac` system will get a cargo build failure because `/Users/sac/wasm4pm-compat` is a hardcoded absolute local path.

### F. Dependency Bloat & Monolith Architecture
1. **Crate dependencies inside `ggen-core`**:
   * File: `/Users/sac/ggen/crates/ggen-core/Cargo.toml`
   * It imports over 100 direct dependencies, including `axum`, `hyper`, `reqwest`, `git2`, `opentelemetry`, `slog` (with slog-scope, slog-stdlog, slog-async), `tracing` (tracing-subscriber), `tempfile`, `rayon`, `pest`, `pest_derive`, `Inflector`, `syn` (with `full` parsing features), `petgraph`, `zip`, `tar`, and `flate2`.
   * This is a massive dependency graph that contributes to slow clean compilation times.
   * Duplicated logging libraries (`slog` and `tracing`) add to binary size and compilation overhead.

---

## 2. Logic Chain

1. **Version Mismatch**:
   * **Fact**: The package version in `/Users/sac/ggen/Cargo.toml` is `26.6.9`.
   * **Fact**: `README.md` and `CLAUDE.md` state the version is `26.5.28` (or `26.5.29`).
   * **Fact**: `getting-started.md` references `2.7.0` and `01-getting-started.md` references `26.5.4`.
   * **Conclusion**: The documentation has not been synchronized with the actual workspace version. This creates confusion about the current capabilities and release state.

2. **Outdated CLI Commands**:
   * **Fact**: `crates/ggen-cli/src/cmds/mod.rs` does not compile or expose modules for `ai` and `template` (explicitly stating they were removed in v26.5.19).
   * **Fact**: `docs/tutorials/getting-started.md` instructs the user to run `ggen ai generate-ontology` and `ggen template generate-rdf`.
   * **Fact**: `docs/reference/01-commands.md` documents `ggen validate`, `ggen inspect`, `ggen add`, and `ggen publish`.
   * **Conclusion**: Onboarding tutorials are broken. A new developer trying to follow the quick start step-by-step will immediately fail at Step 2 and Step 3 with CLI commands not found.

3. **CLI Flag Parser Discrepancies**:
   * **Fact**: `clap-noun-verb` v26.5.19 lacks the kebab-case flag translation hook.
   * **Fact**: The CLI arguments for multi-word parameters use underscores in Rust (e.g., `dry_run`, `validate_only`).
   * **Fact**: All user-facing documentation describes these options with hyphens (e.g., `--dry-run`, `--validate-only`).
   * **Conclusion**: Copied commands from documentation will fail to execute, causing friction and frustration.

4. **Build Portability Issue**:
   * **Fact**: Root `Cargo.toml` references `wasm4pm-compat` via the absolute path `/Users/sac/wasm4pm-compat`.
   * **Fact**: This path is specific to the local user account `sac` and does not exist in standard developer environments.
   * **Conclusion**: Out-of-the-box build reproducibility is broken for other developers. Any clean setup will crash during dependency resolution.

5. **Monolithic Crate Structure**:
   * **Fact**: `ggen-core` includes libraries for REST servers (`axum`), HTTP clients (`reqwest`), Git repository actions (`git2`), and multiple serialization/templating systems.
   * **Fact**: `Cargo.toml` pulls in heavy proc-macros like `syn` (full) and complex dependencies.
   * **Conclusion**: Clean compilation times are heavily impacted. Crate boundaries are blurred: `ggen-core` behaves like a monolith rather than a lean core with peripheral adapter crates.

---

## 3. Caveats
- Since the workspace is in `CODE_ONLY` network mode, we could not inspect external packages (e.g. Homebrew tap `seanchatmangpt/tap/ggen`) to verify if they are functional or up-to-date.
- We did not perform dynamic profile testing or compile-time benchmarks since Cargo was blocking on a lock file (likely held by a concurrent build or stale lock) on `/Users/sac/ggen/target`.
- Our architectural assessment is based on code structure analysis and dependency layouts, not runtime performance measurements.

---

## 4. Conclusion (Architectural Overview & Rails-Inspired Lessons)

### Codebase & Crate Architecture
Ggen is organized as a Cargo workspace with **15 active members** and **6 dormant modules** (on-disk but excluded from the workspace). The core division of labor is:
1. `ggen-cli`: Entry point, parses commands using `clap-noun-verb` and delegates actions.
2. `ggen-core`: The monolith code-generation engine (μ₁-μ₅ pipeline).
3. `ggen-graph`: Oxigraph wrapper with deterministic hashing, delta computation, and validation hooks.
4. `ggen-config`: Validates `ggen.toml`.
5. `ggen-marketplace`: Package registry adapter.
6. `genesis-core-v2`/`genesis-schema-v2`/`genesis-types-v2`: interchangeable-parts kernel for workflow/pattern validation.
7. `cpmp` & `stpnt`: Semantic metadata cataloging and Stewardship cells.

The design relies on **Chicago TDD** (real collaborators, no mocks) and **OTEL tracing** to assert correctness. However, compile times are slow because `ggen-core` pulls in a heavy, centralized list of transitive dependencies instead of delegating platform-specific logic to leaf crates.

---

### Rails Core Team Perspective: 5 Key Evaluations

A Ruby on Rails Core Team member looking at Ggen would evaluate the Developer Experience (DX) and architecture as follows:

#### 1. Convention over Configuration (CoC)
* **Current state**: Ggen is configuration-heavy. The developer must manually register every SPARQL query-to-template mapping under `[[generation.rules]]` in `ggen.toml`.
* **Rails Comparison**: Rails eliminates verbose configurations by standardizing naming structures (e.g. `schema:Person` matches `src/models/person.rs`).
* **Lesson**: Establish standard convention mappings. A class defined in Turtle (e.g., `schema:Person`) should map automatically to standard model, controller, or schema code files without requiring explicit manifest entries.

#### 2. Generators & ScaffoldingScaffolding
* **Current state**: `ggen init` only scaffolds static files. Extending the domain requires manual construction of RDF classes, SPARQL queries, and Tera templates.
* **Rails Comparison**: Rails uses `rails generate model/scaffold` to create complete, interconnected entities.
* **Lesson**: Provide interactive generator commands (e.g., `ggen generate model Person name:string email:string`). This would automatically append the triples to `domain.ttl` and generate corresponding default templates, removing the initial learning curve of RDF.

#### 3. Ontology Migrations
* **Current state**: Changes to the domain require direct editing of the Turtle files. There is no incremental, versioned path to migrate a domain or generated database structures.
* **Rails Comparison**: ActiveRecord migrations (`db/migrate`) provide a step-by-step, version-controlled history of database schema changes that can be run or rolled back.
* **Lesson**: Design "Ontology Migrations". Developers should write incremental SPARQL update scripts (or YAML-based schema edits) that run sequentially. Ggen can trace executed migrations inside the Oxigraph triplestore. This creates an audit history of the domain and enables auto-generation of database migrations.

#### 4. Developer Happiness (DX) & Consoles
* **Current state**: Testing queries or templates requires executing the entire sync pipeline. Diagnostic error outputs can be cryptic and lack context.
* **Rails Comparison**: `rails console` lets developers query data interactively. Rails' error screens display clear, context-rich snippets of code highlighting failures.
* **Lesson**: Build `ggen console` (or `ggen repl`). This interactive console would load the project's ontology into Oxigraph, allowing live SPARQL querying and test rendering of Tera templates. Additionally, syntax/validation errors should show exact line highlights of `.ttl` or `.tera` files.

#### 5. The Golden Path
* **Current state**: Ggen is a flexible toolkit but doesn't prescribe a default stack. Starting a new project requires the developer to design templates and queries from scratch.
* **Rails Comparison**: Rails provides an opinionated, ready-to-run layout out of the box.
* **Lesson**: Package opinionated presets (e.g. `ggen init --preset rust-axum-sqlx`). This would scaffold a standard, working REST API application immediately, ensuring a "zero-friction" onboarding experience.

---

### 3-5 High-Impact Rails-Inspired Lessons for Ggen:
1. **Standardize Naming Conventions**: Eliminate verbose `ggen.toml` configuration by mapping Turtle classes and predicates automatically to source directories and variable names.
2. **Command Line Generators**: Create `ggen generate` command chains to allow developers to build domain schemas in plain text without needing deep expertise in Turtle/RDF syntax.
3. **Provide an Interactive REPL (`ggen console`)**: Load the Oxigraph store in an interactive shell to enable rapid prototyping of queries and templates.
4. **Introduce Ontology Migrations**: Evolve specifications incrementally and version-control schema changes to automatically output database migration scripts.

---

## 5. Verification Method

To verify these findings and audit details:
1. **Grep Version Check**:
   Confirm version differences in documentation:
   `grep -rn "26.5.28" /Users/sac/ggen`
   Verify root Cargo version:
   `grep -rn "version = \"26.6.9\"" /Users/sac/ggen/Cargo.toml`
2. **Validate CLI Subcommand Mismatch**:
   Inspect `crates/ggen-cli/src/cmds/mod.rs` to verify that `ai` and `template` modules are not registered, and that `doctor` is double-registered.
3. **Inspect Dependency Path**:
   Confirm path discrepancy for `wasm4pm-compat` in `/Users/sac/ggen/Cargo.toml`:
   `grep -n "/Users/sac/wasm4pm-compat" /Users/sac/ggen/Cargo.toml`
4. **Observe CLI Flag underscore behavior**:
   Compile the CLI using `cargo build --bin ggen` and run `ggen sync --help` (or inspect `sync.rs` parameters) to observe that parameters map to snake_case flags.
