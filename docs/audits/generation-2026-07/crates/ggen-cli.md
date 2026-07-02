# ggen-cli-lib (`crates/ggen-cli/src`)

**Purpose:** CLI interface for ggen. clap-noun-verb command surface (24 modules under
`cmds/`), plus supporting glue: error types, async/sync runtime bridges, pack-install
orchestration, receipt signing, conventions discovery (resolver/planner/watcher), a
validation framework, and capability introspection.

## LOC

Command: `tokei crates/ggen-cli/src --output json | jq '.Rust.code'` (tokei 14.0.0, 2026-07-02)

| Language | Files | Code |
|---|---|---|
| Rust | 55 | **11,665** |
| Tera (embedded runtime templates) | 10 | 519 |

(The 11,450 figure in the audit roster was an earlier snapshot; this file standardizes
on the number above.) Per-file numbers below via
`tokei <file> --output json | jq '.Rust.code'`.

## Class breakdown

| Class | LOC | % of 11,665 |
|---|---|---|
| GENERATED | 0 | 0% |
| GENERATABLE-NOW | 22 | 0.2% |
| GENERATABLE-WITH-SPEC | 8,601 | 73.7% |
| IRREDUCIBLY-CUSTOM | 3,042 | 26.1% |
| DEAD-DELETE | 0 | 0% |

Estimate basis: per-file classification (every file inspected at least at header/skim
level; generated_commands.rs, main.rs, sync.rs, wizard.rs, init.rs, introspection.rs,
conventions/*, validation_lib/* read in more depth).

## Key finding: the hollow `generated_commands.rs`

`crates/ggen-cli/src/generated_commands.rs` (10 code LOC) carries `//! DO NOT EDIT`
but its `COMMANDS_REFERENCE` array is **8 empty `("", "", "")` tuples** — verified by
reading the file 2026-07-02. The wired rule's SPARQL bindings never reach the template
(key-prefix bug). Template `.specify/templates/cli/commands-reference.rs.tera` and spec
`.specify/cli-commands.ttl` both exist, so per methodology this is **GENERATABLE-NOW**
(wiring broken), NOT GENERATED — the poster child for the anti-gaming rule that an
`@generated` header without reproducibility does not count.

## Per-module rationale

### GENERATABLE-NOW (22 LOC)
- `generated_commands.rs` (10) — see above.
- `main.rs` (12) — thin `cli_match()` entry; `.specify/templates/cli/main.rs.tera`
  exists and `.specify/cli-commands.ttl` exists; only wiring missing.

### GENERATABLE-WITH-SPEC (8,601 LOC)

**`cmds/` noun-verb wiring (~4,890 of cmds' 5,340 LOC)** — the archetypal surface.
Every module is `#[verb]`-annotated clap wiring + serde output structs + thin routing
into ggen-core/ggen-a2a-mcp domain functions (verified in a2a.rs, doctor.rs, graph.rs,
template.rs headers; doctor.rs delegates entirely to
`ggen_core::domain::utils::execute_doctor`, so despite being a diagnostics command the
CLI layer itself is pure wiring). Five unused whole-crate templates exist at
`.specify/templates/cli/{main,error,domain,help}.rs.tera` + `Cargo.toml.tera`, and an
embedded template family at `crates/ggen-cli/src/conventions/templates/clap-noun-verb/`
({command,noun,verb,domain,error,help,main}.rs.tera) — command scaffolding is arguably
GENERATABLE-NOW where the template shape matches (thin modules like `mcp.rs` 34,
`sigma.rs` 34, `packs_receipt.rs` 9, `helpers.rs` 55, `mod.rs` 38), but each module
carries bespoke arg/output shapes not yet in any TTL spec, so the conservative class is
GENERATABLE-WITH-SPEC. Includes `wizard.rs` (1,435) and `init.rs` (882): scaffold
commands whose bulk is hardcoded seed-file content — an epistemic-bypass pattern; the
scaffold content itself should be templates + TTL. Excludes `sync.rs` (below).

**Support glue (~3,711 LOC)**
- `lib.rs` (179), `prelude.rs` (8) — module registry/re-exports; registry-shaped.
- `error.rs` (203) — thiserror enum; template `.specify/templates/cli/error.rs.tera`
  exists (near-NOW; error taxonomy not yet in a TTL spec).
- `introspection.rs` (1,228) — `VerbMetadata`/`CommandGraph` serde structs and a
  hand-maintained verb registry; exactly the data `.specify/cli-commands.ttl` should
  own. Largest single generation win in the crate.
- `runtime.rs` (51), `runtime_helper.rs` (104) — fixed async-bridge boilerplate.
- `config_clap/` (128) — serde config loader + error enum boilerplate.
- `validation/mod.rs` (123) — command-structure validation tables.
- `validation_lib/{mod,error,security,io_validator}.rs` (359) — permission/rule tables,
  boilerplate-shaped.
- `conventions/{mod,presets/*}` (93) — preset data tables (clap_noun_verb preset is
  pure pattern lists).

### IRREDUCIBLY-CUSTOM (3,042 LOC) — every entry a defended claim
- `cmds/sync.rs` (450) — μ₁–μ₅ pipeline orchestration, exit-code policy, transaction
  handling. The pipeline driver cannot be emitted by the pipeline it drives.
- `conventions/planner.rs` (499), `resolver.rs` (438), `watcher.rs` (196) — convention
  discovery, template-metadata parsing, generation planning, debounced file watching.
  Genuinely algorithmic; feeds generation rather than being generated.
- `pack_install.rs` (484) — concurrent install orchestration (Semaphore, join_all,
  caching, dependency-resolution visualization).
- `progress.rs` (390) — async progress state machine over `tokio::broadcast`.
  (Candidate for replacement by a crate like indicatif rather than generation.)
- `receipt_manager.rs` (285) — ed25519 receipt signing/verification; engine-core
  (E-ledger) allowlist candidate per methodology.
- `validation_lib/noun_verb_validator.rs` (163) — cycle detection over command
  dependency graph.
- `version_checker.rs` (137) — binary-mtime/CI-env heuristic.

**Summary:** 0% generated today; ~74% is spec-shaped CLI wiring/registries
(GENERATABLE-WITH-SPEC, consistent with the prior ~75% estimate), 22 LOC is
GENERATABLE-NOW including the hollow generated_commands.rs, and a 3,042-LOC custom core
(pipeline driver, conventions engine, receipts, install orchestration).
