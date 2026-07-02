# Machinery Gaps — Generation-First Audit (2026-07)

Verified at git `013bee436`. Every claim below was re-checked against the working tree on
2026-07-02; where a carried-forward P0 has been partially fixed since `AUDIT_DASHBOARD.md`
(2026-04-01), the residual defect is stated with its own evidence.

Companion: `METHODOLOGY.md` (taxonomy), `supersessions.md` (prior-artifact dispositions).

---

## Gap 1 — FLAGSHIP: the one wired generation rule emits 8 empty tuples

The entire production `ggen.toml` pipeline has exactly one `[[generation.rules]]` entry
(`cli-commands-reference`, `ggen.toml:45-62`), and its output is hollow. Three stacked
defects, each individually silent:

**Layer A — the spec is fine.** `.specify/cli-commands.ttl` declares 8 command
individuals (`a cli:Command` at lines 206, 289, 351, 423, 504, 569, 650, 712), each with
`rdfs:label` and `rdfs:comment`. The SELECT in `ggen.toml:47-57` binds `?cmd ?label ?comment`
correctly — 8 rows come back.

**Layer B — the template indexes with a `?` prefix the engine strips.**
`.specify/templates/cli/commands-reference.rs.tera`:

```tera
("{{ row["?cmd"] | default(value="") }}", "{{ row["?label"] | default(value="") }}", "{{ row["?comment"] | default(value="") }}"),
```

But the engine exposes **unprefixed** binding keys. `crates/ggen-core/src/codegen/pipeline.rs:845-846`:

```rust
let clean_key = key.strip_prefix('?').unwrap_or(&key).to_string();
row.insert(clean_key, clean_sparql_term(&term.to_string()));
```

(Same convention throughout: `pipeline.rs:1007`, `:1103`, `:1136`, `:1143`; and the
low-level path `crates/ggen-core/src/sync/mod.rs:521` groups by unprefixed `"service"`.)
So `row["?cmd"]` never matches `row["cmd"]`.

**Layer C — `default(value="")` masks the miss, and the output is consumed by nothing.**
Every failed lookup silently becomes `""`. The actual generated file,
`crates/ggen-cli/src/generated_commands.rs:4-13`:

```rust
pub const COMMANDS_REFERENCE: &[(&str, &str, &str)] = &[
    ("", "", ""),
    ("", "", ""),
    ... // 8 identical empty tuples
];
```

And a workspace-wide grep for `COMMANDS_REFERENCE` finds **only the definition itself**
(`crates/ggen-cli/src/generated_commands.rs:4`) — zero consumers. So the flagship
pipeline runs end-to-end, "succeeds", writes garbage, and nothing would notice even if
it wrote correct data. This is Decorative Completion (`.claude/rules/coding-agent-mistakes.md` §1.1)
in the product's own dogfood loop.

**Fix**: change template keys to `row["cmd"]` etc. (or make the engine reject unknown
keys), delete `default(value="")`, and either wire `COMMANDS_REFERENCE` into `ggen help`
output or delete the rule. **Effort: S** (template is 8 lines).

---

## Gap 2 — `cli_generator/` is a complete ontology→workspace generator with zero CLI entry points

`crates/ggen-core/src/cli_generator/` contains `workspace.rs`, `cli_layer.rs`,
`domain_layer.rs`, `ontology_parser.rs`, `types.rs`, `dx.rs`, `mod.rs` — exactly the
TTL→CLI-project capability the audit question asks for. It is exported at
`crates/ggen-core/src/lib.rs:152` (`pub mod cli_generator;`) and advertised in the crate
docs (`lib.rs:32`).

Verified: `grep -rn "cli_generator" crates/ggen-cli/src` returns **nothing**. No `ggen`
subcommand can reach it. Tested, exported, dark.

**Why it matters**: this is the machinery that would convert `GENERATABLE-WITH-SPEC`
CLI/domain boilerplate into `GENERATED`. **Effort to wire: M** (new noun-verb command +
plumbing; the generator itself exists).

---

## Gap 3 — `project_generator/` emits hardcoded strings, not RDF projections

`crates/ggen-core/src/project_generator/rust.rs` (393 LOC) and `nextjs.rs` (430 LOC)
build entire projects from inline `format!(r#"..."#)` literals — e.g. `rust.rs:85-97`
hardcodes `tokio = { version = "1.0", ... }` / `clap-noun-verb = "26.5"` dependency
blocks, `rust.rs:112-144` hardcodes `[package]` manifests and `async fn main()` bodies.
No `Graph`, no SPARQL, no `.ttl` input anywhere in the module (grep confirms the only
mention of RDF-ish terms is in doc comments). It is also unreachable from the CLI:
`grep -rn "project_generator" crates/ggen-cli/src` returns nothing.

This is Epistemic Bypass (§1.2): the codebase "knows" project shapes it should ask the
ontology for. Under A = μ(O), these 823 lines are anti-machinery — they compete with the
Tera+SPARQL path. **Effort: M** (port to templates + TTL spec, or DEAD-DELETE if the
cli_generator path supersedes it).

---

## Gap 4 — 490 of 491 templates are dark

`find templates -type f | wc -l` → **491**. Wired generation rules in `ggen.toml` → **1**
(grep `output_file` → one hit, `ggen.toml:60`). So at most 1 template
(`.specify/templates/cli/commands-reference.rs.tera` — which is not even in `templates/`)
is exercised by the production manifest; the 491-file `templates/` tree has zero wiring.

Additionally, `.specify/templates/cli/` holds a **whole-crate template family** —
`Cargo.toml.tera`, `main.rs.tera`, `domain.rs.tera`, `error.rs.tera`, `help.rs.tera` —
5 of its 6 files referenced by nothing (grep across `crates/`, `src/`, `ggen.toml` finds
only `commands-reference.rs.tera` at `ggen.toml:59`; the `domain.rs.tera` hits at
`crates/ggen-cli/src/conventions/presets/clap_noun_verb.rs:60-61` are a *different* file,
`crates/ggen-cli/templates/clap-noun-verb/domain.rs.tera`).

**Why it matters**: potential_coverage in METHODOLOGY §Metrics is dominated by
`GENERATABLE-NOW`, and GENERATABLE-NOW requires "template AND spec exist; only wiring
missing" — this is 490 instances of exactly that missing wiring (or of templates that
should be deleted as dead). **Effort: L** (triage all 491; wire or delete).

---

## Gap 5 — "RDF is truth, .md is generated" has zero implementation

CLAUDE.md rule (`CLAUDE.md:132`): "**RDF is Truth** | Edit `.specify/*.ttl` (source).
Never edit `.md` (generated)." Verified: `grep '\.md"' ggen.toml` → no matches. The only
generation rule targets a `.rs` file (`ggen.toml:60`). There is no TTL→markdown rule, no
docs template wired anywhere in the manifest. Every one of the repo's ~hundreds of
modified `.md` files (see git status) is hand-written, and the governing rule that forbids
editing them protects a pipeline that does not exist.

**Why it matters**: the docs corpus is unconditionally `C` (custom glue) in the
three-ledger model until this pipeline exists, and the rule actively misleads
contributors. **Effort: M** (one CONSTRUCT/SELECT + one md.tera per doc family, reusing
the existing rule machinery).

---

## Carried-forward P0s (from AUDIT_DASHBOARD.md / MASTER_TODO.md, re-verified 2026-07)

### P0-01 — SHACL validation: stub largely fixed; residual fail-open remains

**Status change**: MASTER_TODO.md:70-79 describes shacl.rs as "entirely stubbed — always
passes" (shacl.rs:132-155). That is **no longer accurate**: the current
`crates/ggen-core/src/validation/shacl.rs` (344 lines) implements a real SPARQL
`ShapeLoader` and `crates/ggen-core/src/validation/validator.rs` a real `SparqlValidator`
(`validate()` at validator.rs:39-43 loads shapes and checks constraints).

**Residual defect (fail-open, §1.3)** — `shacl.rs:137-140`:

```rust
let shape_rows = match graph.query_cached(find_shapes_query) {
    Ok(crate::graph::CachedResult::Solutions(rows)) => rows,
    _ => return Ok(shape_set),
};
```

Any query error — or an unexpected result kind — silently returns an **empty shape set**,
which `validate_shapes` then iterates zero times, yielding a passing `ValidationResult`.
A broken shapes graph therefore still validates everything. Same pattern at
`shacl.rs:174-176` and `:259` (per-shape/per-field queries degrade silently).

**Trust impact**: SHACL is the μ-pipeline's poka-yoke; a fail-open loader means
`validate_after = true` in ggen.toml can be satisfied vacuously. **Effort: S**
(propagate the `Err`, add a sabotage test: corrupt shapes graph must fail non-zero).

### P0-02 — Production sync uses GenerationPipeline; StagedPipeline reachable only from tests — STILL OPEN

Two parallel pipelines coexist, exactly as AUDIT_DASHBOARD.md:242 said (module renamed:
the old `v26.5.19/` dir is gone; the staged pipeline now lives in `pipeline_engine/`):

- Production: `ggen sync` → `crates/ggen-cli/src/cmds/sync.rs:437`
  (`ggen_core::codegen::executor::SyncExecutor::new(options)`) →
  `GenerationPipeline` (`crates/ggen-core/src/codegen/pipeline.rs:260`, `run()` at
  `pipeline.rs:1457`).
- Constitutional: `StagedPipeline` (`crates/ggen-core/src/pipeline_engine/pipeline.rs:163`,
  `run()` at `:335`, returning a `BuildReceipt`) — grep shows **every** consumer of
  `pipeline_engine` outside its own crate dir is a test file
  (`crates/ggen-core/tests/pack_sync_pipeline_e2e_test.rs:34`,
  `pack_template_integration_test.rs:34-35`, `normalization_shacl_tests.rs:35-36`, etc.).
  Zero hits in `crates/ggen-cli/src`.

**Trust impact**: the pipeline with staged governance/receipt semantics is the one the
docs describe and the tests exercise; the pipeline users actually run is the other one.
Legacy Path Contamination (§1.4) at the architecture level. **Effort: L** (decide
canonical pipeline, migrate or delete the other).

### P0-03 — Three competing ontology namespaces — SUBSTANTIALLY FIXED, residue confined to registry URLs

The fix is in the tree and self-documenting:
`crates/ggen-marketplace/src/marketplace/rdf/ontology.rs:7-29` ("## Single Canonical
Namespace (P0-03 fix)") re-exports `MARKETPLACE_NS` from
`crates/ggen-marketplace/src/marketplace/ontology.rs:19`
(`pub const MARKETPLACE_NS: &str = "https://ggen.io/marketplace/";`) instead of declaring
a second literal, and `crates/ggen-core/src/rdf/schema.rs:46` now uses the same
`"https://ggen.io/marketplace/"`. The silent-data-loss mechanism (insert under one URI,
SELECT under another) is closed for the RDF namespace.

**Residue (non-RDF)**: registry *endpoint* URLs still disagree —
`https://registry.ggen.dev` (`crates/ggen-marketplace/src/marketplace/rdf/turtle_config.rs:435-464`)
vs `https://registry.ggen.io` (`crates/ggen-marketplace/src/marketplace/network.rs:67-389`).
These are HTTP endpoints, not graph namespaces, so no SPARQL data loss — but a client
configured from TTL will point at a different host than the hardcoded default.
**Effort: S** (pick one host constant).

---

## Summary table

| # | Gap | Class (coding-agent-mistakes) | Effort |
|---|-----|-------------------------------|--------|
| 1 | Flagship rule emits 8 empty tuples; output unconsumed | Decorative Completion + Contract Drift | S |
| 2 | `cli_generator/` unwired | Dark machinery | M |
| 3 | `project_generator/` hardcoded strings | Epistemic Bypass | M |
| 4 | 490/491 templates dark; 5 unused `.specify/templates/cli/` files | Dark machinery | L |
| 5 | No TTL→md pipeline behind "RDF is truth" rule | Missing machinery | M |
| P0-01 | SHACL loader fail-open on query error (`shacl.rs:137-140`) | Fail-Open (residual) | S |
| P0-02 | GenerationPipeline vs StagedPipeline duality | Legacy Path Contamination | L |
| P0-03 | Namespace consolidated; registry-URL residue | Contract Drift (residual) | S |
