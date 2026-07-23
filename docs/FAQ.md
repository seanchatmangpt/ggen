# FAQ

Every answer below is tied to a specific, checkable fact in this repository — a file, a real
command run, or a specific line — not a general impression of how a project like this "usually"
works. Line numbers and version strings drift as the repo evolves; where that matters, the answer
says how to re-check it yourself rather than asking you to trust a frozen number.

## What is ggen, in one paragraph?

ggen is a Rust workspace (root package `ggen`) whose live code-generation engine is
`ggen-engine`, vendored from `~/praxis/crates/ggen` (`crates/ggen-engine/README.md`: "SPARQL-in-
Tera code generation"). A project defines an RDF/Turtle ontology plus Tera templates carrying
SPARQL frontmatter; `ggen-engine::sync::sync()` runs SPARQL `CONSTRUCT`/`SELECT` queries against
an RDF store (`praxis-graphlaw`, a native N3/Datalog/SPARQL/SHACL/ShEx engine, or plain
`oxigraph`), renders Tera templates from the query results, writes files under Hygen-style
create/inject/skip write semantics, and produces a BLAKE3-chained cryptographic receipt
(`praxis-core::ReceiptRecord`) at `.ggen-v2/receipt.json`. Root `Cargo.toml`'s own description
puts it as "a deterministic, language-agnostic code generation framework that treats software
artifacts as projections of knowledge graphs."

## Is the "A = μ(O)" / μ₁–μ₅ formula how the pipeline actually works?

As a description of the *live* `sync.rs` implementation: no, not exactly, and this is worth
being precise about rather than repeating the marketing framing as fact. `crates/ggen-engine/
src/sync.rs`'s own module doc names the pipeline **Resolve → Enrich → Extract → Render → Write**
— five stages, but different names than "Normalize → Extract → Emit → Canonicalize → Receipt."
Reading the function body against its own OpenTelemetry span names surfaces a further
inconsistency baked into the code itself, not something imposed by an outside reading: the
`pipeline.extract` span actually wraps the *Enrich* stage (single-pass SPARQL `CONSTRUCT`
materialization), while the genuine SPARQL row-extraction work happens inside the
`pipeline.generate` span. So: five real stages, real span instrumentation, a real span/stage
naming mismatch inside the code's own self-description — and a separate, older "A = μ(O)" framing
used in some docs and examples that does not correspond to any stage or span name in `sync.rs`.
Treat the μ-formula as aspirational scaffolding language, not a description of what runs.

## What does a sync actually produce?

After a non-dry-run `ggen sync run`, a `praxis-core::ReceiptRecord` is chained (BLAKE3) over
`{graph_hash, outputs: {path → BLAKE3}}` and written to `.ggen-v2/receipt.json`, with the full
append-only log at `.ggen-v2/receipt-log.jsonl`. `ggen receipt verify` (zero arguments) recomputes
and checks that chain against the resolved project root's verifying key
(`.ggen/keys/verifying.key`, generated on first real sync if absent). A real run against
`examples/praxis-core-verify` returned `"valid": true`, `"signed": true`, `"signature_valid":
true`, `"outputs": 6` — see `docs/GETTING_STARTED.md` for the full transcript. A legacy or
unsigned receipt reports `"signed": false` rather than failing outright.

## How many crates are actually in this workspace?

Root `Cargo.toml`'s `[workspace] members` array currently lists 16 entries under `crates/`, plus
the root `ggen` package itself, for **17 workspace crates total**. This is confirmed two
independent ways: `grep -c '^  "crates/' Cargo.toml` and `ls crates/ | wc -l` both return 16.
One further directory, `examples/7-agent-validation`, is explicitly `exclude`d (broken build,
non-member) — it is not part of the 17. `.claude/rules/architecture.md` carries the current,
actively-maintained per-crate breakdown; treat it as more current than any archived doc, and
re-run the `grep`/`ls` above yourself if the number matters for what you're doing, since this repo
adds and removes workspace members at a real cadence.

## What happened to `ggen-core`?

It is fully deleted, not merely disconnected. An interim migration step first moved every
dependent off `ggen-core` and excluded it from `[workspace] members`; a later commit deleted
`crates/ggen-core/` from disk outright. `ls crates/ | grep -i core` today returns only
`genesis-core-v2` and `praxis-core` — no `ggen-core`. `ggen-engine` is the sole live replacement:
`ggen sync`/`doctor`/`graph`/`receipt` all route to it (`crates/ggen-cli/src/lib.rs`'s
`inject_default_verbs`). A handful of experimental, default-off commands that used to import
`ggen_core::` symbols (`wizard`, `sigma`, `inverse_sync`) were deleted in the same pass rather
than re-pointed at the new engine.

## How many packs does ggen ship, and what is a "pack"?

40 pack directories currently exist under `packs/` (`ls packs/ | wc -l`) — a real, current count,
not the "32" some older docs still cite; re-run the `ls` if you need the number to be current for
your purposes, since this repo actively adds packs. Every pack has the same real shape:
`ontology.ttl` (RDF facts) + `templates/*.tmpl` (Tera templates that SPARQL-query the ontology) +
`gates/*.rq` (SPARQL-translated SHACL constraints) + `pack.toml` (name/version/description). A
consumer project declares which packs it depends on in its own `ggen.toml`; `ggen sync run`
composes the pack's ontology and templates into generated modules, tests, docs, and a
cryptographic receipt for that consumer. Concrete, verified examples: `rmcp-pack` models the real
`rmcp` crate (MCP Rust SDK) from its actual vendored source, not docs.rs prose, and its generated
proof tests pass 26/26 in a real consumer; `affidavit-pack` transcribes `~/affidavit`'s real
receipt/chain/verifier source, byte-checksummed against the live crate, and reuses that project's
own test functions — 46/46 passing in `examples/affidavit-verify`.

## Has any pack reached full maturity ("Level 5")?

No — and the project's own tracking doc says so directly, not as an inference. A formal
12-capability Level-5 promotion program (`docs/l5-promotion/L5_PROMOTION_PROGRAM.md`, generated
from `.specify/pack-l5-promotion.ttl`) tracks a subset of packs against a 12-capability bar
(authoritative semantic source, complete generation surface, deterministic regen, generated
verification, generated negative witnesses, generated receipts, consumer replacement, and others).
The doc's own wave structure states the terminal wave ("every non-terminal pack has reached
Level5/Incompatible/Superseded/RejectedWithProof") is still pending. Progress is real but partial
and per-capability: some packs have closed specific capabilities with live, sabotage-tested proof
suites (e.g. editing the ontology and confirming the derived proof fails, then reverting) — but
zero packs have closed all 12. Treat any blanket "pack X is done" claim as unverified until you
check that specific pack's row in that doc.

## Does `just pre-commit` actually pass?

Run it yourself and read the tail of the output — that is the only way to answer this for the
commit you're looking at, and the honest answer changes over time as the workspace grows. It
chains a fixed sequence of gates defined in `justfile`'s `pre-commit:` recipe (currently 11:
`fmt-check`, `check`, `lint`, `test-lib`, `coherence-check`,
`guard-process-intelligence-boundary`, `guard-cheat-scan`, `guard-claims-schema`,
`guard-pack-proofs`, `guard-generation-hash-pin`, `guard-pack-count` — count and names verified
by reading the recipe directly; some docs, including parts of this repo's own `CLAUDE.md`, still
cite "10 gates" and predate `guard-pack-count`). Concretely, from a real run against this
codebase: `check` is `cargo check --workspace`; `lint` is `cargo clippy --all-targets -D warnings`
scoped to the root `ggen` package only, not `--workspace` (other crates carry real, untriaged
clippy debt that would fail if the scope were widened); `test-lib` runs each crate's real test
suite (`cargo test --lib --workspace`); `guard-cheat-scan` runs the `ggen-cheat-scanner` crate
against the whole tree looking for vacuous asserts, tautological checks, assertion-free tests, and
mock imports; `guard-pack-proofs` re-syncs and re-tests several real example consumers end to end;
`guard-pack-count` cross-checks a declared pack count in `.specify/repo-facts.ttl` against the
real `packs/` directory count.

## Why does the workspace require nightly Rust instead of stable?

Two crates.io-published dependencies use nightly-only language features, reached transitively:
`wasm4pm-compat` (via `ggen-lsp` → `lsp-max` → `lsp-max-runtime`, using
`#![feature(unsized_const_params, min_specialization, const_trait_impl, portable_simd)]`) and
`libsqlite3-sys` 0.38.x (via `cpmp` → `rusqlite`'s `bundled` feature, using the unstable
`cfg_select!` macro in its build script). `rust-toolchain.toml` pins a specific dated nightly
(`nightly-2026-06-22` at time of writing) rather than a floating `nightly` channel, specifically
so builds are reproducible from a clean checkout — `rustup` reads and honors this file
automatically; no manual toolchain selection is needed.

## What license is ggen released under?

MIT. The root `LICENSE` file is the standard MIT text (copyright Sean Chatman), and root
`Cargo.toml`'s `[workspace.package]` sets `license = "MIT"`, inherited by every workspace member
via `license.workspace = true`.

## `ggen.toml` has two different-looking schemas in the wild — which one am I looking at?

Both are real and both are live; `ggen.toml` is parsed by one of two independently-defined,
incompatible struct hierarchies, selected by a raw-text pre-parse before any typed parse runs.
`ggen_engine::generation_rules::has_generation_rules` checks the raw TOML for a non-empty
`[[generation.rules]]` array. If present, `sync()` parses the file as
`ggen_config::manifest::GgenManifest` (the "declarative-rules" schema — `[[packs]]` is a flat
array-of-tables). If absent, it falls through to `ggen_engine::config::GgenConfig` (the
"frontmatter" schema — `[packs]` is a table-of-tables of an untagged `Path | Git` enum). Same
top-level table names in both (`[project]`, `[ontology]`, `[packs]`, `[templates]`, `[law]`),
genuinely divergent internal shapes, no automated drift guard between them. If a config value
isn't doing what you expect, check which schema your `ggen.toml` actually triggers before
assuming a bug.

## Is `just sync` / `just sync-dry` broken?

Depends what you mean, and this is a real example of a doc/reality gap worth naming explicitly
rather than silently repeating the stale half. The underlying CLI-argument bug is real: `ggen
sync run --audit true` fails with `error: unexpected argument '--audit' found` (there is no
`--audit` flag, only `--dry-run`/`--watch`), and `--dry-run` is a bare switch, not a
`--dry_run true` key-value pair. But the current `justfile` recipes for `sync`/`sync-dry` no
longer pass those broken arguments — they call `ggen sync run` and `ggen sync run --dry-run`
respectively, and both were run for real against this codebase and exited `0`. If you find a doc
(including, as of this writing, parts of this repo's own `CLAUDE.md`) claiming `just
sync`/`sync-dry` are "currently broken," verify against the live `justfile` before trusting it —
docs here have drifted behind fixes before, and will again.

## How do I check whether ggen is meeting its own performance targets?

`just slo-check` — see `docs/PERFORMANCE_QUICK_START.md` for exactly what it measures, what it
doesn't, and how to read its output.
