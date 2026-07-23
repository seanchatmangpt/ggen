# ggen

**ggen is a deterministic, language-agnostic code generation framework that treats software
artifacts as projections of knowledge graphs** — the description in this crate's own
`Cargo.toml`. Concretely: you write an RDF/Turtle ontology plus Tera templates carrying SPARQL
frontmatter; `ggen sync run` queries the ontology, renders the templates, writes the results to
disk, and produces a BLAKE3-chained cryptographic receipt proving exactly what ran and what it
produced.

MIT licensed. Rust workspace, 17 crates. Requires a pinned nightly toolchain (see
[Getting Started](docs/GETTING_STARTED.md)).

<<<<<<< GENERATED
Current version: `26.7.56` (workspace version in `Cargo.toml`; nightly Rust toolchain
`nightly-2026-06-22`, pinned via `rust-toolchain.toml`). The Definition of Done is `just
pre-commit`, which chains 9 gates: fmt-check → check → lint → test-lib → coherence-check → guard-process-intelligence-boundary → guard-cheat-scan → guard-claims-schema → guard-pack-proofs. This project is
under active, fast-moving development — see [Maturity & Known Limitations](#maturity--known-limitations)
before depending on it for anything production-critical.

=======
<!-- Manual notes for the generated version block go here; this section and everything outside the markers is preserved byte-for-byte by the merge engine. -->
>>>>>>> MANUAL

## What actually happens when you run `ggen sync`

The live pipeline (`crates/ggen-engine/src/sync.rs`, whose own module doc names it this way) has
five stages: **Resolve → Enrich → Extract → Render → Write**.

1. **Resolve** — reads your ontology (`.ttl`) and any pack ontologies you depend on into an RDF
   store — either `praxis-graphlaw` (a native N3/Datalog/SPARQL/SHACL/ShEx engine, the default)
   or plain `oxigraph`.
2. **Enrich** — runs each template's `construct:` SPARQL query once against that store, in a
   single pass (not a fixed-point loop).
3. **Extract + Render** — runs `when:`/`sparql:` `SELECT` queries into rows and renders every
   template through Tera, entirely in memory — nothing touches disk yet, so a mid-run failure
   leaves nothing partial behind.
4. **Write** — applies the already-rendered output using Hygen-style create/inject/skip write
   semantics.
5. A `praxis-core::ReceiptRecord` is chained (BLAKE3) over `{graph_hash, outputs}` and written to
   `.ggen-v2/receipt.json`, with the full log at `.ggen-v2/receipt-log.jsonl`.

One honest caveat, because it's real and it's in the code, not a criticism from outside: the
pipeline's own OpenTelemetry span names don't line up 1:1 with these stage names — the
`pipeline.extract` span actually wraps step 2 (Enrich), and the genuine SPARQL row-extraction
work happens inside the `pipeline.generate` span. See [docs/FAQ.md](docs/FAQ.md) for the full
stage-to-span table and for why the older "A = μ(O)" framing some docs and examples still use
doesn't match this implementation's actual stage or span names.

## Quickstart

```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo build --workspace          # nightly toolchain auto-selected via rust-toolchain.toml
cargo run -p ggen-cli-lib --bin ggen -- --help
```

**There is currently no `cargo install ggen` path to the CLI.** The root `ggen` crate published
to crates.io is a pure library shell (it re-exports only a `VERSION` const — its real dependency
chain is `publish = false` and can't ship to the registry). The actual CLI binary is built from
`ggen-cli-lib` (`crates/ggen-cli`), which is itself `publish = false` — build it from a clone.
This repo's own `justfile` defines its canonical invocation as
`cargo run -p ggen-cli-lib --bin ggen --`.

For a full, verified install → build → run → verify walkthrough — every command in this section
was actually run against this codebase, with real output — see
[docs/GETTING_STARTED.md](docs/GETTING_STARTED.md).

## The pack ecosystem

40 pack directories currently live under `packs/` (`ls packs/ | wc -l`). Each pack is a
self-contained `ontology.ttl` + `templates/*.tmpl` + `gates/*.rq` (SPARQL-translated SHACL
constraints) + `pack.toml` bundle that `ggen sync run` composes into generated Rust modules,
tests, docs, and a cryptographic receipt for a consumer project. Real examples: `rmcp-pack`
models the actual `rmcp` crate (MCP Rust SDK) from its vendored source, with 26/26 generated
proof tests passing in a real consumer; `affidavit-pack` transcribes another real project's
receipt/chain/verifier logic byte-checksummed against the live crate, reusing that project's own
tests (46/46 passing).

A formal 12-capability Level-5 promotion program
(`docs/l5-promotion/L5_PROMOTION_PROGRAM.md`) tracks a subset of these packs against a "pack +
ggen alone builds, tests, and receipts the subsystem" maturity bar. No pack has closed all 12
capabilities yet — the program's own status per pack is "promotion actively underway." See
[docs/FAQ.md](docs/FAQ.md) for more.

## Known limitations

This section exists because the project's own claims ledger
([docs/aps/README.md](docs/aps/README.md), `docs/aps/claims.toml`) explicitly expects a prose
mirror of its findings here — and because real runs performed while writing and re-verifying
this page surfaced live, reproducible gaps worth stating plainly rather than smoothing over:

- **`just slo-check`'s Phase 1 CLI-startup benchmark silently measures nothing** (an internal
  `cargo build --release --bin ggen` call fails because the root package's own `[[bin]]` target
  was intentionally removed in a 2026-07-16 refactor, and the benchmark was never updated to
  build `-p ggen-cli-lib` instead — the failure is swallowed and reported only as a skip
  warning). This is real and currently reproducing. The command as a whole still exits `0`,
  though: Phase 2's wall-clock SLO passes and, checked directly (8 independent runs, standalone
  and via the full recipe), the `receipt_chain_e2e` test it wraps currently passes 16/16 — an
  earlier draft of this section reported 5 failing sub-tests and a matching
  `docs/aps/claims.toml` drift; neither reproduced under repeated testing and both have been
  retracted. Full transcripts, exact commands, and exact file lines:
  [docs/PERFORMANCE_QUICK_START.md](docs/PERFORMANCE_QUICK_START.md).
- **The pack ecosystem's Level-5 maturity bar has not been reached by any pack** — real,
  ongoing, per-capability progress, not a finished state. See the pack ecosystem section above.
- For the authoritative, falsifier-backed list of what's currently `ALIVE` / `PARTIAL` /
  `BLOCKED` / `UNVERIFIED` across CLI nouns and release gates, read `docs/aps/claims.toml`
  directly rather than trusting a prose summary (including this one) to stay current.

## Documentation

- [docs/README.md](docs/README.md) — full documentation index
- [docs/GETTING_STARTED.md](docs/GETTING_STARTED.md) — verified install/build/run walkthrough
- [docs/FAQ.md](docs/FAQ.md) — grounded answers to real questions about this codebase
- [docs/PERFORMANCE_QUICK_START.md](docs/PERFORMANCE_QUICK_START.md) — the one performance
  command that's actually automated, and what it returns right now
- [CONTRIBUTING.md](CONTRIBUTING.md) — development workflow, testing policy, PR process
- [CLAUDE.md](CLAUDE.md) — the full project rulebook this codebase is developed under (Chicago
  TDD, evidence-first documentation, the `just` entry point, Andon stop-the-line discipline)
- [SECURITY.md](SECURITY.md) — vulnerability reporting
- [CHANGELOG.md](CHANGELOG.md) — release history

## License

MIT — see [LICENSE](LICENSE).
