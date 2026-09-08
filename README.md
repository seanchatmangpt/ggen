# ggen

**ggen deterministically manufactures software artifacts from admitted knowledge graphs.**

An RDF/Turtle ontology supplies the domain model. Tera templates carry SPARQL frontmatter. `ggen sync run` resolves the graph, enriches it, extracts bindings, renders outputs in memory, applies write semantics, and emits a BLAKE3-chained receipt describing the graph and resulting artifacts.

```text
ontology + templates + policy → admitted graph → deterministic artifacts → receipt
```

MIT licensed. Rust workspace. The pinned toolchain and verified build path are documented in [Getting Started](docs/GETTING_STARTED.md).

<<<<<<< GENERATED
Current version: `26.9.8` (workspace version in `Cargo.toml`; nightly Rust toolchain
`nightly-2026-06-22`, pinned via `rust-toolchain.toml`). The Definition of Done is `just
pre-commit`, which chains 9 gates: fmt-check → check → lint → test-lib → coherence-check → guard-process-intelligence-boundary → guard-cheat-scan → guard-claims-schema → guard-pack-proofs. This project is
under active, fast-moving development — see [Maturity & Known Limitations](#maturity--known-limitations)
before depending on it for anything production-critical.

=======
<!-- Manual notes for the generated version block go here; this section and everything outside the markers is preserved byte-for-byte by the merge engine. -->
>>>>>>> MANUAL

## System contract

The live sync pipeline is:

```text
Resolve → Enrich → Extract → Render → Write → Receipt
```

1. **Resolve** loads project and pack ontologies into the selected graph backend.
2. **Enrich** applies template `construct:` queries to derive admitted graph facts.
3. **Extract** executes `when:` and `sparql:` selection queries.
4. **Render** evaluates Tera templates entirely in memory.
5. **Write** applies create, inject, skip, merge, or overwrite semantics.
6. **Receipt** records graph and output hashes in `.ggen-v2/receipt.json` and the append-only receipt log.

The documentation must not collapse these stages into an unobserved claim. Current stage-to-span differences and implementation caveats are tracked in [FAQ](docs/FAQ.md).

## Quickstart

```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo build --workspace
cargo run -p ggen-cli-lib --bin ggen -- --help
```

The CLI is built from `ggen-cli-lib`; there is no supported `cargo install ggen` path for the complete CLI. Use the source build above or the repository's canonical `just` recipes.

For a full build, sync, receipt verification, failure map, and replay path, use [Getting Started](docs/GETTING_STARTED.md).

## What ggen owns

- deterministic graph-backed artifact generation;
- ontology and template loading;
- SPARQL-driven enrichment and extraction;
- bounded filesystem actuation through explicit write modes;
- cryptographic generation receipts and replay evidence;
- pack composition and validation gates;
- command surfaces for sync, graph, doctor, receipt, pack, policy, ontology, capability, law, and agent workflows.

## Explicit boundary

ggen emits process evidence. It does not own process discovery, conformance, fitness, precision, or variant analysis. Those analysis surfaces belong to `wasm4pm-compat` and `wasm4pm`. The enforced ownership boundary is defined in `CLAUDE.md` and repository guards.

## Pack ecosystem

A pack combines an ontology, templates, gates, and metadata into a reusable manufacturing unit. Packs can generate implementation modules, tests, documentation, and receipts for consumer projects.

Pack maturity is not inferred from directory presence or isolated passing tests. The Level-5 promotion program records per-capability standing; no prose summary outranks the executable claims and proof gates.

## Capability standing

Use the repository standing vocabulary precisely:

- `ALIVE` — observed execution produced the claimed consequence.
- `PARTIAL_ALIVE` — a bounded checkpoint passed; the crown claim remains open.
- `BLOCKED` — an admitted dependency prevents execution.
- `BUILD_BROKEN` — the relevant verifier cannot currently be reached.
- `UNKNOWN` — observation is absent, stale, or contradictory.
- `UNSUPPORTED` — the capability is outside the admitted boundary.

The authoritative release-relevant claim ledger is [`docs/aps/claims.toml`](docs/aps/claims.toml). Re-run its falsifiers rather than relying on prose snapshots.

## Known limitations

- The workspace requires a pinned nightly Rust toolchain.
- The full CLI is source-built and is not available through `cargo install ggen`.
- Some repository recipes and benchmark surfaces have documented drift; consult the claims ledger and [Performance Quick Start](docs/PERFORMANCE_QUICK_START.md) before treating an exit code as proof of every sub-check.
- Pack Level-5 promotion remains in progress.
- Generated/manual merge targets require their merge markers. Removing them can change whole-file write behavior; use dry-run and preserve generated boundaries.
- Fast CalVer releases make embedded versions, counts, timings, and command transcripts revision-bound evidence rather than permanent contracts.

## Documentation

Start here:

| Need | Canonical surface |
|---|---|
| Build, execute, verify, replay | [Getting Started](docs/GETTING_STARTED.md) |
| Repository questions and implementation caveats | [FAQ](docs/FAQ.md) |
| Current capability standing | [Claims ledger](docs/aps/README.md) |
| Documentation authority and writing law | [Core-Team Documentation Standard](docs/CORE_TEAM_DOCUMENTATION_STANDARD.md) |
| Documentation audit and migration standing | [Documentation Audit Ledger](docs/DOCUMENTATION_AUDIT.md) |
| Full documentation routing | [Documentation Index](docs/README.md) |
| Contribution and verification workflow | [CONTRIBUTING.md](CONTRIBUTING.md) |
| Repository implementation doctrine | [CLAUDE.md](CLAUDE.md) |
| Vulnerability reporting | [SECURITY.md](SECURITY.md) |
| Release history | [CHANGELOG.md](CHANGELOG.md) |

## License

MIT — see [LICENSE](LICENSE).
