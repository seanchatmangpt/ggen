# ggen

**Generate, don't hand-write.** ggen is an RDF/ontology-driven code-generation engine: you model your domain as a Turtle ontology, bind Tera templates with SPARQL, and `ggen sync run` deterministically manufactures code, tests, and docs — then proves what ran with a BLAKE3-chained receipt.

```text
ontology + templates + ggen.toml → 5-stage sync → generated artifacts → receipt
```

MIT licensed. Rust workspace (14 crates). Pinned nightly toolchain (`rust-toolchain.toml`).

<<<<<<< GENERATED
Current version: `26.10.10` (workspace version in `Cargo.toml`; nightly Rust toolchain
`nightly-2026-06-22`, pinned via `rust-toolchain.toml`). The Definition of Done is `just
pre-commit`; its full gate list is the `pre-commit:` dependency line in `justfile` (the sole
source of truth — it has drifted before, do not restate a count here). This project is
under active, fast-moving development — see [Maturity & Known Limitations](#maturity--known-limitations)
before depending on it for anything production-critical.

=======
<!-- Manual notes for the generated version block go here; this section and everything outside the markers is preserved byte-for-byte by the merge engine. -->
>>>>>>> MANUAL

## Install

Source build only — there is no supported `cargo install ggen` path for the complete CLI:

```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo build --workspace
```

## Quickstart

```bash
ggen init my-project       # scaffold: ggen.toml + schema/domain.ttl + templates/
cd my-project              # edit the ontology, add a rule to ggen.toml
ggen sync run              # 5-stage pipeline; writes artifacts + .ggen-v2/receipt.json
ggen receipt verify        # verify the BLAKE3 chain hash of the receipt
```

## The pipeline

`ggen sync run` executes five stages (`crates/ggen-engine/src/sync.rs`), each an OTEL span:

1. **Load** — load project + pack ontologies into the graph backend.
2. **Extract** — run SPARQL selection queries over the graph.
3. **Validate** — SHACL/graph validation; violations refuse the sync.
4. **Generate** — render Tera templates in memory.
5. **Emit** — apply write semantics (create/inject/merge/overwrite) and record the receipt.

Author-time diagnostics (`ggen-lsp`): `GGEN-TPL-001`, `GGEN-OUT-001`, `GGEN-YIELD-001`, `GGEN-RULE-001`, `GGEN-QUERY-002`; sync-time hard errors `E0010`/`E0011`/`E0013`/`E0014`/`E0015` in `crates/ggen-config/src/manifest/validation.rs`. Full table: [CLAUDE.md](CLAUDE.md).

## Packs

A pack is a reusable manufacturing unit: ontology + templates + gates + metadata. The repo ships **95 packs** (`packs/*/pack.toml`) — agent/workflow codegen, TCPS, domain catalogs, release/CI. `ggen packs` manages lockfile-oriented multi-pack projects; `ggen pack` is the singular alias.

## Commands

| Command | Purpose |
|---|---|
| `ggen init` | Scaffold a new project |
| `ggen sync run` | Run the 5-stage pipeline (front door) |
| `ggen receipt verify` | Verify receipt chain hash |
| `ggen graph validate` | SHACL-validate ontologies |
| `ggen law validate` | Validate law gates on the project graph |
| `ggen doctor` | Non-actuating diagnostics + remediation evidence |
| `ggen packs` | Multi-pack project management |
| `ggen ontology` / `ggen capability` / `ggen agent` | Ontology, capability, and agent surfaces |

There is no bare `ggen validate` noun. MCP introspection ships separately (`ggen-mcp` crate).

## Known limitations

- Two incompatible `ggen.toml` schemas exist by raw-text pre-parse; no drift guard.
- `just sync` / `just sync-dry` recipes are broken — use `ggen sync run` directly.
- `wizard`/`sigma`/`inverse_sync` were removed from the default path.
- Requires a pinned nightly toolchain; generated/manual merge targets require their markers intact.

## Documentation

| Need | Surface |
|---|---|
| Build, execute, verify, replay | [Getting Started](docs/GETTING_STARTED.md) |
| FAQ + implementation caveats | [docs/FAQ.md](docs/FAQ.md) |
| Capability standing ledger | [docs/aps/claims.toml](docs/aps/claims.toml) |
| Contribution workflow | [CONTRIBUTING.md](CONTRIBUTING.md) |
| Full routing | [docs/README.md](docs/README.md) |

## License

MIT — see [LICENSE](LICENSE).
