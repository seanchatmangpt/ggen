# ggen

**ggen turns an RDF/Turtle ontology and a set of Tera templates into generated source code, with a
cryptographically-signed receipt proving what ran.**

If you already keep a domain model as data (RDF triples) rather than as scattered structs across
services, ggen lets you write that model once and project it into as many target languages/files as
you have templates for — and every generation run produces a BLAKE3-hashed, chain-linked receipt so
you can prove later exactly which ontology + template inputs produced which output files. You do not
need prior RDF/SPARQL experience to try the quick start below; `ggen init` gives you a working
example ontology and template to start from.

Current version: `26.7.4` (nightly Rust toolchain, pinned via `rust-toolchain.toml`). This project is
under active, fast-moving development — see [Maturity & Known Limitations](#maturity--known-limitations)
before depending on it for anything production-critical.

## Quick Start

```bash
# 1. Build from source (not yet published to crates.io under this name)
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo build -p ggen-cli-lib --bin ggen
# binary is at target/debug/ggen

# 2. Initialize a project (creates ggen.toml, schema/domain.ttl, templates/)
mkdir my-project && cd my-project
/path/to/ggen init

# 3. Preview generation (no files written)
ggen sync run --dry-run

# 4. Run for real — writes output files and a signed receipt
ggen sync run

# 5. Verify the receipt's cryptographic chain
ggen receipt verify
```

Every step above was run against a fresh `ggen init` scaffold as part of writing this README. `ggen
init` creates `ggen.toml`, `schema/domain.ttl` (an example ontology), `templates/example.txt.tera`,
and a `scripts/startup.sh`. Edit `schema/domain.ttl` with your own domain model and add templates
under `templates/`, then re-run `ggen sync run`.

## What actually works today

Confirmed by direct execution, not by reading source:

- `ggen init` — scaffolds a new project (ggen.toml, example ontology, example template)
- `ggen sync run [--dry-run]` — the five-stage pipeline (load → extract → validate → generate →
  emit), with OpenTelemetry spans per stage and a graph-hash-addressed output decision log
- `ggen receipt verify` — verifies the BLAKE3 chain hash and Ed25519 signature of the current sync
  receipt (`.ggen-v2/receipt.json`); reports `signed: true/false` and `signature_valid`
- `ggen graph validate --files <path>` — parses and hashes a Turtle file, reporting quad count
- `ggen --help` — full noun/verb surface: `init`, `ontology`, `receipt`, `packs`/`pack`, `doctor`,
  `capability`, `graph`, `utils`, `policy`, `agent`, `law`, and more (`ggen <noun> --help` for each)

Under the hood, the live pipeline is `ggen-engine` (a native SPARQL-in-Tera engine backed by
`praxis-graphlaw`, an N3/Datalog/SPARQL/SHACL/ShEx implementation) — not the older `ggen-core`
crate, which is disconnected from the workspace build (see Architecture below).

## Maturity & Known Limitations

This is pre-1.0 software with real, currently-open rough edges. Documented here rather than
discovered by you:

- **`ggen.toml` has two incompatible schemas.** Which one a command expects depends on whether it
  reads a raw `[[generation.rules]]` array or not. In practice this means the `ggen.toml` that
  `ggen init` generates is **not** accepted by `ggen doctor` or `ggen law validate` — both fail with
  `TOML parse error ... unknown field 'version', expected 'name'` on a stock `ggen init` output,
  confirmed by direct execution. If you hit this, check which schema the command you're running
  expects before assuming your ontology is at fault.
- **`ggen policy check`/`ggen policy validate` are not usable standalone.** `policy check` requires
  packs to already be installed (`ggen packs install <pack-id>` first) and fails otherwise; `policy
  validate` requires a `--profile` argument with no default. Neither is a drop-in "validate my
  project" command yet.
- **`just sync` and `just sync-dry` are currently broken** — they pass flags (`--audit`, positional
  `true` after `--dry_run`) that the live `sync run` verb doesn't accept. Use `ggen sync run
  [--dry-run]` directly instead of the `just` wrappers for sync.
- **`just lint` and `just bench` only cover the root `ggen` package**, not `--workspace` — passing
  clippy/bench cleanly on `just lint` does not mean every crate in the workspace is clean.
- **`ggen-core` is disconnected but not deleted.** It's excluded from the Cargo workspace
  (`members` → `exclude`) and does not compile standalone. It is not on any default command path;
  treat any doc or comment that still mentions it as historical.
- **No published crate yet.** Install from source; there is no `cargo install ggen-cli` that
  resolves to this project today.

If something above is stale by the time you read it, trust a live run over this file — commands are
the ground truth, not prose.

## Documentation

- `CLAUDE.md` (repo root) — architecture, crate map, workflow, and the rules this project holds
  itself to (Chicago TDD, evidence-first documentation, OTEL validation)
- `.claude/rules/architecture.md` — the actively-maintained crate map and cross-cutting patterns
- `docs/INDEX.md` — full documentation index (tutorials, per-noun command reference, marketplace
  architecture)
- `docs/GETTING_STARTED.md` — a longer walkthrough of ontology embedding and your first generation

## Architecture (short version)

```
ggen.toml → RDF ontology (Turtle) → SPARQL extraction → Tera templates → generated code
                                                                        ↘ BLAKE3 receipt chain
```

The live pipeline (`ggen-engine`) has five stages — Resolve, Enrich, Extract, Render, Write — each
emitting its own OpenTelemetry span (`pipeline.load`, `.extract`, `.validate`, `.generate`,
`.emit`). Its default graph backend is `praxis-graphlaw`, a native N3/Datalog/SPARQL/SHACL/ShEx
engine vendored into this workspace. See `.claude/rules/architecture.md` for the full 16-crate
breakdown; this README intentionally doesn't duplicate it.

## Testing

This project uses Chicago-style TDD exclusively (real collaborators — real filesystems, real
databases, real HTTP — no mocks or test doubles). See `.claude/rules/rust/testing.md` for the full
policy if you're contributing tests.

```bash
just check   # compile check
just test    # full test suite
just lint    # clippy (root package only — see Known Limitations)
```

## License

See [LICENSE](LICENSE) for licensing terms.

## Contributing

Issues and pull requests are welcome at
[github.com/seanchatmangpt/ggen](https://github.com/seanchatmangpt/ggen). Before submitting a PR,
run `just pre-commit` (fmt-check → check → lint → test-lib → coherence-check) — see `CLAUDE.md`
for the full contributor workflow and testing discipline.
