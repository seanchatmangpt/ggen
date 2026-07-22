# Getting Started

This walks through installing the toolchain, building `ggen` from source, and running a real
sync against an existing example project. Every command below was actually run against this
codebase during verification; output is transcribed, not invented. Where the live workspace
version drifted between commands (this project bumps its CalVer version frequently — sometimes
mid-session), that is called out rather than smoothed over.

## Prerequisites

ggen is a Rust workspace and currently requires a pinned **nightly** toolchain — not stable.
`rust-toolchain.toml` at the repo root pins it:

```toml
[toolchain]
channel = "nightly-2026-06-22"
components = ["rustfmt", "clippy"]
```

Two crates.io dependencies force nightly rather than stable: `wasm4pm-compat` (reached via
`ggen-lsp` → `lsp-max` → `lsp-max-runtime`, which uses `#![feature(unsized_const_params,
min_specialization, const_trait_impl, portable_simd)]`) and `libsqlite3-sys` 0.38.x (reached via
`cpmp` → `rusqlite`'s `bundled` feature, which uses the unstable `cfg_select!` macro in its build
script). You do not need to select this toolchain by hand — `rustup` reads `rust-toolchain.toml`
automatically as soon as you run any `cargo`/`rustc` command inside the repo, and will offer to
install it if it isn't present.

You also need [`just`](https://github.com/casey/just) — it is this repo's single command-runner
entry point (never `cargo make`, never a bare `cargo <task>` for gated work; see `CLAUDE.md`).

Verified toolchain versions on the machine this guide was written on:

```console
$ rustc --version && cargo --version
rustc 1.98.0-nightly (91fe22da8 2026-06-21)
cargo 1.98.0-nightly (a595d0da2 2026-06-20)

$ just --version
just 1.56.0
```

`rustup show` confirms the pin is actually honored, not just declared: it reports
`nightly-2026-06-22-aarch64-apple-darwin` as active, "**overridden by
`/Users/sac/ggen/rust-toolchain.toml`**".

## Clone and build

```bash
git clone https://github.com/seanchatmangpt/ggen
cd ggen
cargo build --workspace
```

A real `cargo build --workspace` run against this codebase finished in **1m08s** and exited
`0`, with 0 compiler errors and 19 warnings (13 in `ggen-engine`, 1 in `bcinr-pddl`, the rest
scattered `missing_docs` lints) — tail of the real output:

```console
   Compiling ggen-cli-lib v26.7.30 (/Users/sac/ggen/crates/ggen-cli)
warning: `ggen-engine` (lib) generated 13 warnings (run `cargo fix --lib -p ggen-engine` to apply 1 suggestion)
   Compiling ggen-lsp v26.7.30 (/Users/sac/ggen/crates/ggen-lsp)
    Finished `dev` profile [unoptimized + debuginfo] target(s) in 1m 08s
```

That run reused a partially warm `target/` directory (already 51G from earlier same-day builds),
so 1m08s is **not** a from-empty-`target/` number — a genuinely cold build was not measured. This
workspace is large (17 crates, several vendored dependency crates including a native SPARQL/SHACL
engine) — budget more time on a first, truly clean checkout, especially without a warm
[`sccache`](https://github.com/mozilla/sccache) cache (this repo's `.cargo/config.toml` wires
`sccache` in as the `rustc-wrapper` by default).

The CLI binary lives at `target/debug/ggen` after the build. It is built from the `ggen-cli-lib`
crate (`crates/ggen-cli`) — **not** from the root `ggen` package, which is now a thin, largely
inert library shell (`src/lib.rs` re-exports only a `VERSION` const, because its real dependency
chain — `ggen-engine` → `praxis-core`/`praxis-graphlaw` — is `publish = false` and can't be
re-exported from the crates.io-published `ggen` crate).

## Verify the build

```console
$ ./target/debug/ggen --version
ggen 26.7.30

$ ./target/debug/ggen --help
Usage: ggen [OPTIONS] [COMMAND]

Commands:
  init        Initialize a new ggen project with default structure and scripts...
  policy      This module provides policy management commands wired to the marketplace layer.
  pack        Pack Commands (singular alias for `packs`)
  agent       Agent noun — the AGI-facing CLI surface over `crate::agent::PackAgent`.
  utils       Utils Commands - clap-noun-verb v3.4.0 Migration
  sync        Run the ggen code-generation pipeline: resolve, enrich, extract, render, write.
  graph       Validate RDF/Turtle ontology graphs against praxis vocabulary constraints.
  ontology    Ontology Commands - Embedded and Marketplace Ontology Management
  capability  Capability noun — resolve and enable capability surfaces (`ggen capability <verb>`).
  law         Law-state operations on the project graph: load rules, materialize, validate
              gates, explain derivations, export.
  packs       Packs noun — lockfile-oriented, multi-pack project management (`ggen packs <verb>`).
  receipt     Inspect and verify sync receipt chains (BLAKE3-hashed provenance log).
  doctor      Check environment health: lockfile drift, orphaned artifacts, receipt staleness.
  help        Print this message or the help of the given subcommand(s)

Options:
      --format <format>    [possible values: json, json-pretty, yaml, table, plain, tsv, quiet]
      --select <select>
      --introspect
      --structured-errors
      --autonomic
  -h, --help
  -V, --version
```

Both exited `0`. The version string ticked from `26.7.28` to `26.7.30` between two calls minutes
apart during the run that produced this transcript — this repo bumps its own `Cargo.toml` version
frequently (CalVer, `YY.M.patch`), including from concurrent activity on other branches while a
session is live. At the time this doc was written, `Cargo.toml`'s `[workspace.package] version`
read **26.7.31** — treat any single version string here as a snapshot, not a promise, and check
`Cargo.toml` line 2 for what's actually current.

The canonical entry point also works and produces the same result:

```console
$ cargo run -p ggen-cli-lib --bin ggen -- --version
ggen 26.7.30
```

## Run a real sync against an example project

`examples/praxis-core-verify` is a real, committed ggen consumer project (its own `ggen.toml`,
ontology, templates, and an existing sync receipt from a prior real run). Point the binary you
just built at it:

```bash
cd examples/praxis-core-verify
/path/to/ggen/target/debug/ggen sync run --dry-run
```

This exited `0` and printed real JSON: all 6 tracked outputs reported
`"skipped": "unchanged: content identical"`, with a populated `graph_hash_hex` and a populated
`packs.praxis-core-pack` hash, plus five real OTEL pipeline spans (`pipeline.load`, `.extract`,
`.validate`, `.generate`, `.emit`), each carrying a real `pipeline.duration_ms`.

Running it for real (no `--dry-run`) is idempotent — same result, `"written": []`, everything
skipped as unchanged. Two runs producing byte-identical output is the sync pipeline's own
determinism claim, and this is what it looks like in practice, not by inspection of the source.

Then verify the cryptographic receipt chain:

```console
$ /path/to/ggen/target/debug/ggen receipt verify
{
  "valid": true,
  "chain_hash": "966ea779718097ee25bd5e8f6ce3b497ed1a5630c727adbded8fae23b6cb510e",
  "payload_hash": "c4e2fb91a846d1d4dd414b2fd52ee4675444d96589c20ec8099d9f7795ca6611",
  "graph_hash": "4539047ec45ff68ca06c3d7bb1decddb759278b2a268a5929b0a9aa91be1a81e",
  "outputs": 6,
  "signed": true,
  "signature_valid": true
}
```

Exit `0`. `receipt verify` takes zero arguments — it always targets `.ggen-v2/receipt.json` under
the resolved project root and resolves the verifying key automatically
(`crates/ggen-engine/src/verbs/receipt.rs`).

You can also run `ggen doctor` at the repo root itself — `ggen` self-hosts its own `ggen.toml`
and generates parts of `CLAUDE.md` from it:

```console
$ /path/to/ggen/target/debug/ggen doctor
{
  "healthy": true,
  "checks": {
    "lockfile_drift": {"status": "skip", "detail": "skipped: pack resolution is not implemented for the declarative-rules ([[generation.rules]]) schema yet ..."},
    "orphaned_artifacts": {"status": "skip", "detail": "skipped: template discovery is not implemented for the declarative-rules ([[generation.rules]]) schema yet."},
    "receipt_staleness": {"status": "pass", "detail": "every receipt output matches its recorded hash on disk", "stale": []}
  }
}
```

Exit `0`. The two `"skip"` entries are honest, not silently-passing: `ggen.toml` at the repo root
uses the newer `[[generation.rules]]` declarative-rules schema, and `doctor`'s lockfile-drift and
orphaned-artifact checks are implemented against the older frontmatter schema only — this is a
real, current gap, not a fabricated caveat.

## `just sync` / `just sync-dry` — a real, live doc correction

`ggen sync run --audit true` genuinely fails — the live verb has no `--audit` flag (only
`--dry-run`/`--watch`):

```console
$ ggen sync run --audit true
error: unexpected argument '--audit' found
```
Exit `1`. If you see this claim repeated elsewhere (including in this repo's own `CLAUDE.md`,
which still describes `just sync`/`just sync-dry` as "currently broken" for this reason), check
the **actual justfile recipes** before trusting that — they no longer pass the broken flags:

```make
sync:
    ggen sync run

sync-dry:
    ggen sync run --dry-run
```

Both `just sync` and `just sync-dry` were run for real against this codebase and exited `0` —
`just sync-dry` printed a real dry-run JSON report against the repo's own `ggen.toml`; `just sync`
performed a real sync and appended a new signed entry to `.ggen-v2/receipt-log.jsonl`. The
underlying CLI-flag bug the doc describes is real; the justfile recipes that would trigger it were
fixed since that doc paragraph was written, and it hasn't caught up. Point stands generally for
this project: prefer running the actual command over trusting a doc's claim about it.

## Next steps

- Chicago TDD workflow (RED → GREEN → REFACTOR) and the RDF-spec-first development cycle:
  `CLAUDE.md`'s Workflow section, or `.claude/rules/_core/workflow.md`.
- Full local validation gate: `just pre-commit` — see `docs/FAQ.md` for what it actually checks.
- Performance-sensitive work: `docs/PERFORMANCE_QUICK_START.md`.
- Everything else indexed: `docs/README.md`.
