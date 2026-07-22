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
`0`, with 0 compiler errors and **14** real warnings (13 in `ggen-engine`, 1 in `bcinr-pddl`) —
tail of the real output. A naive `grep -c '^warning:'` over the raw log returns 19, not 14; the
other 5 lines are 2 build-script notices (`ggen@...: Discovered N templates/ontologies`, printed
by `ggen`'s own `build.rs`, not a compiler warning), 1 unrelated Cargo notice
(`profiles for the non root package will be ignored`), and the 2 per-crate summary lines
(`` `ggen-engine` (lib) generated 13 warnings``, `` `bcinr-pddl` (lib) generated 1 warning``)
double-counting warnings already tallied above. Verified reproducibly across two consecutive
builds on this branch (identical warning text and count both times):

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
ggen 26.7.31

$ ./target/debug/ggen --help
Usage: ggen [OPTIONS] [COMMAND]

Commands:
  init        Initialize a new ggen project with default structure and scripts.
              Creates a minimal, working ggen project scaffold with:
              - ggen.toml configuration
              - schema/domain.ttl (RDF ontology with example)
              - Makefile (setup, build, clean targets)
              - scripts/startup.sh (project initialization script)
              - templates/ (empty, ready for custom Tera templates)
              - output directory (current directory by default)
              [... full multi-section help text for `init` continues here; see `ggen init --help` ...]
  utils       Utils Commands - clap-noun-verb v3.4.0 Migration
  graph       Validate RDF/Turtle ontology graphs against praxis vocabulary constraints.
  doctor      Check environment health: lockfile drift, orphaned artifacts, receipt staleness.
  law         Law-state operations on the project graph: load rules, materialize, validate gates, explain derivations, export.
  agent       Agent noun — the AGI-facing CLI surface over `crate::agent::PackAgent`.
  pack        Pack Commands (singular alias for `packs`)
  packs       Packs noun — lockfile-oriented, multi-pack project management (`ggen packs <verb>`).
  policy      This module provides policy management commands wired to the marketplace layer.
  ontology    Ontology Commands - Embedded and Marketplace Ontology Management
  capability  Capability noun — resolve and enable capability surfaces (`ggen capability <verb>`).
  sync        Run the ggen code-generation pipeline: resolve, enrich, extract, render, write.
  receipt     Inspect and verify sync receipt chains (BLAKE3-hashed provenance log).
  help        Print this message or the help of the given subcommand(s)

Options:
      --format <format>    Output format [possible values: json, json-pretty, yaml, table, plain, tsv, quiet]
      --select <select>    Select/project nested JSON output using JSONPath, key selection, or JMESPath query projections
      --introspect         Introspect CLI capabilities as JSON Schema array for LLM tool-calling
      --structured-errors  Output errors using StructuredError format
      --autonomic          Enable autonomic features and output structured errors
  -h, --help               Print help
  -V, --version            Print version
```

Both exited `0`. Two things worth flagging about this transcript, both verified by actually
running `--help` repeatedly (6 consecutive invocations of the same unchanged binary), not assumed:

- **The order of the noun list (everything between `init` and `help`) is not stable across
  invocations.** `init` is always first and `help` always last, but the 12 nouns in between came
  back in 6 different orders across 6 consecutive runs of the identical binary — consistent with
  unordered (hash-map-based) command registration rather than a fixed list. Don't treat the order
  shown above as a contract; it's one real, valid snapshot, not *the* order.
- **`init`'s own listed description is long** — a multi-paragraph usage/flags/output walkthrough,
  reproduced identically on every run — not the short one-liner a `--help` summary might lead you
  to expect. Elided above for length; run `ggen init --help` (or `ggen --help` itself) to see it
  in full.

The version string ticked from `26.7.28` to `26.7.30` at one point during the session that
originally produced this page, then to `26.7.31` by the time of the most recent re-verification —
this repo bumps its own `Cargo.toml` version frequently (CalVer, `YY.M.patch`), including from
concurrent activity on other branches while a session is live. Treat any single version string
here as a snapshot, not a promise, and check `Cargo.toml` line 2 for what's actually current.

The canonical entry point also works and produces the same result:

```console
$ cargo run -p ggen-cli-lib --bin ggen -- --version
ggen 26.7.31
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
and generates parts of `README.md`/`CLAUDE.md` from it:

```console
$ /path/to/ggen/target/debug/ggen doctor
{
  "healthy": true,
  "checks": {
    "receipt_staleness": { "status": "pass", "detail": "every receipt output matches its recorded hash on disk", "stale": [] },
    ...
  }
}
```

Exit `0`. This page is itself part of the same first-principles rewrite that produced this
`README.md`, and while writing it we found and fixed a real defect in this exact interaction, worth
disclosing rather than hiding now that it's resolved: `ggen.toml`'s `docs-readme-region`
generation rule (`output_file = "README.md"`) uses `mode = "Merge"` — it only ever replaces the
single `<<<<<<< GENERATED ... ======= ... >>>>>>> MANUAL` region in the target file, leaving every
byte outside those markers untouched. An earlier commit on this same rewrite branch replaced
`README.md` wholesale with hand-authored prose carrying no such markers, which made
`receipt_staleness` fail exactly as this section used to describe. Merging this branch against a
`main` that had moved forward surfaced the real conflict, and fixing it properly (restoring the
`GENERATED`/`MANUAL` markers around the version/toolchain paragraph, not just papering over the
symptom) is what makes the transcript above pass.

**The underlying engine behavior is still real and worth knowing, independent of this page's own
now-fixed history:** tested directly against a throwaway copy, a real (non-`--dry-run`) sync's
merge step, given a target file with no `<<<<<<< GENERATED`/`>>>>>>> MANUAL` markers to merge
into, does not refuse or warn — it silently replaces the **entire file** with just the
freshly-rendered generated region plus a placeholder manual comment (observed: took a 114-line
`README.md` down to 10). If you ever hand-edit a `mode = "Merge"` target and remove its markers,
`ggen sync run` (without `--dry-run`) will destroy that content silently rather than catching the
mistake — a real Fail-Open gap in the write engine, not merely a hypothetical. `ggen sync run
--dry-run` / `just sync-dry` are safe in every case tested here — verified separately, exit `0`,
correctly report a `planned: write (dry-run)` outcome without touching the file.

Separately, `crates/ggen-cli/tests/generated/cli_proof_tests.rs` is `.gitignore`d
(`generated/` — line 158) and only ever produced by a real (non-`--dry-run`) `ggen sync run` at
the repo root. On a fresh clone of `main` that hasn't run one yet, `doctor` reports it missing
until the first real sync creates it — not a defect, just a precondition worth knowing before
trusting `doctor`'s output on a brand-new checkout.

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

Both recipes were verified for real, but **not against this branch's own working tree** — see the
warning two sections up: a real `just sync` (or `ggen sync run` without `--dry-run`) at the ggen
repo root right now would overwrite this very file. `just sync-dry` is safe and was run directly
against this codebase: exit `0`, a real dry-run JSON report against the repo's own `ggen.toml`.
`just sync` (the real, writing variant) was verified against an isolated scratch copy of this
branch instead, precisely to avoid that risk: exit `0`, and it did perform a real sync and append
a new signed entry to `.ggen-v2/receipt-log.jsonl` there — confirming the CLI-flag bug fix
(`--audit true` isn't reachable through the current justfile recipes) is real, without touching
the actual branch. The underlying CLI-flag bug the doc describes is real; the justfile recipes
that would trigger it were fixed since that doc paragraph was written, and it hasn't caught up.
Point stands generally for this project: prefer running the actual command over trusting a doc's
claim about it — just don't run the writing variant of `sync` at this repo's own root while
`README.md` lacks its merge markers.

## Next steps

- Chicago TDD workflow (RED → GREEN → REFACTOR) and the RDF-spec-first development cycle:
  `CLAUDE.md`'s Workflow section, or `.claude/rules/_core/workflow.md`.
- Full local validation gate: `just pre-commit` — see `docs/FAQ.md` for what it actually checks.
- Performance-sensitive work: `docs/PERFORMANCE_QUICK_START.md`.
- Everything else indexed: `docs/README.md`.
