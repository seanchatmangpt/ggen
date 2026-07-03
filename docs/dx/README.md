# Developer Experience — ggen

`just` is the single entry point for every dev task. Run `just --list` for
the full list with descriptions; the core loop:

| Command | Purpose | Speed |
|---------|---------|-------|
| `just check` | `cargo check --workspace` | <5s warm |
| `just build` / `just build-release` | debug / release binary | — |
| `just fmt` / `just fmt-check` | format / verify formatting | — |
| `just lint` | clippy, `-D warnings` | <180s |
| `just test` | full test suite (primary gate) | <30s warm |
| `just test-lib` | unit tests only | <10s warm |
| `just test-doc` | doctests (`/// Examples` blocks) | — |
| `just pre-commit` | fmt-check → check → lint → test-lib | <2min |
| `just doctor` | fast local health check | <1s, no network |
| `just doctor true` | + SLO benchmarks + observability probes | seconds |
| `just doc` | build API docs | — |
| `just bench` | criterion benchmarks | — |
| `just audit` | `cargo audit` | — |
| `just sync` / `just sync-dry` | ggen's own μ₁-μ₅ generation pipeline | — |

Niche or slow test suites (BDD specs, mutation testing, marketplace
compile-proof tests, Phase-2/coherence checks) aren't wrapped in `just` —
run the underlying `cargo test`/`cargo mutants`/`ggen graph validate`
command directly (see the comment block above `test-doc` in the
`justfile`). CI's `phase2` job runs the same commands verbatim.

## `ggen doctor`

One command, two modes:

- **`ggen doctor`** (default) — rust/cargo/git toolchain, marketplace DB
  openability, pack cache count, `ggen.toml`/`Cargo.toml`/`.specify`
  presence. All local, no network, sub-second.
- **`ggen doctor --all`** — adds SLO microbenchmarks (Oxigraph/Tera
  throughput) and observability-stack probes (Tempo/OTel/Jaeger). Opt-in
  because these are slow and/or require services running locally.
- **`ggen doctor --check <name>`** — run a single named check (e.g.
  `rust`, `git`, `slo`, `observability`) for scripting.

## No external build tool required

Everything routes through plain `cargo` via `just` recipes — no `cargo
make`, no external `cargo-cicd`/`affi` binaries. If a recipe needs a tool
beyond stock cargo (e.g. `cargo audit`, `cargo mutants`), the recipe itself
says so and the tool is a normal `cargo install`.
