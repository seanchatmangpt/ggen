# Session Learnings — v26.11.3 wave (ggen)

Non-obvious, recurring-class facts learned the hard way this wave. Project-specific
(global counterparts live in ~/.claude/CLAUDE.md).

## FM-LAW-015 EA admission

- Every sync root needs `ea.graph.json` or the disclosed `--skip-ea-admission`
  opt-out (`sync.rs:346-351` — the gate's own sanctioned escape hatch; a skip is
  loud, never silent).
- Author graphs via `cargo run -p ggen-abb-sbb --example validate_ea_graph` —
  it prints kernel-computed `sbb_content_digest`/`contract_digest`/`graph_digest`
  for pinning. Author with placeholder digests first; the pre-pinning probe must
  refuse with typed `SBB_DIGEST_MISMATCH` (non-vacuity). Never empty-SBB bypass.
- Root classification decides the remedy: content packs whose authored-surface law
  pins an exact file set take the opt-out; roots that manufacture artifacts take a
  real graph. Check the consumer's verifier BEFORE committing evidence artifacts
  (per-consumer contracts differ: tcps commits its lock as evidence; CMD forbids
  committed `ggen.lock` entirely — authored surface is {ggen.toml, ontology.ttl}).
- Consumer `ggen.lock` is sync runtime state — never commit (the one lawful
  exception: evidence-contract consumers). CASR refusal = CONSUMER_AUTHORED_SURFACE_REFUSED.

## Generated-output drift

- `ggen sync run` minted/updated files must be committed in the same sweep as the
  change that regenerates them — untracked or stale generated files fail fresh
  checkouts (`ea.graph.json` class) or clobber-refuse (`FM-WRITE-005`).
- FM-WRITE-005 regen-by-delete loop: run sync, `rm` the named diverging file,
  repeat until exit 0 (each refusal names exactly one file).
- Templates ending `}}\n` leak a newline into renders. Tera has NO Jinja-style
  `-}}` on `{{ }}` expression tags (`trim_blocks` is `{% %}`-only) — fix is
  template restructure or engine-level `trim_blocks` with a two-sided render
  census including control-tag-EOL lines.
- Generated Rust must be rustfmt-clean BY TEMPLATE (CI pins nightly-2026-06-22
  `fmt --check`): a template emitting dirty bytes re-arms the fmt gate on every
  sync (ZD-15 flap). Fix the template, never hand-patch the output (GGM-SRC-002).
- CI gate order masks debt: the fmt step dying before clippy hid 56 pre-existing
  test lint errors. Budget for the cascade when clearing a first-gate failure.

## Self-host observation (revision-bound)

- The observer output is bound to the exact HEAD revision: any landing commit
  re-breaks the binding. Re-run `observe_self_host.py` at the terminal HEAD and
  commit the outputs as the closing re-pin.
- A C21-class constraint: the receipt cannot contain its own commit hash — a
  `--check` gate must compare against the observation commit, not literal HEAD.
- The observation re-derives on every run; CI syncs may clobber-refuse
  (`FM-WRITE-005`) if the committed copy embeds stale env-dependent facts.

## Premise rot (measured instances)

- Go ≥1.24 ships `crypto/mlkem` (FIPS 203 in stdlib) — a "handshake impossible
  from Go" blocker recorded under go1.23 vanished when the toolchain pinned 1.26.10.
- ZD-9's committed observation made the "prove seed fails closed" step succeed —
  the honest fix renamed the step to prove-verify, retaining the sed-sabotage
  fail-closed leg.
- ggen-scip had never been published; ggen-sjira carried a path-only dep on it
  (hard publish blocker until `version = "26.11.3"` is added).
- crates.io `max_version` for ggen/ggen-graph/ggen-config/ggen-marketplace is
  26.15.2 (July CalVer anomaly, older content) — publishing 26.11.3 never takes
  `max_version`; consumers must exact-pin `=26.11.3`; registry polls assert
  `versions[0]`, never `max_version`.
