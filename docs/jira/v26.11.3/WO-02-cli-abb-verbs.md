# WO-02: CLI first-class surface for ABB admission kernel (`ggen ea`)

- **Date**: 2026-11-03
- **Repo**: /Users/sac/ggen
- **Lane**: WRITER (spec) → IMPLEMENT lanes consume this order
- **Standing**: UNVERIFIED (spec written from task-grounded reads of
  `crates/ggen-cli/src/cmds/sbb/mod.rs` and `crates/ggen-abb-sbb/examples/admit_demo.rs`;
  no acceptance command has been run at spec time)

## Subject

Wire the ggen-abb-sbb kernel's plan/admit/manufacture/replay loop into the ggen CLI as a
first-class consumer surface, replacing the example binary as the only runnable entry point,
and end the two-features-one-name collision with the existing `sbb` noun.

## Ground truth (measured at spec time)

- The ONLY runner of the kernel today is the example binary:
  `cargo run -p ggen-abb-sbb --example admit_demo`
  (`crates/ggen-abb-sbb/examples/admit_demo.rs`).
- The existing CLI noun `ggen sbb`
  (`crates/ggen-cli/src/cmds/sbb/mod.rs:193-295`) carries verbs
  `schema | inspect | validate | distribution | receipt | replay` and is a
  capability-density feature: it imports NOTHING from `ggen_abb_sbb`. The name collision
  (`sbb` vs the ABB/SBB admission kernel) is a naming accident, not shared code.
- Real fixtures exist at `fixtures/ea-graph.json` (used by `admit_demo.rs`).

## Resolution R6 (naming)

- `ggen sbb` KEEPS capability-density semantics unchanged; its help text gains a
  one-line cross-reference: "admission kernel lives under `ggen ea`".
- `ggen ea` OWNS admission (kernel consumer surface). No verb moves between nouns.
- Both nouns' long-help `about` strings must disambiguate explicitly.

## Task

New CLI noun `ggen ea`, cobra-style per-verb files under
`crates/ggen-cli/src/cmds/ea/`, one file per verb, thin — all kernel logic stays in
`ggen_abb_sbb`; the CLI only parses a graph + Request and maps results to exit codes.

### Verbs

| Verb | Effect |
|---|---|
| `ea plan` | Load graph, run kernel planning, print the plan (or `--json`) |
| `ea admit` | Build a Request from flags, run kernel admission; on Refusal print typed code + detail |
| `ea manufacture` | Execute the admitted plan against the graph |
| `ea replay` | Replay a prior manufacture receipt and compare digests |
| `ea verify-receipt` | Validate a receipt file's digest/identity fields |

### Request flags (shared where applicable)

`--abb <path>`, `--sbb <path>`, `--authority <id>`,
`--expected-graph-digest <hex>`, `--generation <n>`,
`--baseline-generation <n>`, `--json`.

`--json` on any Refusal-emitting verb emits the typed refusal code + detail as JSON on
stdout (human text otherwise). Structure mirrors the kernel's Refusal serialization.

### Exit-code contract

Map the kernel's 26 Refusal variants onto the house contract:

| Exit | Meaning |
|---|---|
| 0 | Admitted / verified |
| 1 | Kernel Refusal (deterministic admission rejection; typed code printed) |
| 2 | Usage / input error (bad flag, unreadable graph, malformed JSON) |
| 3 | Internal / unexpected kernel error (panic, invariant violation) |

The mapping must be a single total function `refusal_variant -> exit 1 + code` —
every one of the 26 variants enumerated explicitly; no catch-all default that
silently maps an unknown variant to 0.

## Acceptance (command-cited)

1. End-to-end demo loop against the real fixture exits 0:
   ```
   cd /Users/sac/ggen && cargo run -p ggen-cli -- ea plan --abb fixtures/abb.json --sbb fixtures/sbb.json --json && cargo run -p ggen-cli -- ea admit ... && cargo run -p ggen-cli -- ea manufacture ... && cargo run -p ggen-cli -- ea replay ...
   ```
   Exact flag wiring pinned during implementation from
   `crates/ggen-abb-sbb/examples/admit_demo.rs`; the loop must exercise
   plan -> admit -> manufacture -> replay with no manual intermediate edits.
2. Every kernel Refusal variant has a pinned test asserting its exit code AND
   its typed `--json` code. A variant with no test = acceptance failure.
3. `cargo run -p ggen-cli -- ea plan --help` (and each verb) shows a usage
   example line.
4. `ggen sbb` behavior unchanged: existing sbb test suite still passes
   (`cargo test -p ggen-cli sbb`), plus the R6 cross-link line present in its help.
5. Zero imports added from `ggen_abb_sbb` into the `sbb` noun module.

## Constraints

- Kernel (`crates/ggen-abb-sbb/src/`) untouched — consumer only. If the kernel lacks a
  needed public API (e.g. typed refusal enumeration), file a separate order; do NOT patch
  the kernel inside this one.
- Cobra-style per-verb files: `crates/ggen-cli/src/cmds/ea/{plan,admit,manufacture,replay,verify_receipt}.rs` + `mod.rs`.
- No mocks, no stub fixtures — all acceptance runs hit `fixtures/ea-graph.json` and the
  real kernel.
- Existing `sbb` noun module not refactored beyond the R6 help line.

## Falsifiers

- F1: the demo loop (Acceptance 1) exits nonzero at any stage.
- F2: injecting an unknown/future Refusal variant yields exit 0 or a bare exit 1 with
  no typed code (proves the mapping was not total).
- F3: `ea replay` on a tampered receipt exits 0.
- F4: any test asserting only that a function was called (no exit/JSON assertion) —
  vacuous coverage; acceptance requires exit+code assertions.
- F5: `cargo test -p ggen-cli sbb` regresses after the R6 help edit.

## Fences

- Chesterton: `ggen sbb` is a different, older capability-density feature — do not "fix"
  the collision by merging or deleting either noun; R6 partition only.
- Do not re-implement admission logic in the CLI layer; any logic pull is a kernel
  API gap, escalated as a new order.
- `--json` is additive structured output; never the only output mode.
- Do not rename `ggen sbb` verbs or change their exit codes (compat surface).

## Standing

UNVERIFIED. Becomes PARTIAL_ALIVE on Acceptance 1 (demo loop exit 0 receipt), ALIVE on
Acceptance 1-5 all receipted on the same commit.
