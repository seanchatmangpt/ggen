# WO-06: Close the envelope-court scope gaps (digest binding, freshness, fallback observability)

- **Date**: 2026-11-03
- **Repo**: /Users/sac/ggen
- **Lane**: WRITER (spec) → IMPLEMENT lanes consume this order
- **Standing**: UNVERIFIED (spec written from task-grounded reads of
  `crates/ggen-abb-sbb/src/envelope_court.rs` (91 lines) and
  `crates/ggen-engine/src/envelope.rs:95-130`; no acceptance command has been
  run at spec time)

## Subject

The envelope court currently validates only cap coverage. Close three scope
gaps — envelope-vs-receipt graph digest agreement, ontology freshness
semantics, and observability for the engine's fail-soft cap fallback — and
register the exact-equality cap binding as law, not a bug.

## Ground truth (measured at spec time)

- `crates/ggen-abb-sbb/src/envelope_court.rs` is 91 lines. `envelope_court()`
  validates exactly ONE property: cap coverage. It resolves
  `cap_id = "{capability}.max_artifacts"` against `receipt.outputs.len()`,
  requires exact equality, and returns:
  - `Pass` on equality,
  - `VacuousGate` (exit 1) when the cap id is absent from the cap table,
  - `Refused { code: "CAP_CEILING_MISMATCH" }` (exit 2) on inequality.
- The court does NOT compare `env.graph_digest` with `env.receipt.graph_digest`.
  It does NOT check ontology freshness (or any timestamp/generation check) —
  those live elsewhere (`verify_receipt`, `crates/ggen-abb-sbb/src/lib.rs:1037`).
  Digest/identity validation exists in the kernel but is invisible to the
  envelope court, so an envelope whose graph digest disagrees with its own
  bound receipt passes the court today.
- The engine's envelope path is fail-soft by contract:
  `crates/ggen-engine/src/envelope.rs:107-113` documents that a missing
  reticle, a missing envelope individual, or malformed cap literals silently
  fall back to the built-in defaults, min-clamped. The intent is sound — caps
  gate is a ceiling, never an authority grant (refusing sync on a bad ceiling
  file would let a corrupt file deny-service the project) — but the fallback
  path is unobservable: nothing is logged or counted, so a corrupt reticle is
  silently absorbed.
- The fail-soft kernel is fail-loud (typed verdicts, `exit_code()` 0/1/2).
  The split is intentional; this order does not change it, only makes the
  soft half visible.

## Resolution R7 — graph-digest agreement, registered decision

- **Decision (pick one, cited)**: the envelope court checks
  `env.graph_digest == env.receipt.graph_digest` itself, at court time.
  Cited: RESOLUTIONS.md R5 (qualification is digest-bound only) and the
  court's existing one-surface role — the court is the single place that
  replays an
  envelope against its own receipt; delegating to `verify_receipt` (lib.rs:1037)
  would leave the court admitting envelopes whose own two halves disagree.
- **Verdict**: fold into the existing `Refused` variant with a distinct typed
  code `ENVELOPE_GRAPH_MISMATCH` (exit 2, same shell semantics as
  `CAP_CEILING_MISMATCH`). No new `ReplayVerdict` variant needed — `Refused`
  is already `{ code: String, detail: String }`, so the kernel types change is
  zero. (A dedicated variant is allowed under the compile-fail sealed pattern
  if the implementer prefers it, but is not required.)
- Ordering: the digest check runs BEFORE the cap-table lookup (identity before
  policy). A mismatched envelope is refused regardless of caps.

## Resolution R8 — ontology freshness: delegated, registered

- **Decision (pick one, cited)**: ontology-hash freshness semantics stay
  DELEGATED to the kernel's `verify_receipt` (lib.rs:1037). The envelope court
  does not duplicate freshness. Cited: lib.rs:1037 already owns
  timestamp/generation checks; duplicating them in the court creates the
  N-implementations-of-one-calculus drift class (O(N²) drift). This row is the
  registration the task requires.
- Requires a RESOLUTIONS row (R8) so the delegation is registered, not
  implicit — the court's doc comment must name `verify_receipt` as the
  freshness authority.

## Resolution R9 — fail-soft fallback observability

- `extract_caps` (crates/ggen-engine/src/envelope.rs) gains a counter/log at
  each fallback site: missing reticle file, missing envelope individual, and
  malformed cap literals each emit a `ENVELOPE_CAP_FALLBACK` event (structured
  log + counter, one per fallback kind) when a reticle exists but fails to
  yield caps — so a corrupt reticle is never silently absorbed.
- The fail-soft CONTRACT is unchanged: the function still returns defaults,
  still min-clamps, never refuses sync. Only visibility is added. A
  no-reticle case (no reticle file at all) is the legitimate common path and
  emits nothing; a present-but-corrupt reticle emits the counter.
- If the engine has no counter infrastructure, a `tracing` (or equivalent
  crate already in the engine's deps) warn-level structured event is the
  fallback implementation; do not add new deps to synthesize one.

## Resolution R10 — exact-equality cap binding is law

- The exact-equality requirement (`declared == measured`) is registered as
  law: a partial run needs a FRESH envelope minted against the fresh receipt —
  an envelope is a per-manufacture binding, not a reusable budget account.
  Document this in the court's module doc comment ("this exactness is law,
  not a bug; partial runs mint a fresh envelope"). Do not loosen to
  `<=`, do not add slack, no tolerance.

## Task

### 1. Digest agreement check (kernel)

`crates/ggen-abb-sbb/src/envelope_court.rs` — `envelope_court()` gains, first
thing it does, a digest-agreement branch:

```rust
if env.graph_digest != env.receipt.graph_digest {
    return ReplayVerdict::Refused {
        code: "ENVELOPE_GRAPH_MISMATCH".into(),
        detail: format!(
            "envelope graph_digest {} != receipt graph_digest {}",
            env.graph_digest, env.receipt.graph_digest
       ),
    };
```

Kernel types unchanged (Refused already carries `code`/`detail`). Update the
module doc comment: verdict list gains the ENVELOPE_GRAPH_MISMATCH row and the
R10 exactness-is-law paragraph.

### 2. Freshness delegation registration

Court module doc comment names `verify_receipt`
(`crates/ggen-abb-sbb/src lib.rs:1037`) as the ontology-freshness authority
(R8). No code change beyond the doc comment.

### 3. Fallback observability (engine)

`crates/ggen-engine/src/envelope.rs` — instrument each fallback site of
`extract_caps` (a missing reticle file, a missing envelope individual,
malformed cap literals) with one structured warn-level event per fallback
kind:

- `code = "ENVELOPE_CAP_FALLBACK"` plus a distinguishing `kind` field
  (`missing_reticle` / `missing_individual` / `malformed_literals`);
- warn level, no panic, no refusal — the fail-soft contract is untouched;
- if the engine already has counter infrastructure, emit the counter too;
  otherwise use the engine's existing structured-log crate (e.g. `tracing`
  if already in deps). Do NOT add new dependencies to synthesize one.

The legitimate common path (no reticle file at all) emits nothing; a
present-but-corrupt reticle emits the event. The implementer writes real
Rust against the actual `ENVELOPE_RETICLE_REL` constant and the engine's
existing logging infrastructure — the spec text (R9) is normative, not any
sketch.

### 4. Exact-equality law documentation

Court module doc comment gains the R10 paragraph: exact equality
(`declared == measured`) is law; a partial run mints a fresh envelope; do not
loosen to `<=` or tolerance.

## Acceptance (command-cited, mutation courts)

Mutation courts — a check whose removal does not fail a test is vacuous:

1. **Digest check is load-bearing**: remove the `ENVELOPE_GRAPH_MISMATCH`
   branch from `envelope_court()` → a pinned test FAILS. Pin test:
   construct an envelope with `graph_digest != receipt.graph_digest`, assert
   verdict is `Refused` with code `ENVELOPE_GRAPH_MISMATCH` and
   `exit_code() == 2`. Without the branch the verdict would be `Pass`/
   cap-verdict, so the test flips. Receipt command:
   `cargo test -p ggen-abb-sbb envelope_court`
2. **Fallback counter is load-bearing**: corrupt a reticle on a real temp
   copy of a project root (malformed cap literal, e.g. `res:maxArtifacts "x"`)
   and assert the `ENVELOPE_CAP_FALLBACK` event/counter fires (assert on
   captured structured log or counter value, never call-count of a mock —
   assert the emitted artifact). Receipt command:
   `cargo test -p ggen-engine envelope`
3. **Exit codes unchanged**: `Pass=0 / VacuousGate=1 / Refused=2` mapping in
   `ReplayVerdict::exit_code()` — existing pinned tests still pass unchanged:
   `cargo test -p ggen-abb-sbb envelope_court` (includes exit-code pins).
4. `cargo test -p ggen-abb-sbb && cargo test -p ggen-engine` green.

## Falsifiers

- F1: removing the digest branch (Acceptance 1's mutation) leaves all tests
  green — proves the check was vacuous.
- F2: corrupt-reticle test passes WITHOUT the counter emission (proves the
  test asserted call-shape, not emitted artifact).
- F3: any acceptance cites a mock/assert-on-interaction rather than emitted
  artifact — vacuous, banned.
- F4: `ReplayVerdict::exit_code()` mapping changed (0/1/2 pins regress).

## Fences

- **Do not weaken fail-soft** where documented intentional
  (envelope.rs:107-113): `extract_caps` still returns defaults, still
  min-clamps, never refuses on a corrupt reticle. Only observability is
  added, never refusal.
- Kernel types unchanged unless the verdict enum gains a variant (allowed;
  compile-fail sealed pattern preserved). `Refused { code, detail }` already
  accommodates `ENVELOPE_GRAPH_MISMATCH` with zero type change.
- Court is identity-before-policy: digest check precedes cap lookup.
- Freshness stays at `verify_receipt` (lib.rs:1037) — the court must NOT
  duplicate timestamp/generation checks (R8 delegation).
- Chicago-style: assert on real emitted artifacts (verdicts, structured log
  lines, counter values), never mock call-counts.

## Files

| Path | Change |
|---|---|
| `crates/ggen-abb-sbb/src/envelope_court.rs` | digest branch + doc comment (R7/R8/R10) |
| `crates/ggen-abb-sbb/tests/envelope_courts.rs` | pinned mismatch + exit-code tests |
| `crates/ggen-engine/src/envelope.rs` | `ENVELOPE_CAP_FALLBACK` instrumentation (R9) |
| `docs/jira/v26.11.3/RESOLUTIONS.md` | R7-R10 rows (coordinator-owned file — deliver rows to coordinator for commit) |
