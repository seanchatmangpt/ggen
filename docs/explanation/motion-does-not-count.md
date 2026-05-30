<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Motion Does Not Count](#motion-does-not-count)
  - [The distinction](#the-distinction)
  - [The five ways motion masquerades as value](#the-five-ways-motion-masquerades-as-value)
  - [How value is proven](#how-value-is-proven)
  - [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Motion Does Not Count

> Explanation. The single hardest discipline in the Genesis Run, and the one most
> coding agents fail.

A command that exits `0` and prints "Success" has produced **motion**. Whether it
produced **value** is a separate question, answerable only by looking at durable state.

## The distinction

| | Motion | Value |
|---|--------|-------|
| Evidence | stdout, exit code, "it ran" | a file written, a row inserted, a hash changed |
| Reproducible? | Re-running prints the same message | Re-running is detectable as a second transition (or correctly idempotent) |
| Falsifiable? | No — narration cannot be refuted | Yes — the state is there or it is not |

A coding agent — human or machine — that counts motion as value will report a feature
"done" because the test passed and the command ran. But:

- The test proves the **test harness** works, not that the external service was called.
- The command exiting `0` proves it **did not crash**, not that it changed the world.

## The five ways motion masquerades as value

These are the mistake classes ggen patches are gated against
(`.claude/rules/coding-agent-mistakes.md`):

1. **Decorative completion** — `pack add` prints "Lockfile: .ggen/packs.lock" but the
   lockfile is never written. The world looks the same after as before.
2. **Epistemic bypass** — logic that should be *asked* of the ontology (a SPARQL query)
   is instead *hardcoded* inline. The codebase "knows" what it should only "ask."
3. **Fail-open** — a missing required resource produces a `warn!` and the run continues,
   instead of an `Err` that halts. (The SPARQL formatter that reformats a *broken* tree
   because the parser is error-tolerant — fixed in this release by gating on parse errors.)
4. **Legacy-path contamination** — the correct path was built but the old bypass was left
   reachable; being simpler, the bypass is what runs in practice.
5. **Contract drift** — the receipt or lockfile no longer describes what actually ran:
   an empty `signature`, a stale `input_hashes`, a hardcoded UUID.

## How value is proven

Only by **externalizable evidence** that survives the process:

- a file on disk with real bytes,
- a lockfile entry with a non-empty digest,
- a receipt with a real Ed25519 signature and current-run input hashes,
- an OTEL span proving the external call was made (`llm.complete`, real token counts),
- a Chicago-TDD test that **cannot pass unless the command did real durable work**.

> The easiest path to passing must be real execution. Faking must be harder than the
> real thing.

## See also

- [The Genesis Run](genesis-run.md) — why resting at `fill` (motion) is unlawful
- [Oracle Gaps](oracle-gaps.md) — motion-as-value, productised
- [Command proof matrix](../reference/cli/command-proof-matrix.md) — where each command's value is recorded
