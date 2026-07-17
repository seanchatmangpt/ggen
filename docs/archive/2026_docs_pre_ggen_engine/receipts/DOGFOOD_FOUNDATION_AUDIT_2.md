<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Dogfood Foundation Audit &#035;2](#dogfood-foundation-audit-2)
  - [SETTLEMENT-CHECK-2: HOLDS](#settlement-check-2-holds)
  - [Cracks &#035;55–#58: CLOSED + verified](#cracks-5558-closed--verified)
  - [Command-proof matrix reconciled to PROVEN](#command-proof-matrix-reconciled-to-proven)
  - [clap-noun-verb kebab decision](#clap-noun-verb-kebab-decision)
  - [Quarantined dead refs](#quarantined-dead-refs)
  - [PUBLIC-ONTOLOGY-FOUNDATION-1: DONE](#public-ontology-foundation-1-done)
  - [Receipt closure](#receipt-closure)
  - [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Dogfood Foundation Audit #2

> Follow-on to [Audit #1](DOGFOOD_FOUNDATION_AUDIT_1.md). Where #1 *found* the cracks
> (#55–#58), this iteration *closed and verified* all four, then settled the build and
> docs to match. Every status below is grounded in the actual test files, not narrated.

## SETTLEMENT-CHECK-2: HOLDS

The four dogfood cracks #55–#58 are all CLOSED and verified by real Chicago-TDD piers
under `crates/ggen-cli/tests/`. No phantom completion: each closure is backed by a test
that asserts observable durable state (lockfile entry, signed receipt, on-disk scaffold,
refusal).

## Cracks #55–#58: CLOSED + verified

| # | Crack | Status | Verifying evidence |
|---|-------|--------|--------------------|
| #55 | A2A `ggen_construct.rs` fake-success stub + dummy receipt (Oracle Gap) | CLOSED | fail-loud; no `// simulate success`, no hardcoded `rcpt-simulated-*` receipt |
| #56 | `packs_receipt` under-binds closure + fail-open | CLOSED | receipt binds the real pack closure (`pack:<id>@<version>:<digest>`); refuses to emit on empty digest (fail-closed) — asserted in `proof_pack_test.rs` |
| #57 | `pack add` does not write `.ggen/packs.lock` | CLOSED | `pack add` writes a non-empty lockfile entry with a `sha256-…` `integrity` digest. `proof_pack_test.rs` `EXPECT_LOCKFILE = true`, 10/10 |
| #58 | `init` lacks a behavioral Gall pier | CLOSED | `proof_init_test.rs` — a GALL boundary-preparation pier, 4/4 (scaffold / refuse-clobber / force-preserve / subdir) |

## Command-proof matrix reconciled to PROVEN

The KEEP/PROVE surface is now PROVEN by execution (counts read from the test files):

| Noun | Backing test | Count |
|------|--------------|-------|
| `init` | `tests/proof_init_test.rs` | 4 |
| `sync` | `tests/gall_sync_actuation.rs` | 5 |
| `graph` | `tests/proof_graph_test.rs` | 10 |
| `pack` | `tests/proof_pack_test.rs` | 10 |
| `policy`/`doctor`/`utils` | `tests/proof_policy_doctor_utils_test.rs` | 12 (7/2/3) |

`lsp` remains PROVEN (GATED) behind `--features lsp`. The 5 archived nouns
(`a2a`/`framework`/`mcp`/`sigma`/`wizard`) stay gated behind default-off `experimental`.

## clap-noun-verb kebab decision

The kebab-case noun/verb routing fix was made **upstream in clap-noun-verb** (commit
`313ba73`), **not** patched into ggen. Rationale: the local 26.5.28 WIP branch of
clap-noun-verb broke 6 `graph` tests + 1 `init` test by emitting pretty-printed JSON the
proof piers do not expect. Decision: do not vendor the WIP; land the fix later via a
minimal **published** clap-noun-verb release so the proof surface stays green.

## Quarantined dead refs

The dangling `cmds::market` / `cmds::project` references (Audit #1 §"Agent over-flag")
are quarantined — they live only in `tests/integration/` and `tests/cli/` subdirs not in
the compiled default target set, so they are not default-build hazards. Left as a noted
later-cleanup, not acted on this loop.

## PUBLIC-ONTOLOGY-FOUNDATION-1: DONE

The boundary now rests on public ontology footing — PROV-O / DCTERMS / SKOS — replacing
the private `gall:` namespace. The public footing is what `gall_sync_actuation.rs`
actuates from (a real public-ontology-aligned O\*).

## Receipt closure

With cracks #1–#3 of the boundary backlog closed (archive-gate, pack-add lockfile,
verifying build), the v26.5.28 release receipt now binds the full O\* closure —
`O-STAR-RECEIPT-CLOSURE-1` (see [boundary](../reference/release/v26-5-28-boundary.md)).

## See also

- [Audit #1](DOGFOOD_FOUNDATION_AUDIT_1.md) — the find pass (#55–#58)
- [Command-proof matrix](../reference/cli/command-proof-matrix.md) — per-noun PROVEN status
- [v26.5.28 boundary](../reference/release/v26-5-28-boundary.md) — backlog #1–#3 DONE
- [O-STAR-RECEIPT-CLOSURE-1](O_STAR_RECEIPT_CLOSURE_1_RECEIPT.md)
