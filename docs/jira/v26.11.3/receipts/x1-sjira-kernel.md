# X1 — ggen-sjira admission kernel (core slice)

Repo: /Users/sac/ggen. Base: pre-existing tree at session start (no git ops run, per
lane contract). Date: 2026-11-03.

## Files created (owned)

- `crates/ggen-sjira/` — new crate
  - `Cargo.toml` — deps exactly serde / serde_json / sha2 / thiserror
  - `src/lib.rs` — `#![forbid(unsafe_code)]`, IO-free, module docs naming port
    coverage and deferred slices
  - `src/work_order.rs` — WorkOrder (serde, `deny_unknown_fields`), Standing
    enum (UNKNOWN/PARTIAL_ALIVE/ALIVE/BLOCKED/BUILD_BROKEN/UNSUPPORTED + `REFUSED(<reason>)`
    round-trip), canonical sorted-key JSON encoding, `digest_of` (sha256
    `sha256:<64hex>`), `definition_digest` over the closed definition subset
    (standing excluded — standing changes never move it)
  - `src/admit.rs` — `admit_work_order` port: required-field/blank checks,
    hand-rolled validators (no regex crate, dep floor held): 40-hex sha,
    `sha256:<64hex>` digest, owner/repo, absolute-IRI origin authority,
    non-empty string lists, 15-class projection vocabulary, standing-at-UNKNOWN
    law, authority stamped `NONE`; typed refusal enum with remediation Display
    (MalformedWorkOrder, StandingNotUnknown, InvalidOriginAuthority,
    DigestMismatch, InvalidRepository, InvalidSha, Refusal::InvalidList,
    UnsupportedProjectionType, Deserialization)
  - `src/generated.rs` — placeholder for the future ontology-projection surface
- `crates/ggen-cli/src/cmds/sjira/mod.rs` — `sjira` noun, `admit` verb
  (`ggen sjira admit <file>`), refusal → `REFUSED:SEMANTIC_JIRA: <display>` argument error
- `docs/jira/v26.11.3/receipts/x1-sjira-kernel.md` — this receipt

## Files edited (disclosed beyond the base contract)

1. `/Users/sac/ggen/Cargo.toml` — ONE line: `  "crates/ggen-sjira",` added to
   `[workspace] members`. DISCLOSED per contract.
2. `crates/ggen-cli/src/cmds/mod.rs` — `pub mod sjira;` + sjira noun-registry
   static (the 2 disclosed lines' worth of wiring).
3. `crates/ggen-cli/Cargo.toml` — `[dependencies.ggen-sjira]` path dep (not in
   the ownership contract but mechanically required for the noun to reach the
   kernel; disclosed as the third disclosed touch).

## Port-coverage table

| igniter capability (semantic_jira.ex) | status |
|---|---|
| `@required` field set + `deny_unknown_fields` | PORTED (WorkOrder struct, 16 required fields incl. `subject`) |
| standing vocabulary + `REFUSED(...)` form | PORTED (Standing enum, parse/as_str round-trip) |
| regexes (sha/digest/repo/IRI) | PORTED as hand-rolled validators (regex not in dep floor) |
| `admit_work_order/1` typed refusals | PORTED (Refusal enum, remediation Display per house law) |
| standing-at-UNKNOWN admission law | PORTED |
| authority stamped NONE | PORTED (AdmittedWorkOrder.authority = "NONE") |
| canonical digest + definition_digest (closed subset, standing excluded) | PORTED (digest_of / definition_digest) |
| authority index / origin resolution (Authority.resolve) | DEFERRED — later slice |
| frontier / schedule / lease conflict fencing | DEFERRED — later slices |
| lease_request / execution_package / do_intent | DEFERRED — later slices |
| promote + transition apply/append/project (event-sourced standing) | DEFERRED — later slices |
| replay_check / semantic_diff | DEFERRED — later slices |
| repair_work_order / composition_subject / machine_experience / view / render_projection | DEFERRED — later slices |
| `@definition_fields` beyond the struct's 16 fields (dependencies, required_receipt_classes, path_scope, authority_requirement) | DEFERRED — struct grows in the dependencies slice |

## Verification (real output)

- `cargo test -p ggen-sjira`: 5 lib + 12 integration tests pass (17 total, 0 failed).
  Covers the igniter admission CASES: valid admit (authority NONE, digest shapes),
  missing-field refusal, unknown-field refusal (deny_unknown_fields), bad-regex
  refusals (sha / repo / origin IRI / replay digest), standing-not-unknown refusal,
  REFUSED(...) vocabulary, digest determinism (key-order independence), and
  standing-moves-snapshot-not-definition law.
- `cargo check -p ggen-cli-lib`: green (Finished dev profile, 0 errors) with the
  sjira noun wired.

## Status

ALIVE for the core slice; deferred slices UNBUILT (not claimed).
