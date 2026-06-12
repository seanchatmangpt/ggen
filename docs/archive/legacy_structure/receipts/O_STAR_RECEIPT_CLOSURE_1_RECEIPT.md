<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [O-STAR-RECEIPT-CLOSURE-1 Receipt](#o-star-receipt-closure-1-receipt)
  - [Final State](#final-state)
  - [The crack (exposed by SYNC-ACTUATOR-1)](#the-crack-exposed-by-sync-actuator-1)
  - [The fix](#the-fix)
  - [Evidence — a real receipt's `input_hashes`](#evidence--a-real-receipts-input_hashes)
  - [The four false-consequence exclusions (all hold)](#the-four-false-consequence-exclusions-all-hold)
  - [Proven by](#proven-by)
  - [Remaining (toward the rest gate)](#remaining-toward-the-rest-gate)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# O-STAR-RECEIPT-CLOSURE-1 Receipt

## Final State

**O-STAR-RECEIPT-CLOSURE-1 COMPLETE** — the sync receipt binds the full `O*`, making
`R ⊢ A = μ(O*)` true to the equation rather than to the manifest alone.

> The artifact proves nothing by itself. The receipt proves nothing unless it binds `O*`.

## The crack (exposed by SYNC-ACTUATOR-1)

`emit_sync_receipt` hashed only `ggen.toml` (+ installed packs). The ontology and the
external template — both of which determine the artifact — were **outside the witnessed
closure**. So `R ⊢ A = μ(O*)` was actually `R ⊢ A = μ(ggen.toml)`: a receipt that could
not detect a changed ontology or template.

## The fix

`crates/ggen-cli/src/cmds/sync.rs` `emit_sync_receipt` step 4 now binds the full closure
(single fix point — covers both the manifest and low-level sync paths):

- **actuator identity** — `actuator:ggen-sync@<version>` (which μ produced this)
- **manifest** — `ggen.toml:<sha256>`
- **ontology** — `<ontology.source>:<sha256>` + every `[ontology] imports` entry
- **external query files** — each `query = { file = ... }`
- **external template files** — each `template = { file = ... }`
- inline inference rules / queries / templates live inside `ggen.toml` → already bound
- a closure input that cannot be read is recorded `<path>:MISSING` — never silently
  dropped (a verifier must see the gap)

## Evidence — a real receipt's `input_hashes`

```
actuator:ggen-sync@26.5.28
ggen.toml:bd88373ae52a9881edfeb9329a9ea0a17a379b49d606f20a214a5c8c88445ff4
ontology/command-foundation.ttl:8c18a21789f8d66aef62d796d5c68fb8cc6c1fce6b80f19622bc982d48f7eea0
templates/gall_command_foundation.rs.tera:7e2fa95bcdab760b0aa36be5a51396a44a2e6ecfc5ab4e4aa32cca9347795aac
output: ./generated/gall_command_foundation.rs:50e6bb20b116e87e7b452dd3aa950b4ee3c829fc5d8c6396a1b3ad4cd439d1f2
```

## The four false-consequence exclusions (all hold)

| Path | Receipt? |
|------|----------|
| dry-run | **no** (non-actuating preview — fixed in 42fe8ccf) |
| refusal (incapable boundary) | **no** (gated before actuation) |
| real actuation | **yes** — binds full `O*` + actuator + output |

## Proven by

`crates/ggen-cli/tests/gall_sync_actuation.rs::gall_receipt_binds_full_o_star_closure`
(5/5 in the actuation suite): asserts the receipt's `input_hashes` contains the manifest,
the ontology (`.ttl`), the template (`.tera`), the actuator identity, and no `MISSING`.

## Remaining (toward the rest gate)

- Whole-workspace `cargo make check && lint && test && slo-check && audit`.
- GALL-COMMAND-FOUNDATION-1 (manufacture the command-foundation pack via sync).
- Capability-map resumes only after the floor holds.
