# Book Schemas — Standing Note

This directory ships the book's own receipt/certification schemas. On 2026-07-19 they were
compared against the shipped reference schema in the host repository,
`packs/tcps-release-pack/reference/schemas/build-receipt.schema.json`. They are **different
schemas describing different receipt objects**, and the discrepancy is recorded here rather
than resolved silently — reconciling (or explicitly declaring both canonical) is an open
design question for the maintainer.

## `inspection-receipt.schema.json` vs the reference `build-receipt.schema.json`

| Aspect | Book `inspection-receipt` | Reference `build-receipt` |
|--------|---------------------------|---------------------------|
| `$id` | absent | `urn:tcps:schema:build-receipt.schema.json` |
| `title` | "Level Five Inspection Receipt" | "TCPS Build Receipt" |
| Required set | `subject, version, inputs, checks, outcome` | `schema, step, recorded_at, source_digest, status` |
| Overlap of required fields | none — the two required sets are disjoint | — |
| Version pinning | `version` is `const "v26.7.19"` | no version field; free-form `schema` string |
| Digest field | none | `source_digest` with pattern `^[0-9a-f]{64}$` |
| Timestamp | none | `recorded_at` (`date-time`) |
| Result field | `outcome` enum: `verified, refused, bounded, unsupported, inconsistent` | `status`: unconstrained string |
| Extra fields | not restricted (`additionalProperties` unset, defaults open) | explicitly `additionalProperties: true`; optional `target`, `product`, `tier` (integer 1..3) |

Consequence: a receipt valid under one schema is invalid under the other (each requires
fields the other never emits). The book's chapters that reference "inspection receipts" and
the release pack's build receipts are therefore two distinct proof objects, not one schema
with drift.

`level-five-certification.schema.json` has no counterpart in
`packs/tcps-release-pack/reference/schemas/` and was not part of this comparison.
