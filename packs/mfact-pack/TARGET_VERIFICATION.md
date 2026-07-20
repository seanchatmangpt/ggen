# Target verification (mfact-pack)

Dated hand-verification note, closing the Target-API-fidelity L1->L2 gap
named in `docs/packs/L5_VALIDATION_REPORT.md`'s mfact-pack section ("there
is not even an L2-level 'verified by hand against one target version,
once'").

- **Verified on:** 2026-07-18
- **Target repo:** `~/mfact` (real, in-the-wild checkout on this machine)
- **Target commit:** `801abf7933dabf5c95f9fb18ff21a7a8a1f6a564` (`git -C ~/mfact rev-parse HEAD`, committed 2026-07-16)
- **What was checked:** `~/mfact/README.md`'s "Quickstart" and "What's here"
  sections against every fact this pack's `ontology.ttl` asserts:
  - The 3-stage pipeline (`ggen` projects, `Lean` admits, `mfact`
    certifies) matches the README's opening paragraph verbatim.
  - The 5 top-level authority directories (`packs/`, `procint/`, `paper/`,
    `release/`, `.mfact/artifacts.toml`) and their one-line descriptions
    match the README's "What's here" bullet list.
  - `~/mfact/packs/` on disk at this commit contains exactly
    `lean-math-pack`, `post-release-pack`, `quadrature-pack` — consistent
    with `mfa:Project`'s `rdfs:comment` ("mfact's own packs/ (lean-math-pack,
    quadrature-pack, post-release-pack)").

## What this does NOT close

This is the L2 bar only ("verified by hand against one target version,
once, dated note"), not L3+. mfact/procint does not expose its own
machine-readable ontology this pack could directly track (no `.ttl`/RDF
export of its pipeline semantics under `~/mfact`) — reaching L3 ("generated
output compiled against the pinned target crate in the pack's own CI
proof") is not attempted here since there is no Rust crate surface of
mfact's own to compile against (mfact is a Lean 4 project; this pack
generates a Rust *reference* catalog of its README, not bindings to any
mfact API). Reaching L5 ("Pack tracks the target's ontology, not its API")
requires mfact to publish such an ontology — an external, upstream change
this repo does not control, tracked as the honest blocker for this
dimension rather than worked around.

## 2026-07-19 update: a real (but unmerged) Rust reference was found

`~/mfact`'s own `main` branch and `docs/MFACT_CORE_DESIGN.md` are unchanged
by this update — the domain owner still has not decided a Rust contract for
`mfact-core`, and `crates/mfact-core/` on `main` still has no `Cargo.toml`
or `src/`. What changed: a search of `~/mfact`'s local git worktrees
(`git branch -a --contains <commit>` against every `#[test]`-containing
`.rs` file under `~/mfact`, excluding `target/`/`.lake/`) found a real,
independently-authored, substantive `mfact-core` Rust crate on TWO unmerged
agent-worktree branches (`worktree-wf_24b4eb65-119-19`,
`worktree-wf_24b4eb65-119-6`), most recently touched by commit
`154b62f956a` ("fix(mfact-core): add [build-dependencies] cc = \"1\" to
Cargo.toml (G10)"); `src/receipt.rs` itself was last touched by commit
`c7413cb352e` (2026-07-11).

**Honest status of this source:** real code, same author, same machine,
directly on-domain (a `GgenReceiptEngine` computing chained-BLAKE3 receipts
over canonical N-Quads `Fact`s — exactly the "every standing claim is
computed from a build" pipeline this pack's `mfa:Certify` individual already
described) — but genuinely unmerged, exploratory work, not `~/mfact`'s
shipped or canonical API. It is used here as a **fidelity target**, scored
as such, not represented as mfact's canonical surface.

- **Files copied verbatim** into `reference/mfact-core/src/` (SHA-256,
  verified equal before/after copy, both source and copy hashed with the
  same `shasum -a 256` invocation):

  | File | SHA-256 |
  |---|---|
  | `lib.rs` | `63be598eff28035b36269ff84361ab52cc226c22a8ec282f31e9efa683818c29` |
  | `receipt.rs` | `e52a3853ce3d5c503081efea666885bd2d870d3bf58a1da015479d9a553ab6c9` |
  | `validate.rs` | `5fd3031005bf51e6baf03ac577623f331ca667e4232b3422aba728fe413a0a08` |

- **Ontology extended**: `ontology.ttl`'s new `mfa:ReceiptEngine` class /
  `mfa:GgenReceiptEngine` individual (hash algorithm, ordering strategy,
  duplicate-refusal policy, source-file provenance — all real facts from
  `receipt.rs`, not invented). `shapes.ttl` gained `mfa:ReceiptEngineShape`
  with real cardinality/datatype constraints (mirrors the existing
  `PipelineStageShape`/`AuthorityDirShape` pattern; still passes
  `ggen graph validate`, `shapes_conform: true`, confirmed by running it).

- **Two new templates** render `src/mfact_refusal.rs` (the reference's
  `Refusal` enum + `hash_bytes`, transcribed verbatim) and
  `src/mfact_receipt.rs` (the reference's `Fact`/`Receipt`/
  `GgenReceiptEngine`, transcribed verbatim, with an ontology-SPARQL-driven
  doc header). The one deviation from the reference, disclosed in the
  generated file's own header comment: `use crate::{Refusal, hash_bytes};`
  becomes `pub use crate::mfact_refusal::{Refusal, hash_bytes};`, because
  this pack splits `Refusal`/`hash_bytes` into their own module rather than
  sharing a crate-root `lib.rs` with the reference's unported
  `Manifest`/`Artifact`/genesis-fold surface (out of scope this round, see
  below).

- **The headline artifact**: the reference's own `#[cfg(test)] mod tests`
  block from `receipt.rs` (76 lines, 4 real tests: N-Quads formatting with
  and without a named graph, empty-seed refusal, sort-order-independent
  determinism, exact-duplicate-fact refusal) is appended to the generated
  `src/mfact_receipt.rs` template **byte-identical** to the reference —
  confirmed by `diff` against the extracted reference test body both
  immediately after authoring the template and again after a real `ggen
  sync run` on a scratch consumer (`diff` reported no differences either
  time; see verification commands below).

### Verification commands run and their real output

```
$ ggen graph validate --files packs/mfact-pack/ontology.ttl --shapes packs/mfact-pack/shapes.ttl
{"files_checked": 1, "shapes_checked": 1, "files": [{"quads": 84, "shapes_conform": true, ...}]}

$ ggen sync run   # in a scratch consumer wiring mfact-pack as its only pack
{"written": ["src/mfact_catalog.rs", "src/mfact_catalog_ext.rs",
  "tests/mfact_catalog_proof.rs", "src/mfact_lib_wiring.rs",
  "src/mfact_receipt.rs", "docs/mfact_reference.md", "src/mfact_refusal.rs"],
  "skipped": []}

$ diff <extracted reference receipt.rs test body> <extracted generated mfact_receipt.rs test body>
(no output -- byte-identical)

$ cargo test   # in the scratch consumer, against the generated code
running 4 tests
test mfact_receipt::tests::test_fact_to_nquad ... ok
test mfact_receipt::tests::test_receipt_engine_duplicate_rejection ... ok
test mfact_receipt::tests::test_receipt_engine_empty_seed ... ok
test mfact_receipt::tests::test_receipt_engine_determinism ... ok
test result: ok. 4 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out

running 26 tests   (pre-existing mfact_catalog_proof.rs, unaffected)
test result: ok. 26 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out

$ ggen sync run   # again, then:
$ diff -rq <src/ before> <src/ after>
(no output -- idempotent regeneration confirmed)
```

### What this does and does not prove

**Proves**: a real, independently-authored (if unmerged) Rust test suite,
copied byte-for-byte, passes with zero modification against code generated
entirely from `ontology.ttl` + templates in this pack — the strongest
fidelity mechanism this repo has identified (per `TCPS-PACK-ARD-PRD.md`),
now demonstrated on a second pack. Regeneration is idempotent. SHACL shapes
gate the new class with real cardinality constraints, not vague ones.

**Does not prove**: that this is `~/mfact`'s canonical, shipped API — it is
not (see provenance note above; `main`'s `docs/MFACT_CORE_DESIGN.md` is
explicit that no Rust contract has been chosen). Does not cover
`validate.rs`'s `validate_manifest_concurrently` surface or `lib.rs`'s
`Manifest`/`Artifact`/`Evidence`/`compute_genesis_fold` surface — those
remain untranscribed (real, disclosed scope limit, not silently dropped).
Does not establish Generation-depth/Handler-gap/Ontology-expressiveness at
L5 for the whole pack: the pre-existing `mfact_catalog`
struct/function bodies (`validate_pipeline_order`, `validate_handoff_contract`)
are still hand-written Rust in the template, not derived from a generic
ontology-expression vocabulary the way `TCPS-PACK-ARD-PRD.md` describes for
`tcps-core-pack`.

## Honest maturity assessment (per `docs/packs/PACK_MATURITY_MODEL.md`)

- **Target-API fidelity**: raised from L2 ("verified by hand, once") to a
  genuine, checked **L3-equivalent** for the receipt-engine slice
  specifically — "generated output tested against the pinned target['s own
  real test file], in the pack's own proof" — via the byte-identical
  `receipt.rs` test-body reuse above. Not claimed for the pack as a whole
  (the pre-existing catalog/README slice is still hand-verified-once, L2),
  and the "target" here is an unmerged worktree branch, not mfact's
  canonical `main` — a real, named, load-bearing caveat on how far even L3
  reaches.
- **Test generation**: for the receipt-engine slice, meets the L5 wording
  ("generated proof suite is sufficient evidence on its own — passing it
  certifies the subsystem") in the narrow sense that the reference's own
  independent test suite (not a pack-authored proof file) passes against
  generated code. Still bounded by the fidelity caveat above.
- **Generation depth / Handler-gap / Ontology expressiveness**: unchanged
  from before this pass — still L1/L2. The new `GgenReceiptEngine` logic is
  hand-written Rust in `mfact_receipt.rs.tmpl`, not derived from a
  structural expression vocabulary in `ontology.ttl` the way
  `TCPS-PACK-ARD-PRD.md`'s plan describes; only the doc header and the
  class's descriptive facts are SPARQL-driven.
- **Overall**: this pack does not reach L5 by this pass. It moves one real
  dimension (Target-API fidelity, for one module) from L2 to a bounded L3,
  backed by the verification commands above rather than assertion. Reaching
  L5 would additionally require: (a) mfact publishing a canonical Rust
  contract (or this pack tracking one merged into `main`, not a worktree
  branch), and (b) deriving `GgenReceiptEngine`'s logic from an ontology
  expression vocabulary rather than literal template Rust.
