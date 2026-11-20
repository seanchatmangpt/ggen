# v26.11.3 RESOLUTIONS (ABB/SBB hardening decisions)

Canonical checkout: /Users/sac/ggen. Coordinator owns git + justfile.

## R1 — Composer + kernel are complementary gates

Lawful consumer path = `ggen pack compose` → `ggen sync run` Stage 2c, never
either alone. Composer output alone is not admitted material; kernel admission
alone (skipping composition) is a legacy bypass. Any consumer (arcus included)
that runs one without the other is refused at the seam, not warned.

## R2 — Kernel Refusal survives the boundary as typed codes

Kernel `Refusal` must cross into ggen-engine / arcus as typed codes, not
stringified FM-LAW-015. The abb-sbb `Refusal` enum is the canonical typed home;
downstream mirrors register in the refusal registry
(`ontologies/shapes/boundary-refusal.shacl.ttl`). Serialization of a refusal
into prose is a contract-drift defect, same class as receipt describing
something other than what ran.

## R3 — Missing ea.graph.json fails loud

Missing ea.graph.json at Stage 2c must fail loud or be an explicit opt-out
flag; silent skip is banned. A warning-and-continue is a fail-open defect
(coding-agent-mistakes class 3). The opt-out flag, if added, is named in the
receipt and registers as a registered divergence, never a default-on bypass.

## R4 — EA graph authoring = Turtle+SHACL + one projection rule

EA graphs are authored as Turtle+SHACL in a pack; a single projection rule
renders ea.graph.json; serde structs become the projection target, not a
hand-maintained parallel schema. Hand-editing projected ea.graph.json is
banned (generated-output law); edit the TTL and re-project.

## R5 — Qualification is digest-bound only

Qualification verdicts bind to content digests of the admitted subject.
Marketplace ALIVE is evidence in the qualification receipt, never admission.
A qualification that cites "the pack is ALIVE in the marketplace" without a
digest of the exact subject is UNVERIFIED, regardless of external standing.

## Open decisions (unresolved)

(none yet — append as they arise, with date and decider)

## R11 — WO-06 R10 SUPERSEDED by the G3 ceiling-law fix

WO-06 R10 ("exact-equality cap binding as law, never loosen to `<=`") is
SUPERSEDED by G3's landed correction of the ceiling semantics in
`crates/ggen-abb-sbb/src/envelope_court.rs` (HIGH-1 fix, v26.11.3): the
lawful relation is `measured <= declared`. CapTable is documented
"cap id -> maximum value" — a ceiling is a maximum, not an equality; the
previous `declared == measured` inverted ceiling semantics and passed only
when a receipt happened to fill the ceiling exactly. Corrected law: a
receipt measuring at or under the declared ceiling PASSES; a receipt
measuring above the ceiling refuses (`CAP_CEILING_MISMATCH`, exit 2). R7
(`ENVELOPE_GRAPH_MISMATCH`, identity-before-policy digest check) was already
landed by G3 — verified 2026-11-03 at `envelope_court.rs:53-61`; R8
freshness delegation to `verify_receipt` (`ggen-abb-sbb/src/lib.rs:1056`)
is now registered in the court's module doc comment. Receipt:
`docs/jira/v26.11.3/receipts/i5-wo06-reconcile.md`.

## R12 — WO-06 R10 superseded (cross-reference to R11)

WO-06's R10 ("exact-equality cap binding as law") is superseded by the G3
ceiling-law correction — decision text and receipt live in R11 above. This
row exists so WO-06's decision surface resolves to the current law without
reading R11's history. Landed receipt:
`docs/jira/v26.11.3/receipts/i5-wo06-reconcile.md` (verified on disk
2026-11-03).

## R13 — `--skip-ea-admission` is an opt-out flag, never a default-on bypass (R3 instance)

The EA-graph Stage 2c opt-out is the named CLI flag `--skip-ea-admission`,
landed on the `ggen sync` verb surface and threaded to
`SyncOptions.skip_ea_admission` (R3's "explicit opt-out flag" clause). It is
a registered divergence, not a silent skip: three real-binary courts plus a
SHIM-COURT flip witnessed the flag's effect. Receipt:
`docs/jira/v26.11.3/receipts/k8-cli-flag-depgraph.md` (landed, verified on
disk 2026-11-03).

## R14 — Declarative manifests admit through the same hoisted Stage 2c admission fn, at the kernel default ceiling (R1/R3 instance)

F2 hoisted the frontmatter Stage 2c EA admission sequence verbatim into
`pub(crate) fn run_ea_graph_admission(root, skip_ea_admission,
envelope_iri, prior_generation)` in
`crates/ggen-engine/src/sync.rs`; both the frontmatter path and the
declarative-rules dispatch call the one function — no parallel admission
implementation exists (R1's "never either alone" law holds on the
declarative path too). Declarative manifests declare no template
frontmatter envelopes, so the declarative dispatch passes `None` and the
kernel applies DEFAULT_MAX_ARTIFACTS (64) — a default ceiling, not a
bypass. The missing-graph fail-closed gate and the `--skip-ea-admission`
registered divergence (R13) apply identically on both paths. Owned court
`declarative_ea_admission_test` 4/4 green; G1's frontmatter wiring court
7/7 (hoist did not disturb the frontmatter path); suite-wide fixture
fallout (~100 declarative fixtures loudly refusing FM-LAW-015) recorded in
the receipt as follow-up work, not rounded up. Receipt:
`docs/jira/v26.11.3/receipts/f2-declarative-admission.md` (landed,
verified on disk 2026-11-03).

## R15 — Cycle law split: structural graph vs capability ordering edges (WO-03 §4)

WO-03 §4 ("Reconcile cycle laws") requires a RESOLUTIONS row naming which
law governs each edge kind when composer admission and kernel Stage 2c
depgraph admission overlap on the same pack set
(`docs/jira/v26.11.3/WO-03-composer-stage2c.md`). Ruling:

- **kernel-refuses-all** governs `[graph] depends_on` and consumer `depends_on`
  edges in `ggen-abb-sbb` `resolve_sync_order` / `depgraph.rs`: any hard
  structural cycle is a `CYCLIC_PACK_DEPENDENCY` refusal at Stage 2c — no
  silent drop.
- **composer-drops** governs `capabilities.requires` URN-derived ordering
  edges in `packs_registry::composer` Kahn (`order_with_capability_edges`):
  cycle-closing ordering edges are dropped deterministically so topological
  sort terminates; cycle **refusal** remains dependencies-only (see
  `crates/ggen-marketplace/src/packs_registry/composer.rs` step 5 comment,
  lines 274–279: "these edges NEVER refuse; an edge that would close a cycle
  is dropped … cycle refusal remains dependencies-only").

Falsifier: a structural `[graph]` or consumer-edge cycle that the composer
admits but the kernel refuses (or the reverse), or a capability-ordering
cycle treated as `CYCLIC_PACK_DEPENDENCY` without this row, is a contract
defect. WO-03 contradiction-test clause (§ Acceptance #3) binds. Coordinator
merged at integration; verified on disk 2026-11-20.
