# WO-07: Depgraph Robustness — Port Normalization, Honest Scope, and Closure Bound

## Date

2026-11-03

## Repo

/Users/sac/ggen (canonical checkout; no worktrees)

## Lane

WRITER / implement lane — `crates/ggen-abb-sbb/src/depgraph.rs` and its tests only. Kernel surface
outside this file is fenced (Refusal enum additions coordinated per RESOLUTIONS R2; new variants are
additive, so no downstream mirror breaks).

## Subject

`crates/ggen-abb-sbb/src/depgraph.rs` (484 lines) — the cross-pack Datalog-style resolver. Five
defects, each verified against the file on 2026-11-03:

1. **Ports are bare strings, exact-match only.** `PackManifest.requires`/`provides` and the
   `requires`/`provides` comparison in Gate 4 (`depgraph.rs:202-220`) compare `BTreeSet<String>`
   byte-exactly. No trimming, no case folding. `"Hashing"` vs `"hashing "` vs `"hashing"` bind as
   three different ports; a case/whitespace variant silently fails Gate 4 (or silently passes a
   provider that a differently-cased consumer can never see).
2. **Doc-comment overclaims artifact-collision coverage.** `depgraph.rs:3` claims the resolver is
   "refused on cycles, unbound ports, and artifact collisions". The kernel's collision check is
   intra-pack only (`depgraph.rs:154`, inside the per-pack artifact loop); cross-pack
   `DuplicateArtifactPath` across two packs is owned by the composer, not this module. The comment
   describes coverage the module does not implement — contract-drift class.
3. **ConsumerEdges has no `requires` field.** `depgraph.rs:29-35` defines `ConsumerEdges { name,
   depends_on }`. A consumer `ggen.toml [graph] requires = [...]` is never extracted and never
   port-checked in Gate 4; a consumer demanding a port no pack provides passes silently.
4. **Naive fixpoint closure has no iteration bound.** The transitive-closure loop is O(V²E) per
   pass × passes, with no ceiling. At pack scale this is fine; on a malformed/pathological graph
   it loops without a typed exit.
5. **Kahn-phase re-refusal is documented unreachable but comment-less about why.** `depgraph.rs:233-241`
   returns `Refusal::CyclicPackDependency` from the Kahn phase after the cycle gate (Gate 3) already
   refused cycles. The inline comment says "unreachable given the cycle gate above" but does not
   cite the invariant, and house law wants loud-kept-or-removed decided explicitly.

## Task

1. **Port normalization law — pick one, pin it, register it.** Per RESOLUTIONS R2/R3 spirit
   (typed, fail-closed), this WO selects: **reject non-canonical ports at extract time with a typed
   refusal**. Canonical form = trim + lowercase at *comparison/admission* is rejected because it
   makes two spellings silently equal (fail-open by normalization); a pack author with `" Hashing"`
   gets a loud refusal naming the offending port and pack, not a silent mismatch at Gate 4.

   - Add `Refusal::NonCanonicalPort { pack: String, port: String, field: String }` (or the closest
     additive fit to the existing enum) — typed per R2.
   - Enforce in `extract_pack_manifest` and `extract_consumer_edges` (once consumer `requires`
     exist, item 3): a port that differs from `trim().to_lowercase()` of itself refuses.
   - If the implementer instead lands normalize-and-match (trim+lowercase everywhere), that is a
     lawful alternative ONLY with the resolution appended to RESOLUTIONS.md as R6 with date and the
     court results — the decision is the deliverable either way, not just the code.
2. **Fix the doc-comment claim (item 2).** Reword `depgraph.rs:3` (and any duplicated claim on
   `plan()`/the gate) to state exact scope: "refused on cycles, unbound ports, and intra-pack
   artifact collisions; cross-pack duplicate artifact paths are the composer's responsibility."
   Do NOT implement a cross-pack collision check in this WO — composer ownership is the standing
   decision; the fix is honesty, not scope growth. If the implementer disagrees and implements the
   cross-pack check, that also requires the RESOLUTIONS R6 append.
3. **Add consumer `requires` to Gate 4 — or register the scope limit.** Preferred: add
   `requires: BTreeSet<String>` to `ConsumerEdges` (serde default, empty = no requirement —
   legacy `ggen.toml` without `[graph] requires` stays dependency-free per the constraint), extract
   it in `extract_consumer_edges`, and check it in Gate 4 against the full closure (consumer
   requires resolve against *all* packs, not just its direct `depends_on`, since the consumer
   consumes every pack by construction). The unbound-port refusal for a consumer carries
   `pack: <consumer name>` so the error names the consumer. Alternative (lawful fallback):
   document the scope limit on `ConsumerEdges` and in this WO's Standing section as a registered
   divergence. Pick one; do not leave it implicit.
4. **Add the closure bound.** Cap the fixpoint at a named constant (e.g.
   `MAX_CLOSURE_ITERATIONS: usize = packs.len().saturating_mul(2).max(SMALL_FLOOR)` or a flat
   ceiling — implementer's choice, document which). Exceeding the bound refuses with a typed
   variant (e.g. `Refusal::ClosureBoundExceeded { iterations: usize }`) rather than looping or
   panicking. Justify the bound: a converging fixpoint over V packs needs at most V passes; V×2
   with a floor is generous headroom, so hitting it means a graph the rules cannot converge on.
5. **Kahn-phase re-refusal: keep loud, cite the invariant.** House law prefers keeping the loud
   branch. Add a comment citing the exact invariant: Gate 3's cycle refusal fires on
   `transitive_dep(P, P)` for any P; a non-empty `remaining` with no ready node implies a cycle
   among those nodes, which Gate 3 must have refused — so this branch is reachable only if Gate 3
   and the Kahn phase disagree on cycle detection, i.e. a resolver invariant violation, not a pack
   authoring error. Keep the refusal; do not remove.

## Acceptance

Falsifiable, command-cited. All from `/Users/sac/ggen`:

1. **Normalization court.** A Chicago test builds real TOML with a non-canonical port (case variant
   and whitespace variant, both directions: provider side and requirer side) and asserts
   `extract_pack_manifest` returns the typed `NonCanonicalPort` refusal carrying the offending port
   and pack name; a fully-canonical manifest extracts clean. Both chosen-law directions pinned.
2. **Bound court.** A synthetic graph engineered to not converge under the implementation's rules
   (or, minimum, a graph with V packs and a forced iteration count > bound) triggers
   `Refusal::ClosureBoundExceeded` within the bound — no hang, no panic. Court proves the guard
   fires; a comment explains how the synthetic graph defeats convergence.
3. **Consumer requires court (if item 3 landed as the check).** Real `ggen.toml` with
   `[graph] requires = ["nope"]` and packs that never provide it → Gate 4 refuses with the consumer's
   name in the refusal; the same `ggen.toml` with `requires = []` (or absent `[graph]`) proceeds —
   legacy packs stay dependency-free.
4. **Scope-honesty grep gate.** `grep -n "artifact collisions" crates/ggen-abb-sbb/src/depgraph.rs`
   shows only scope-qualified phrasing ("intra-pack artifact collisions" or a pointer to composer
   ownership); the unqualified claim at line 3 is gone.
5. **Existing property tests stay green.** Closure==BFS, cycle-path reconstruction, and unbound-port
   naming tests pass unchanged:
   `cargo test -p ggen-abb-sbb depgraph` exits 0 with the prior test names present
   (`grep -n "fn .*closure\|fn .*cycle\|fn .*unbound" crates/ggen-abb-sbb/src/depgraph.rs` shows
   them untouched or additively renamed).
6. **Full crate suite green.** `cargo test -p ggen-abb-sbb` and `cargo build -p ggen-abb-sbb`
   exit 0.
7. **Mutation non-vacuity.** Reverting any one fix (remove the canonical-port check; remove the
   bound guard; restore the overclaiming comment; drop consumer `requires` from Gate 4) makes a
   named acceptance test fail; restoring makes it pass. Record per-defect mutation → test → exits.

## Constraints

- **No graph-format breakage**: a pack `pack.toml` or consumer `ggen.toml` with no `[graph]` table
  stays dependency-free, exactly as today. All new fields are `#[serde(default)]`.
- **Additive Refusal variants only** — downstream typed-code mirrors (R2) must not break; no
  variant renamed or removed.
- **Chicago tests**: real TOML documents, real `extract_*`/gate calls, assert on returned
  `SyncPlan`/`Refusal` state; no mocks, no stubs, no sleeps.
- The doc-comment fix is honesty, not scope growth — no cross-pack collision implementation
  without a RESOLUTIONS append.
- One canonical checkout; no worktrees; no `git reset --hard`; commits `-F <file>`.

## Standing

ALIVE (2026-11-04 reconciliation: cited receipts g6-depgraph.md §Standing reads "ALIVE for the
landed surface … PARTIAL on refusal variants" and k1-refusal-flip.md §Standing reads "ALIVE for
the flipped surface", explicitly resolving G6's PARTIAL-on-refusal-variants; RECEIPTS.md WO-07
row reads ALIVE with `-p ggen-abb-sbb` 129+3dt, hardening 15/15).

## Falsifiers

- `grep -n "artifact collisions" crates/ggen-abb-sbb/src/depgraph.rs` still shows the unqualified
  claim (scope-honesty gate fails).
- A case- or whitespace-variant port extracts without a typed refusal (normalization court vacuous).
- A non-converging synthetic graph loops past the bound instead of refusing
  (`ClosureBoundExceeded` never observed).
- A consumer `ggen.toml` with unsatisfiable `[graph] requires` passes Gate 4 silently (check not
  landed) — or a legacy `ggen.toml` without `[graph]` now refuses (format breakage).
- Any acceptance test constructs a `Mock`/stub in place of a real TOML collaborator.

## Fences

- `depgraph.rs` + its tests only; no edits to `lib.rs` refusal plumbing beyond the additive
  enum variants this WO names.
- Do not change `SyncPlan`'s shape or the gate's signature in a breaking way.
- Do not touch composer code (`DuplicateArtifactPath` across packs stays the composer's lane) —
  fence, not fix.
- No sleeps, no timing assertions, no `#[ignore]`, no `#[allow]` bypasses.
