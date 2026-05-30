# CONSOLIDATE-002 — Pre-Inventory: merge the three order-sensitive publish branches

**Role:** Merge Architect (read-only; only this file written).
**Target:** `crates/ggen-lsp/src/state.rs` — `ServerState::analyze_and_observe`.
**Decision:** **READY** (natural species-driven loop reproduces the exact emit order).

---

## 1. Ground truth (verified against the code, not docs)

### 1.1 The three publish branches (the last triplication)

`analyze_and_observe` (state.rs lines 465–655) contains six fixed-order
group/clear blocks over three species:

| # | Block | Lines | Species | Anchor URI | Self-merge guard |
|---|-------|-------|---------|-----------|------------------|
| A1 | publish group | 530–546 | TPL (`GGEN_TPL_001`) | `.tera` (`template_path`) | `edited == anchor` |
| A2 | publish group | 551–568 | HARNESS (`GGEN_HARNESS_001`) | `Cargo.toml` (`manifest_path`) | `edited == anchor` |
| A3 | publish group | 578–594 | OUT (`GGEN_OUT_001`) | `ggen.toml` (`manifest_path`) | `edited == anchor && !published_self` |
| B  | self fallback | 598–601 | (edited's own single-file) | `uri` | `!published_self` |
| C1 | stale clear | 608–614 | TPL | cleared `.tera` URIs | (residual) |
| C2 | stale clear | 622–635 | HARNESS | cleared `Cargo.toml` URIs | (residual) |
| C3 | stale clear | 643–652 | OUT | cleared `ggen.toml` URIs | (residual) |

Each `observe_diagnostics` call appends an OCEL event batch to
`.ggen/ocel/agent-edit-events.ocel.jsonl` **in call order** (verified:
`observe_diagnostics` → `IntelLog::at_root(&self.root).append(&events)`,
state.rs:286; `IntelLog::append` is append-only NDJSON, log.rs:53–76). Reordering
any `observe_diagnostics` call reorders the on-disk OCEL line sequence ⇒ FAKE-LIVE.

### 1.2 Trigger gates are MUTUALLY EXCLUSIVE by edited-file basename

- `tpl_is_trigger` = `.tera | .sparql`/`.rq` OR `ends_with("ggen.toml")` (state.rs:498–499).
- `harness_is_trigger` = `ends_with("Cargo.toml") | ends_with("Makefile.toml")` (state.rs:509, `is_harness_surface` 101–103).
- `out_groups` rides the **same** `tpl_is_trigger` gate (state.rs:521).

No single edited file is both a TPL trigger and a HARNESS trigger (basename
`ggen.toml` ≠ `Cargo.toml`/`Makefile.toml`). Therefore:

> **HARNESS can never co-fire with TPL/OUT in one `analyze_and_observe` pass.**
> The only multi-species pass possible is **TPL + OUT** (a `.tera`/`.rq`/`ggen.toml` edit).

This is the load-bearing fact for the sequence-equivalence proof: the multi-species
golden need only exercise **TPL + OUT**.

### 1.3 The `own_diags` / `published_self` GLOBAL merge-once invariant

There is ONE `own_diags: Vec<Diagnostic>` (state.rs:485) and ONE
`published_self: bool` (528). A self-merge does `std::mem::take(&mut own_diags)`
(empties it) and sets `published_self = true`. Because the three anchors are
disjoint **per edited file**, at most one branch self-matches per pass:

- `.tera` edit → only TPL's `edited == template_url` can be true.
- `ggen.toml` edit → only OUT's `edited == manifest_url` can be true (TPL anchors
  `.tera`, never `ggen.toml`; HARNESS gate is off).
- `Cargo.toml` edit → only HARNESS can self-match (TPL/OUT gates off).
- `.rq` edit → no anchor equals a `.rq` URI, so no branch self-merges;
  `own_diags` falls through to block B (598–601).

The OUT branch's extra `&& !published_self` guard (584) is therefore a *defensive
generalization*, not a behavioral necessity in any reachable single-pass state.
**A unified loop MUST keep an equivalent global `published_self` short-circuit so
the merge-once-per-pass invariant holds even if a future species shares an anchor.**

### 1.4 Normalizable (nondeterministic) serialized fields

Per `intel/events.rs` builders + `OcelEvent` serialization (log.rs:66
`serde_json::to_string(ev)`), each line carries:

| Field | Deterministic? | Normalize how |
|-------|----------------|---------------|
| `id` (event_id = blake3(activity‖file‖run_id‖seq)) | NO (run_id, seq) | drop / replace with `"<id>"` |
| `activity` | **YES** | keep (sequence key) |
| `timestamp` (`Utc::now()`) | NO | replace with `"<ts>"` |
| `objects[].id` for type `episode` (blake3(file‖code‖run_id)) | NO (run_id) | replace with `"<episode>"` |
| `objects[].id` for type `receipt` (blake3(file‖code‖run_id)[..16]) | NO (run_id) | replace with `"<receipt>"` |
| `objects[].id` for type `file` | **YES** (path) | keep, but make root-relative |
| `objects[].id` for type `diagnostic_code` | **YES** (`GGEN-TPL-001` etc.) | keep |
| `objects[].id` for type `repair_route` | **YES** (seeded route id) | keep |
| `attributes.receipt_id` | NO (run_id) | replace with `"<receipt>"` |
| `attributes.session_id` (= run_id) | NO | replace with `"<run>"` |
| `attributes.agent_id` / `transport` | **YES** (`editor`/`lsp`) | keep |
| `attributes.span` / `severity` / `route` / `route_source` / `error_count` | **YES** | keep |
| absolute `file` path (TempDir root) | NO (temp path) | make root-relative |

The **proof key** is the ordered list of `(activity, root_relative_file, code)`
tuples — robust to every nondeterministic field while fully sensitive to a
reorder. (Pass counts alone CANNOT detect a reorder; the tuple SEQUENCE can.)

---

## 2. Order map (the exact emit sequence the merge must reproduce)

A pass emits `observe_diagnostics` calls in this total order. Within one species
group/clear block, the per-anchor order is the iteration order of the detector's
returned `Vec` (`detect_*` push order — preserved as-is by any unified loop that
iterates the same `Vec`).

```
PHASE A (publish groups), species order = [TPL, HARNESS, OUT]:
  A.TPL    : for g in tpl_groups     -> observe(anchor=.tera)      [self-merge if edited==.tera]
  A.HARNESS: for g in harness_groups -> observe(anchor=Cargo.toml) [self-merge if edited==Cargo.toml]
  A.OUT    : for g in out_groups     -> observe(anchor=ggen.toml)  [self-merge if edited==ggen.toml && !published_self]

PHASE B (self fallback):
  if !published_self -> observe(uri, own_diags)

PHASE C (stale clears), species order = [TPL, HARNESS, OUT]:
  C.TPL    : if tpl_is_trigger     -> for c in tpl_clears_for(...)            -> observe(residual)
  C.HARNESS: if harness_is_trigger -> for c in clears_for(HARNESS,...)        -> observe(residual)
  C.OUT    : if tpl_is_trigger     -> for c in clears_for(OUT,...)            -> observe(residual)
```

**Hard constraints any unified loop must satisfy (else FAKE-LIVE):**

1. Species iteration order is **[TPL, HARNESS, OUT]** in BOTH Phase A and Phase C.
2. Phase B (self fallback) sits **strictly between** Phase A and Phase C.
3. `published_self` is **global across all three Phase-A species** and gates Phase B.
4. `own_diags` is taken **at most once** per pass (the first self-matching anchor).
5. Each species' Phase-C clear runs **iff its own trigger gate is set**
   (`tpl_is_trigger` for TPL and OUT; `harness_is_trigger` for HARNESS) — the
   gate, not "did the group fire", controls the clear (matches current code:
   C.TPL/C.OUT both guarded by `tpl_is_trigger`, C.HARNESS by `harness_is_trigger`).
6. Within a block, anchor order = detector `Vec` order (do not sort).

---

## 3. Is a natural unified loop achievable? → YES (READY)

The three Phase-A branches are **structurally identical modulo a species
descriptor**:

```
(code, groups: Vec<(PathBuf, Vec<Diagnostic>)>, flagged_out: &mut HashSet<Url>, self_guard)
```

The only real differences are:
- the diagnostic `code` key (TPL / HARNESS / OUT) — already a `&'static str`
  threaded through `clears_for`;
- the `groups` Vec (already computed up front: `tpl_groups`, `harness_groups`,
  `out_groups`);
- which `current_*_flagged` set to populate (TPL→`current_flagged`,
  HARNESS→`current_harness_flagged`, OUT→`current_out_flagged`);
- the self-merge guard: TPL/HARNESS = `edited==anchor`; OUT = `edited==anchor && !published_self`.

The guard difference collapses naturally: **`edited==anchor && !published_self`
is the correct guard for ALL THREE** (§1.3 proves the extra `!published_self` is
behavior-preserving for TPL and HARNESS because no second self-match is reachable
in one pass — adding the guard cannot change any observable emit, since by the
time TPL/HARNESS would self-match `published_self` is still `false`). So a single
loop using the OUT guard for every species reproduces every reachable sequence
**byte-identically**.

Likewise Phase C is three identical `clears_for(code, ...)` → residual → observe
blocks differing only by `code` and trigger gate — already proven mergeable
(CONSOLIDATE-001 merged the close-path clears the same way).

Because species order is **fixed data** (`[TPL, HARNESS, OUT]`) and the loop body
iterates each species' precomputed `Vec` in order, the unified loop's call
sequence is **identical** to the current concatenation of the three branches.
**No unnatural construction is required.** → **READY.**

A 4th species then becomes one registry tuple
`(code, detect_fn_result, &mut flagged_set)` + a detector — not a 4th publish
branch and not a 4th clear branch.

---

## 4. Verbatim diff plan (for the implementer — NOT applied here)

> Strictly behavior-preserving. SINGLE-WRITER. No `#[allow]`. Nothing outside
> `crates/ggen-lsp/`. Do NOT touch `intel/mine.rs`. Do NOT edit existing test
> assertions.

### 4.1 Introduce a local species descriptor (function-local, no public API change)

Inside `analyze_and_observe`, after the three `*_groups` are computed (state.rs:525)
and after `edited_self_url`/`published_self` are declared (527–528), build an
ordered species list. Each entry owns its `code`, its `groups`, and a place to
record `current_flagged`:

```rust
// Ordered species descriptors — the FIXED [TPL, HARNESS, OUT] emit order.
// `groups` is the precomputed detector output; `flagged` accumulates the
// anchors flagged THIS pass (handed to the matching Phase-C clear).
struct Species {
    code: &'static str,
    groups: Vec<(PathBuf, Vec<Diagnostic>)>,
    flagged: HashSet<Url>,
}
let mut species = [
    Species { code: crate::analyzers::GGEN_TPL_001,     groups: tpl_groups,     flagged: HashSet::new() },
    Species { code: crate::analyzers::GGEN_HARNESS_001, groups: harness_groups, flagged: HashSet::new() },
    Species { code: crate::analyzers::GGEN_OUT_001,     groups: out_groups,     flagged: HashSet::new() },
];
```

### 4.2 Replace the three Phase-A branches (state.rs:529–594) with ONE loop

```rust
for sp in &mut species {
    let groups = std::mem::take(&mut sp.groups); // move out to iterate by value
    for (anchor_path, diags) in groups {
        let Some(anchor_url) = url_from_path(&anchor_path) else { continue; };
        sp.flagged.insert(anchor_url.clone());
        if edited_self_url.as_ref() == Some(&anchor_url) && !published_self {
            // edited file IS this anchor: merge own single-file diags once.
            let mut merged = std::mem::take(&mut own_diags);
            merged.extend(diags);
            self.observe_diagnostics(&anchor_url, &merged).await;
            published.push((anchor_url, merged));
            published_self = true;
        } else {
            self.observe_diagnostics(&anchor_url, &diags).await;
            published.push((anchor_url, diags));
        }
    }
}
```

Note: the unified guard is `edited==anchor && !published_self` for ALL species
(§3 proves this is behavior-preserving). The former `current_flagged` /
`current_harness_flagged` / `current_out_flagged` locals are now `species[i].flagged`.

### 4.3 Phase B (self fallback) — UNCHANGED (state.rs:598–601)

Stays exactly as-is between the loops.

### 4.4 Replace the three Phase-C branches (state.rs:608–652) with ONE loop

```rust
for sp in &species {
    // Each species' clear runs iff ITS trigger gate is set (preserves current
    // guards: TPL/OUT under tpl_is_trigger, HARNESS under harness_is_trigger).
    let gated = match sp.code {
        c if c == crate::analyzers::GGEN_HARNESS_001 => harness_is_trigger,
        _ => tpl_is_trigger, // TPL and OUT
    };
    if !gated { continue; }
    for cleared in self.clears_for(sp.code, uri, &sp.flagged).await {
        let residual = self.residual_single_file_diags(&cleared).await;
        self.observe_diagnostics(&cleared, &residual).await;
        published.push((cleared, residual));
    }
}
```

`tpl_clears_for` (the public shim) is NOT removed — the stale-clear test drives it
directly; the unified loop uses the generic `clears_for(code, ...)` it already
delegates to (state.rs:165–168), so behavior is identical.

### 4.5 Anti-FAKE-LIVE checklist for the implementer

- [ ] Species order literal is exactly `[TPL, HARNESS, OUT]` in BOTH loops.
- [ ] Phase B unchanged and between the loops.
- [ ] Guard is `edited==anchor && !published_self` for all species.
- [ ] `own_diags` taken at most once (guaranteed by `published_self` short-circuit).
- [ ] `clears_for` arguments unchanged per species (`uri`, `sp.flagged`).
- [ ] No detector `Vec` is sorted/reordered.
- [ ] `cargo make test -p ggen-lsp` all green UNCHANGED; `clippy -p ggen-lsp --no-deps -- -D warnings` = 0; `cargo fmt` clean.

---

## 5. The sequence-equivalence proof (golden fixture + new test)

### 5.1 Multi-species scenario (TPL + OUT in ONE pass)

The OUT-001 invalid fixture deliberately keeps the template body lawful
(`templates/item.tera` = `Item: {{ name }}`), so it raises OUT only. For the
multi-species golden we need a fixture where, on a SINGLE edit, **both** TPL (on
the `.tera` body) and OUT (on the `ggen.toml` output_file) fire.

**New fixture** (`crates/ggen-lsp/tests/fixtures/consolidate_002_multispecies/`):
a project where:
- `queries/items.rq` projects **only `?name`** (`SELECT ?name WHERE { ... }`);
- `templates/item.tera` body consumes `{{ row["title"] }}` (unbound) → **TPL-001 on `.tera`**;
- `ggen.toml` rule `output_file = "out/{{ slug }}.txt"` (`slug` unbound) → **OUT-001 on `ggen.toml`**.

**Edit driving the pass:** analyze the **`ggen.toml`** URI (a TPL+OUT trigger).
This pass fires:
- A.TPL: TPL group anchored on `.tera` (edited≠`.tera` ⇒ no self-merge; observe tpl_diags).
- A.HARNESS: empty (no Cargo.toml / gate off).
- A.OUT: OUT group anchored on `ggen.toml` (edited==`ggen.toml`, `!published_self`
  ⇒ self-merge own TomlAnalyzer diags + out_diags, publish once).
- B: skipped (`published_self == true`).
- C.TPL / C.OUT: no clears on first pass (flagged sets fresh), C.HARNESS gate off.

This single pass emits the TPL episode chain (DiagnosticRaised + RouteSelected +
RepairSuggested on `.tera`) **before** the OUT episode chain (same on `ggen.toml`)
— a genuine multi-species ordered sequence. (Adding a second pass that repairs the
`.rq` to bind both vars would additionally exercise C.TPL + C.OUT clears in order;
the golden may capture pass 1 only for a tight, decisive ordering, or both passes
for fuller coverage — implementer's choice, but the golden must be captured from
pass(es) that fire ≥2 species.)

### 5.2 Golden capture (committed fixture)

Add a `#[ignore]`-gated generator mirroring `oracle_tape_generator.rs`:
`tests/multispecies_sequence_generator.rs`. It:

1. writes the fixture into a `TempDir`, builds `ServerState::with_root`;
2. drives `analyze_and_observe(&ggen_toml_uri, src)` (the multi-species pass);
3. reads the EXTERNAL log via the existing `read_log_lines` helper pattern;
4. **normalizes** each line to the tuple `(activity, root_relative_file, code)`
   per §1.4 (extract `activity`; the `file`-type object id made relative to
   `root`; the `diagnostic_code`-type object id) — dropping `id`, `timestamp`,
   `episode`/`receipt` ids, `session_id`, `receipt_id`;
5. writes the ordered tuple list to
   `tests/fixtures/consolidate_002_multispecies/golden-sequence.jsonl`
   (one normalized tuple per line, in emit order).

Regeneration command (documented in the generator header):
```bash
cargo test -p ggen-lsp --test multispecies_sequence_generator -- --ignored --nocapture
```

**Capture the golden from CURRENT (pre-merge) code and COMMIT it before the merge.**

### 5.3 New assertion test (runs in CI, pre- and post-merge)

`tests/consolidate_002_sequence_equivalence.rs`:

1. same fixture + `analyze_and_observe(&ggen_toml_uri, src)` on a fresh `TempDir`;
2. read + normalize the emitted sequence with the SAME normalizer as the generator;
3. load the committed `golden-sequence.jsonl`;
4. `assert_eq!(emitted_normalized_seq, golden_seq)` — ordered, element-by-element.

This test PASSES on pre-merge code (it equals the golden it was captured from) and
must PASS UNCHANGED on post-merge code. Any reorder fails it. It is NOT one of the
existing tests, so it adds proof without editing existing assertions.

### 5.4 Normalizer (shared, test-only helper)

```text
line -> serde_json::Value
activity   = v["activity"]
file       = first v["objects"][i] where type=="file", id made relative to root
code       = first v["objects"][i] where type=="diagnostic_code"
tuple      = (activity, file_rel, code)
```
No timestamps, no blake3 ids, no run_id/session_id in the tuple.

---

## 6. ALIVE / FAKE-LIVE / BLOCKED definitions for CONSOLIDATE-002

**ALIVE (success):**
- The three Phase-A publish branches AND the three Phase-C clear branches are each
  one species-driven loop over the ordered `[TPL, HARNESS, OUT]` descriptor list.
- The multi-species normalized `(activity, file, code)` sequence emitted on the
  `ggen.toml` edit is **IDENTICAL** to the committed pre-merge golden.
- ALL existing tests pass UNCHANGED: the 3 species living-loops
  (`ggen_tpl_001_living_loop`, `ggen_harness_001_living_loop`,
  `ggen_out_001_living_loop`), `ggen_tpl_001_stale_clear`,
  `ggen_tpl_001_did_close_clear`, `ggen_tpl_001_regression`, `ggen_live_buffer_001`.
- `clippy -p ggen-lsp --no-deps -- -D warnings` = 0; `cargo fmt` clean.

**FAKE-LIVE (reject):**
- The normalized sequence changed (any reorder, insertion, or drop).
- An existing test assertion was edited to make it pass.
- The `own_diags` merge-once semantics changed — e.g. a template that IS its own
  edited file now double-publishes (own_diags emitted twice), or `published_self`
  no longer gates Phase B.
- `tpl_clears_for` public shim removed (the stale-clear test drives it directly).

**BLOCKED (deliver partial + this inventory, no forced merge):**
- The exact `[TPL, HARNESS, OUT]` + Phase-B-between-clears order cannot be held in
  a unified loop without an unnatural construction (e.g. needing per-species
  special-casing that defeats the loop). **NOT the case here (§3): READY.**

---

## 7. Return

**READY.** Natural species-driven loop (§4) reproduces the exact emit order; the
unified `edited==anchor && !published_self` guard is behavior-preserving (§1.3,
§3). Proof = committed pre-merge normalized `(activity, file, code)` golden for a
TPL+OUT `ggen.toml`-edit pass (§5), asserted equal post-merge by a NEW test that
edits no existing assertion.
