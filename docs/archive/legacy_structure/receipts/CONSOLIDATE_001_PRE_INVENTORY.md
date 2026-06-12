# CONSOLIDATE-001 — Pre-Inventory & Lawful-Extent Decision

**Checkpoint kind:** REFACTOR (deepen authority + reduce drift). NOT a new diagnostic.
**Author role:** Consolidation Architect (read-only; this doc is the only write).
**Date:** 2026-05-30
**Repo:** ggen @ /Users/sac/ggen, branch `main`.

---

## 1. The duplication (confirmed by reading the code, not by claim)

All in `crates/ggen-lsp/src/`.

### 1.1 `state.rs` — three parallel flagged-sets + three clears_for + three detect_*_for + three branches

| Concern | TPL-001 | HARNESS-001 | OUT-001 | Lines |
|---|---|---|---|---|
| Flagged set field | `tpl_flagged: Arc<Mutex<HashSet<Url>>>` | `harness_flagged: …` | `out_flagged: …` | 88, 94, 102 |
| Field init | `Arc::new(Mutex::new(HashSet::new()))` ×3 | — | — | 142–144 |
| `clears_for` | `tpl_clears_for` | `harness_clears_for` | `out_clears_for` | 159–168, 179–188, 199–208 |
| `detect_*_for` | `detect_tpl_001_for` | `detect_harness_001_for` | `detect_out_001_for` | 669–677, 722–730, 685–693 |
| `analyze_and_observe` detect step | `detect_tpl_001_for(uri)` gated on `tpl_is_trigger` | `detect_harness_001_for` gated on `harness_is_trigger` | `detect_out_001_for` gated on `tpl_is_trigger` | 497–524 |
| `analyze_and_observe` publish loop | self-merge / publish over `current_flagged` | over `current_harness_flagged` | over `current_out_flagged` | 528–593 |
| `analyze_and_observe` stale-clear | `tpl_clears_for` + residual | `harness_clears_for` + residual | `out_clears_for` + residual | 607–641 |
| `close_document` reconcile | TPL block | HARNESS block | OUT block | 386–436 |

The three `*_clears_for` methods are **byte-identical** except for the field they lock (`tpl_flagged` / `harness_flagged` / `out_flagged`): prev-minus-current keyed subtraction, exclude `edited`, store `current` for next pass.

The three `detect_*_for` methods differ ONLY in (a) which root-walk they use (`project_root_for` vs `harness_root_for`) and (b) which index + pure detector they invoke. **Both return the identical type `Vec<(PathBuf, Vec<Diagnostic>)>`** — the heterogeneity (`ProjectIndex` vs `HarnessIndex`) is already erased at this boundary.

### 1.2 `check.rs` — three folds

`fold_tpl_001` (411–460), `fold_harness_001` (476–519), `fold_out_001` (537–580). Byte-identical except:
- TPL/OUT call `ProjectIndex::from_root`; HARNESS calls `HarnessIndex::from_root`.
- the pure detector invoked (`detect_tpl_001` / `detect_harness_001` / `detect_out_001`).
- TPL reads the **template** content for route context; HARNESS/OUT read the **manifest** content.

The body (skip-empty, count errors, read content, resolve routes when `registry.is_some()`, append-or-create `FileReport` via `paths_match`) is identical across all three.

### 1.3 `route/diagnostic_species.rs` — the registry (metadata)

Already a clean data table (`static SPECIES: &[DiagnosticSpecies]`). 3 entries, all `detector_active: true`. This is the metadata authority and is the natural place for the routing predicate (basename → species) to be derived from, but **its struct is `#[derive(Copy)]` with all `&'static` fields** and is consumed by `route::registry` and tests that assert exact field values (`diagnostic_species::tests`). Touching its shape is high-blast-radius and unnecessary for this checkpoint.

---

## 2. Per-species variation (bounds what is unifiable)

1. **Index type differs.** TPL + OUT detect over a `ProjectIndex` (`ggen.toml`-rooted, via `project_root_for`). HARNESS detects over a `HarnessIndex` (`Cargo.toml`-rooted, via `harness_root_for`). **DIFFERENT index types, DIFFERENT root walks.**
2. **Surface routing differs by basename.** `ggen.toml` → TPL + OUT (both ride `tpl_is_trigger`). `Cargo.toml`/`Makefile.toml` → HARNESS (`harness_is_trigger`). `.tera`/`.rq`/`.sparql` → TPL + OUT only. The two trigger sets are **disjoint by basename**.
3. **OUT shares TPL's trigger gate** (both ProjectIndex-derived) but anchors on a DIFFERENT surface (`ggen.toml` vs the `.tera` body). The self-merge ordering in `analyze_and_observe` is subtle: OUT's self-merge is guarded by `&& !published_self` (line 583) because a `ggen.toml` edit could be claimed by neither TPL (anchors on `.tera`) — so OUT must check it wasn't already self-published. **This ordering is load-bearing and must be preserved exactly.**

**Conclusion on heterogeneity:** a single fn-pointer table over the *pure* detectors (`fn(&ProjectIndex)` vs `fn(&HarnessIndex)`) CANNOT cleanly hold both — the signatures differ. BUT the `detect_*_for(&self, uri: &Url) -> Vec<(PathBuf, Vec<Diagnostic>)>` wrappers already homogenize them. **That wrapper boundary — not the pure detector — is the lawful seam.**

---

## 3. HARD CONSTRAINT discovered: `tpl_clears_for` is a public test contract

`crates/ggen-lsp/tests/ggen_tpl_001_stale_clear.rs` calls
`state.tpl_clears_for(&edited, &current).await` at lines **123, 144, 264, 284**
(also `ggen_tpl_001_living_loop.rs` references it in doc prose).

`tpl_clears_for` is therefore a **frozen public API**. It MUST keep its exact signature
`pub async fn tpl_clears_for(&self, edited: &Url, current: &HashSet<Url>) -> Vec<Url>`
and exact behavior. The safety net forbids editing the test, so I cannot rename or remove it.

`harness_clears_for` / `out_clears_for` have **NO external callers** (grep of `tests/` and `src/` outside `state.rs` is empty). They are free to be removed/internalized.

This asymmetry sets the lawful extent: the unified primitive becomes the *implementation*, and `tpl_clears_for` becomes a thin preserved shim that delegates to it.

---

## 4. LAWFUL EXTENT decision (what is safe to unify, what is scoped out)

### SAFE — unify (this checkpoint)

**S1. Flagged-sets → one map.** Replace the three `Arc<Mutex<HashSet<Url>>>` fields with one
`flagged: Arc<Mutex<HashMap<&'static str, HashSet<Url>>>>` keyed by species code
(`"GGEN-TPL-001"` / `"GGEN-HARNESS-001"` / `"GGEN-OUT-001"`). One field, one init.

**S2. Generic `clears_for(code, edited, current)`.** One private method does the keyed
subtraction against `flagged[code]`. The three public/internal names collapse onto it:
- `tpl_clears_for` is **kept** as a public shim → `self.clears_for(GGEN_TPL_001, edited, current)` (preserves the test contract).
- `harness_clears_for` / `out_clears_for` are **removed**; their two call sites in `analyze_and_observe` + `close_document` call `clears_for(code, …)` directly.

**S3. Detect step behind a thin dispatch (enum, not fn-pointer).** Introduce a small private
helper `detect_for(&self, code, uri) -> Vec<(PathBuf, Vec<Diagnostic>)>` that `match`es on the
species code and calls the existing `detect_tpl_001_for` / `detect_harness_001_for` /
`detect_out_001_for`. This is the **natural** abstraction given heterogeneous index types: the
`match` lives in ONE place, and the three private `detect_*_for` methods are RETAINED unchanged
(they already homogenize ProjectIndex vs HarnessIndex). No forced fn-pointer table over
incompatible signatures.

**S4. `check.rs` folds → one generic fold.** Extract the identical fold body into
`fn fold_species(root, files, registry, groups: Vec<(PathBuf, Vec<Diagnostic>)>) -> usize`
that takes the **already-computed groups** (the only per-species difference — index type +
detector — is resolved by the caller). The three `fold_*_001` shrink to one-liners that compute
their groups and delegate. Error-count semantics are byte-identical (caller sums the return).

### SCOPED OUT — documented follow-on (NOT forced here)

**F1. `analyze_and_observe` publish loops are NOT fully merged.** The three publish loops
(`current_flagged` / `current_harness_flagged` / `current_out_flagged`) share shape but the OUT
loop's `&& !published_self` guard (line 583) and the per-species self-merge interleave are
order-sensitive across all three loops sharing one `own_diags` (consumed by `std::mem::take`).
Collapsing them into one data-driven loop would require reifying "anchor surface + self-merge
policy + published_self interaction" into per-species metadata and re-proving the take/merge
ordering. That is a behavior-risk surface. **Unifying the flagged-set + clears_for + detect
dispatch (S1–S3) removes the bulk of the triplication; the publish-loop merge is a separate,
larger refactor that should be its own checkpoint (CONSOLIDATE-002) with its own oracle.**
Forcing it now would risk FAKE-LIVE (subtle reordering of OCEL events / self-merge).

**F2. `diagnostic_species.rs` struct unchanged.** The basename→species routing predicate is
NOT moved into the registry struct this checkpoint (would change a `Copy` `&'static` struct
consumed by route::registry + asserted field-by-field in tests). The trigger predicates
(`is_ggen_manifest` / `is_harness_surface`) stay where they are. Follow-on: derive routing from
`DiagnosticSpecies.surfaces`.

---

## 5. ALIVE / FAKE-LIVE / BLOCKED — concrete

**ALIVE (success):**
- `cargo make` build green; `cargo clippy -p ggen-lsp --no-deps -- -D warnings` = 0; `cargo fmt --check` clean.
- ALL existing species tests pass UNCHANGED (no test edited):
  `ggen_tpl_001_living_loop` 5, `ggen_tpl_001_stale_clear` 3, `ggen_tpl_001_regression` 9,
  `ggen_tpl_001_did_close_clear` 2, `ggen_out_001_living_loop` 7, `ggen_harness_001_living_loop` 9
  (note: grep counts 9 test attrs in harness file; spec says 7 — run authoritative).
- Full `cargo test -p ggen-lsp` suite green.
- The 6-link OCEL chain (DiagnosticRaised → RouteSelected → RepairSuggested → RepairApplied → GatePassed → ReceiptEmitted) still emits per species (verified via the living_loop tests that already assert it).
- `species_registry().len() == 3`, all `detector_active`.
- `tpl_clears_for` retains exact signature + behavior (shim).

**FAKE-LIVE (failure — reject):**
- A species stops firing, clears the wrong URI, or the OCEL chain changes shape/order.
- Any safety-net test had to be edited to pass.
- The OUT `&& !published_self` ordering drifted (event interleave changed).
- `harness_clears_for`/`out_clears_for` removed but a hidden caller existed (build break — would be caught, but flagged here).

**BLOCKED (partial delivery + document the rest):**
- If collapsing the publish loops (F1) cannot be done without changing OCEL event order → deliver S1–S4 only, document F1 as CONSOLIDATE-002. (This is the EXPECTED outcome: F1 is deliberately scoped out.)

---

## 6. Exact file-change manifest

| File | Change |
|---|---|
| `crates/ggen-lsp/src/state.rs` | Replace 3 flagged fields with 1 `HashMap`; add private `clears_for` + `detect_for`; keep `tpl_clears_for` as shim; delete `harness_clears_for`/`out_clears_for`; rewrite their call sites in `analyze_and_observe` + `close_document` to use the generic primitives. Publish loops UNCHANGED (F1 scoped out). |
| `crates/ggen-lsp/src/check.rs` | Add `fold_species(...)`; shrink `fold_tpl_001`/`fold_harness_001`/`fold_out_001` to delegate. Folds keep computing their own groups (index heterogeneity) but share the append/route/count body. |
| `crates/ggen-lsp/src/route/diagnostic_species.rs` | Add `pub const` code constants (`GGEN_TPL_001`, `GGEN_HARNESS_001`, `GGEN_OUT_001`) as the single source of the map keys, so state.rs does not string-literal them. NO struct/table change. |

Nothing outside `crates/ggen-lsp/` (except this doc). No `#[allow]`. No commit. Single-writer.
`crates/ggen-lsp/src/intel/mine.rs` UNTOUCHED.

---

## 7. Authority/drift justification (coding-agent-mistakes gate)

- **Deepens authority:** a future 4th ProjectIndex/HarnessIndex species becomes (a) one map key, (b) one `match` arm in `detect_for`, (c) one `fold_species` call — not three copy-pasted branches. The keyed `clears_for` makes "which flagged set" data, not code.
- **Reduces drift:** the three byte-identical `clears_for` bodies (a known divergence hazard — fix one, forget the others) become one. The three fold bodies likewise.
- **No new bypass:** `tpl_clears_for` shim preserves the existing authoritative entry the test asserts. The generic primitive is the ONLY implementation (no dual path).
