# CONSOLIDATE-001 — Receipt

**Kind:** REFACTOR (deepen authority + reduce drift). Behavior byte-equivalent. No test edited. No commit.
**Verdict:** ALIVE (safe partial per design — F1 publish-loop merge scoped out as CONSOLIDATE-002, not forced).

## Files changed (only these; + this receipt)
- `crates/ggen-lsp/src/state.rs` — 3 `*_flagged` fields → 1 `flagged: Arc<Mutex<HashMap<&'static str, HashSet<Url>>>>`; 3 `*_clears_for` → 1 generic `clears_for(code, edited, current)` + a preserved public `tpl_clears_for` shim; 4 clears call-sites rekeyed by species code; 1 broken intra-doc link (`Self::harness_clears_for`) repointed to `Self::clears_for`. The 3 private `detect_*_for` helpers RETAINED (irreducible ProjectIndex vs HarnessIndex heterogeneity). Publish loops in `analyze_and_observe` byte-for-byte untouched.
- `crates/ggen-lsp/src/check.rs` — 3 `fold_*_001` bodies → 1 shared `fold_species(files, registry, groups)`; the 3 folds shrink to `index-build + delegate`. Call sites (`error_count += fold_*`) unchanged.

## Keys
Used existing `crate::analyzers::{GGEN_TPL_001, GGEN_HARNESS_001, GGEN_OUT_001}` (`&str`, byte-equal to the registry `code` fields) as the map keys — no new constant, no second source of the literal. `route/diagnostic_species.rs` and `route/mod.rs` therefore unchanged (registry/struct/table intact: `species_registry().len() == 3`).

## What was unified vs left branched
UNIFIED (byte-identical-modulo-name → no behavior change):
1. flagged sets: 3 fields → 1 HashMap keyed by species code.
2. clears_for: 3 methods → 1 generic; `tpl_clears_for` kept as a public shim (4 live test call-sites in `ggen_tpl_001_stale_clear.rs` compile unchanged).
3. fold tail: 3 `fold_*_001` → 1 `fold_species` consuming `Vec<(PathBuf, Vec<Diagnostic>)>`.

LEFT BRANCHED (lawful — forcing them would change behavior or be an unnatural abstraction):
- 3 private `detect_*_for` (TPL/OUT over ProjectIndex/ggen.toml-root; HARNESS over HarnessIndex/Cargo.toml-root): heterogeneous index types + root resolvers; a single fn-pointer table would be a forced abstraction. A thin `detect_for(code)` match dispatcher was NOT added because no call site needed it (the publish loops were left intact per F1 scope-out) — adding it would have been dead code under `-D warnings`.
- 3 publish loops in `analyze_and_observe` (the OUT loop's `&& !published_self` self-merge guard + shared-`own_diags` `std::mem::take` interleave is order-sensitive; merging risks OCEL-sequence drift = FAKE-LIVE). Documented follow-on: **CONSOLIDATE-002**.

## Net result
A 4th ProjectIndex/HarnessIndex species = 1 map key (its code) + 1 `detect_X_for` + 1 fold one-liner + (if Project-family) reuse of `clears_for` — not 3 copy-pasted clears + 3 copy-pasted folds + a 4th field. Authority deepened (one keyed reconciler, one fold body), drift reduced (single flagged-set store).

## Proof tails

### cargo make check
```
Checking ggen-lsp v26.5.29 (/Users/sac/ggen/crates/ggen-lsp)
Checking ggen-lsp-mcp v26.5.29 / ggen-lsp-a2a v26.5.29
Finished `dev` profile [unoptimized + debuginfo] target(s) in 2.11s
Build Done in 3.05 seconds.
```

### cargo clippy -p ggen-lsp --no-deps -- -D warnings  (0 warnings)
```
Checking ggen-lsp v26.5.29 (/Users/sac/ggen/crates/ggen-lsp)
Finished `dev` profile [unoptimized + debuginfo] target(s) in 2.19s
```

### cargo fmt -p ggen-lsp -- --check  (clean)
```
FMT_EXIT=0   (no diff)
```

### cargo test -p ggen-lsp  (all green; lib 163 passed; no test file modified)
```
ggen_tpl_001_living_loop      : 5 passed
ggen_tpl_001_stale_clear      : 3 passed   (exercises tpl_clears_for shim)
ggen_tpl_001_regression       : 9 passed
ggen_tpl_001_did_close_clear  : 2 passed
ggen_out_001_living_loop      : 7 passed
ggen_harness_001_living_loop  : 7 passed
lib (incl. diagnostic_species, check) : 163 passed; 0 failed
0 failed across the full suite; Doc-tests: 0
```

git status: only `crates/ggen-lsp/src/{check.rs,state.rs}` modified; no `tests/` file touched.
