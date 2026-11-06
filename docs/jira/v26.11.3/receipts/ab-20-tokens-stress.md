# AB-20 Tokens Stress Receipt

Standing: PASS

Lane: AB-20 (STRESS). Date: 2026-11-04. Subject: `crates/ggen-lsp/src/tokens.rs`
`TokensEngine` full/delta path (E80-5/PG-A2 wired surface), read-only.

## Deliverable

New stress court: `crates/ggen-lsp/tests/tokens_delta_stress.rs` — 4 tests,
real collaborators only (real `.specify/repo-facts.ttl` oxigraph parse, real
`ggen.toml`, real ggen.toml-declared generated-Rust files tiled into the
fixture, real tree-sitter-rust parse per mint). No mocks. Engine
(`tokens.rs`) untouched.

## Fixture

Five real ggen.toml-declared generated surfaces tiled in `mod` blocks until
≥5k lines (5440 lines observed):
`generated_commands.rs` (104), `cli_proof_tests.rs` (614),
`generated_diagnostic_codes.rs` (63), `ggen-abb-sbb/refusals.rs` (217),
`generated_contract.rs` (75) — ~1088 lines/tile, 5 tiles.

## Laws verified

1. **10k didChange churn soak** (`soak_10k_did_change_cycles`): deterministic
   LCG churn (line rename / comment insert, text change asserted per cycle).
   Every delta applied cleanly (`apply_edits` never `None`); resultId chain
   strictly monotone `tok-N`, counter advance = exactly 1 per mint; counter
   conservation re-read at end (2 census + 1 baseline + 10,000 deltas + 20
   sampled fulls = tok-10023); sampled delta ≡ fresh-full byte equality every
   500th cycle (20 verifications); generation-slot census (behavioral probe:
   slot present, stored-len replace-all on stale id) identical before/after
   — no slot leak; per-URI slot isolation holds post-soak.
2. **17 edit shapes × 3 size classes** (`edit_shapes_delta_equals_full_across_size_classes`):
   prepend / append / replace-mid / delete-span / multi-change(3) /
   multi-change(5) / half-block / duplicate-block / swap / rename-all /
   empty-line / no-op (17 shapes pinned by count assert) over tiny (~50) /
   medium (~500) / large (~5k) fixtures: applied delta byte-equal to fresh
   full re-classification (encoded arrays AND decoded tokens); no-op yields
   zero edits; prepend edit starts at array offset 0.
3. **50%-replace worst case** (`half_replace_worst_case_scale`): single
   didChange replacing 2720/5440 lines. Correctness: applied delta ==
   fresh full. Cost recorded:
   - debug: 4,308,477 us (4.3 s/delta)
   - release: 841,224 us (841 ms/delta)
   - old array 2200 u32, new array 14790 u32, 1 edit,
     touched 14,620 u32 elements (664.5% of old array — worst case: the
     prefix/suffix diff keeps nothing; every downstream coordinate shifts).
4. **Alloc census per cycle** (`alloc_census_per_delta_cycle`): counting
   global allocator (real System underneath, observe-only), 100 measured
   cycles over the 5k fixture: avg 18,478 allocs/cycle, avg 1,698,265
   bytes/cycle (release, gate run 1; 1,807,086 allocs total in an earlier
   debug run: 18,070/cycle, 1,678,896 B/cycle). Baseline, not a budget —
   the engine is a host facade (full re-classify per mint).

## Gates (actual commands + exits)

- Gate run 1: `cargo test -p ggen-lsp --release --test tokens_delta_stress`
  → exit 0, **4 passed; 0 failed** (soak 2378.87 s).
- Gate run 2: same command → exit 0, **4 passed; 0 failed** (soak 2327.41 s).
- Suite: `cargo test -p ggen-lsp -- --skip soak_10k_did_change_cycles
  --skip edit_shapes_delta_equals_full_across_size_classes --skip
  half_replace_worst_case_scale --skip alloc_census_per_delta_cycle` →
  exit 0 (fail-fast; every test binary green including
  `manifest_contract_test`). The four stress tests are excluded by
  test-name skips and covered by the dedicated release gates above.

## Disclosures

- **Profile**: gates run in `--release`. Measured debug-profile cost is
  ~4.3 s per delta on the 5k fixture (half-replace probe), projecting a
  ~12 h debug soak — structurally infeasible; release soak is ~29-40 min.
- **Out-of-lane one-line unblock (disclosed per SLA)**: suite gate was red
  at `manifest_contract_test` because the workspace version bump to 26.11.3
  (another lane) had not synced the plugin version in
  `/Users/sac/ggen/.claude-plugin/marketplace.json` (still 26.10.12).
  Synced the single field to 26.11.3; the version-drift guard then passed.
  Owner of that file supersedes this diff.
- **Test-authoring bug found and fixed in-lane** (never a tokens.rs defect):
  first soak run failed on the test's own mint-accounting assertion
  (expected 10026 vs actual 10023 — the formula double-counted the baseline
  full and counted the census-after mints that occur after the read).
  Fixed; both subsequent gate runs green.
- A concurrent sibling-lane debug soak process (not this lane's) ran for
  most of this window and contended for CPU; not killed, times above are
  therefore upper bounds.
