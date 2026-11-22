# Taffy AOT preflight — tip7 exact-tip honesty (v27.1.1)

```text
tip_base=63da993cc194ff2177ee757e01c883b582028493
cited_tip_at_kickoff=f0a0e07bc
branch=cursor/v2711-taffy-preflight-tip7-bc54
bc=bc-bf13a704-0967-5b0c-adb6-35bc69273006
court=taffy_aot_preflight_court
blob=d8923b07eb4325ace90087a604b9daec0a83d29e
tests=7
budget_s=1500
exit=0
standing=ALIVE
lineage_alive=19c08a3b0 / absorb cde02d1f2
exact_tip_reconfirm=ALIVE (EXIT:0 @ 63da993cc)
crown_absorb=ABSORBED tip-wins (this land)
ManagePullRequest=not_used
force_push=none
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `TAFFY-AOT-PREFLIGHT-TIP7-STANDING` |
| Branch | `cursor/v2711-taffy-preflight-tip7-bc54` |
| Agent | `bc-bf13a704-0967-5b0c-adb6-35bc69273006` |
| Tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `63da993cc194ff2177ee757e01c883b582028493` (live FF beyond kickoff cite `f0a0e07bc…`) |
| Court blob on tip | `d8923b07eb4325ace90087a604b9daec0a83d29e` — **7** `#[test]` |
| Exact command | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-tip7-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/gate1-tip7-taffy` (isolated; seeded from `/tmp/gate1-taffy` warm 2.0G) |
| Budget | **1500s** (25min) hard `timeout` |
| This probe | **EXIT:0** — **7 passed; 0 failed** @ `2026-11-22T18:35:56Z`→`18:59:34Z` (compile ~23m38s under concurrent thrash; tests 0.00s) |
| Log / status | `/tmp/cursor/taffy-tip7.log` · `/tmp/cursor/taffy-tip7.status` → `TAFFY_EXIT=0` |
| Lineage tip-cargo ALIVE | **held** — **7 passed; EXIT 0** @ `19c08a3b0` / absorb `cde02d1f2` |
| Prior tip6 | `cursor/v2711-taffy-preflight-tip6-bc54` — **PARTIAL_ALIVE** EXIT:**137** under thrash — [`TAFFY-AOT-PREFLIGHT-TIP6-STANDING.md`](./TAFFY-AOT-PREFLIGHT-TIP6-STANDING.md) |
| Peer exact-tip ALIVE | [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md) — EXIT:**0** @ `2685119bd` (ancestor of tip7 base; tip7 reconfirms on live tip) |
| Crown absorb | **ABSORBED** tip-wins onto `cursor/ecosystem-spec-27-1-1-bc54` (pre-absorb tip below; side `ff0b27e9d`) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Tip7 push-first honesty: exclusive tip-cargo obtained EXIT **0** with **7** passed on tip `63da993cc` → standing **ALIVE (exact tip-cargo)**. Lineage tip-cargo **ALIVE** @ `19c08a3b0` remains held. Do **not** invent photolithography path crown `ALIVE` or Foundry `ALIVE` from this fence alone.

**Honesty law (held):**
`PRESENT` ≠ `lineage_tip_cargo_ALIVE` ≠ `exact_tip_reconfirm_ALIVE` ≠ photolithography path crown `ALIVE`.

---

## Why ALIVE

| Check | Result |
|-------|--------|
| Tip carries `taffy_aot_preflight_court` (7 tests; blob `d8923b07e`) | **YES** |
| Exact isolated tip-cargo EXIT 0 @ live tip within 25min | **YES** — EXIT **0**; **7 passed; 0 failed** |
| Lineage tip-cargo ALIVE @ `19c08a3b0` | **held** |

### Tip-cargo evidence (verbatim)

```text
running 7 tests
test layout_refusal_gap_too_small_names_width ... ok
test layout_refusal_overlap_names_width_and_nodes ... ok
test layout_refusal_touch_target_too_small_names_width_and_node ... ok
test overflow_at_375_with_text_spacing_1_5x_is_refused ... ok
test six_width_seal_admits_valid_layout_under_1_5x ... ok
test six_width_seal_refuses_overflow_only_at_320_naming_failing_width ... ok
test valid_layout_at_375_with_text_spacing_1_5x_is_admitted ... ok

test result: ok. 7 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
TAFFY_EXIT=0
```

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ 63da993cc; court blob d8923b07e
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/tmp/gate1-tip7-taffy \
  cargo test -p ggen-graph --test taffy_aot_preflight_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 7 passed; EXIT 0 → exact tip-cargo ALIVE (replay)
```

---

## Compact return

```text
tip_base=63da993cc194ff2177ee757e01c883b582028493
branch=cursor/v2711-taffy-preflight-tip7-bc54
bc=bc-bf13a704
EXIT=0
tests=7
standing=ALIVE
target=/tmp/gate1-tip7-taffy
lineage_alive=19c08a3b0/cde02d1f2
crown_absorb=ABSORBED ManagePullRequest=not_used force_push=none Foundry_ALIVE=not_claimed
```
