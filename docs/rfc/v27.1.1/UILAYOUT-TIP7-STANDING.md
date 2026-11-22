# ui_layout_court — tip7 serial honesty after shui tip7 EXIT0 (v27.1.1)

```text
gate_reason=shui_tip7_retry_EXIT0_then_serial_uilayout
tip_base=ae34bf4bcafe1cc2a86fe9598d9769fe1b9dcdc4
live_crown_at_ledger=ef3df3a8e31b4ea49e3057d4b95cf21562fe5127
branch=cursor/v2711-shui-tip7-retry-506b
bc=bc-e40d4b25-d2c9-55e1-9ff5-abdf442d506b
court=ui_layout_court
blob=1dfe6bdfab2cb8dfd35fa7870692185cf004ab4c
tests=11
budget_s=1500
exit=0
standing=ALIVE
prior_exact_tip6=PARTIAL_then_absorbed_ALIVE@7b38c25b4
prior_alive_lineage=a508e6bae / sealed 2685119bd (11 passed)
shui_tip7_peer=ALIVE EXIT0 @ ae34bf4bc /tmp/tip7-serial
crown_absorb=PENDING
ManagePullRequest=not_used
force_push=none
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-TIP7-STANDING` |
| Branch | `cursor/v2711-shui-tip7-retry-506b` |
| Agent | `bc-e40d4b25-d2c9-55e1-9ff5-abdf442d506b` |
| Tip base at cargo | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `ae34bf4bcafe1cc2a86fe9598d9769fe1b9dcdc4` |
| Court blob on tip | `1dfe6bdfab2cb8dfd35fa7870692185cf004ab4c` — **11** `#[test]` |
| sha256 (cargo tree) | court `d0fb1364…4799` |
| Exact command | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/tip7-serial cargo test -p ggen-graph --test ui_layout_court --locked --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/tip7-serial` (same serial dir as shui tip7 retry; warm; **never** deleted `/tmp/gate1-taffy-tip6`) |
| Budget | **1500s** (25min) hard `timeout` |
| This probe | **EXIT:0** — **11 passed; 0 failed** @ `2026-11-22T19:54:04Z`→`19:54:10Z` (~6s wall) |
| Gate | Ran only after shui tip7 retry EXIT:**0** on same `/tmp/tip7-serial` |
| Log / status | `/tmp/cursor/uilayout-tip7-retry.log` · `/tmp/cursor/uilayout-tip7-retry.status` → `UILAYOUT_EXIT=0` |
| Sibling `ui-layout-court.ts` | **UNSUPPORTED** |
| Prior tip-cargo ALIVE (lineage) | **held** — **11 passed; EXIT 0** @ `2685119bd` / lineage `a508e6bae` |
| Crown absorb | **PENDING** tip-wins onto `cursor/ecosystem-spec-27-1-1-bc54` |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Tip7 serial honesty after shui tip7 retry EXIT:**0**: exclusive tip-cargo obtained EXIT **0** with **11** passed on tip `ae34bf4bc` → standing **ALIVE (exact tip-cargo)**. Do **not** invent photolithography path crown `ALIVE` or Foundry `ALIVE` from this fence alone. Sibling TS adapter remains **UNSUPPORTED**.

**Honesty law (held):**
`PRESENT` ≠ `lineage_tip_cargo_ALIVE` ≠ `exact_tip_reconfirm_ALIVE` ≠ Foundry `ALIVE`.

---

## Why ALIVE

| Check | Result |
|-------|--------|
| Tip carries `ui_layout_court` (11 tests; blob `1dfe6bdf`) | **YES** |
| Shui tip7 retry EXIT 0 before this cargo | **YES** |
| Exact isolated tip-cargo EXIT 0 @ live tip within 25min | **YES** — EXIT **0**; **11 passed; 0 failed** |
| Serial `/tmp/tip7-serial` / `--test-threads=1` | **held** |
| Lineage tip-cargo ALIVE @ `2685119bd` / `a508e6bae` | **held** |

### Tip-cargo evidence (verbatim)

```text
running 11 tests
test anti_vacuity_mutation_widened_text_is_refused_not_vacuous_pass ... ok
test consume_does_not_reimplement_geometry_on_prebuilt_seal ... ok
test empty_embedded_digest_on_seal_is_refused ... ok
test geometry_overflow_refuses_before_consume ... ok
test json_fixture_feeds_nodes_into_taffy_aot_and_admits ... ok
test mismatched_digest_is_refused ... ok
test missing_pinned_digest_is_refused ... ok
test missing_seal_is_refused ... ok
test six_width_seal_under_1_5x_is_consumed_alive ... ok
test stale_embedded_digest_is_refused ... ok
test ts_contract_stub_documents_zcode_cli_boundary ... ok

test result: ok. 11 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
UILAYOUT_EXIT=0
```

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ ae34bf4bc; court blob 1dfe6bdf
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/tip7-serial
timeout 1500s cargo test -p ggen-graph --test ui_layout_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 11 passed; EXIT 0 → exact tip-cargo ALIVE (replay)
```

---

## Compact return

```text
tip_base=ae34bf4bcafe1cc2a86fe9598d9769fe1b9dcdc4
branch=cursor/v2711-shui-tip7-retry-506b
bc=bc-e40d4b25
EXIT=0
tests=11
standing=ALIVE
target=/tmp/tip7-serial
shui_tip7=ALIVE
lineage_alive=2685119bd/a508e6bae
crown_absorb=PENDING ManagePullRequest=not_used force_push=none Foundry_ALIVE=not_claimed
```
