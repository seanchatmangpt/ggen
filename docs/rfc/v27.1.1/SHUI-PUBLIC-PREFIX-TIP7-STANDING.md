# shui_public_prefix_court — tip7 serial honesty retry (v27.1.1)

```text
gate_reason=peer_taffy_tip6_EXIT0_then_serial_retry_after_ENOSPC
tip_base=ae34bf4bcafe1cc2a86fe9598d9769fe1b9dcdc4
live_crown_at_ledger=ef3df3a8e31b4ea49e3057d4b95cf21562fe5127
cited_kickoff_tip=f12972590e280609ddaedeafad84cda63372ea4b
branch=cursor/v2711-shui-tip7-retry-506b
bc=bc-e40d4b25-d2c9-55e1-9ff5-abdf442d506b
prior_agent=bc-cce08684-5177-553d-9ac4-69871515c0dc
court=shui_public_prefix_court
blob=3cda065f6564917504f241ac3626c111cd2dfb6a
tests=4
budget_s=1500
exit=0
standing=ALIVE
prior_tip7_partial=d35482374 EXIT101 (TARGET_DIR_WIPED / ENOSPC mid-oxrocksdb)
prior_exact_tip7_partial=881f2ec16 EXIT137
prior_tip6_partial=c661d228a EXIT137
prior_alive_lineage=008a81d21 / sealed a4b3d3fb4 (4 passed; tip-cargo ALIVE held)
peer_taffy_tip6=ALIVE EXIT0 pid=3873369 /tmp/gate1-taffy-tip6
crown_absorb=PENDING
ManagePullRequest=not_used
force_push=none
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-TIP7-STANDING` |
| Branch | `cursor/v2711-shui-tip7-retry-506b` |
| Agent | `bc-e40d4b25-d2c9-55e1-9ff5-abdf442d506b` (follow-up from `bc-cce08684`) |
| Tip base at cargo | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `ae34bf4bcafe1cc2a86fe9598d9769fe1b9dcdc4` (court blob unchanged vs sealed ALIVE) |
| Live crown at ledger | `ef3df3a8e31b4ea49e3057d4b95cf21562fe5127` (docs-only FF past cargo tip; court blob ≡) |
| Court sources on tip | `crates/ggen-graph/tests/shui_public_prefix_court.rs` blob `3cda065f6564917504f241ac3626c111cd2dfb6a` (**4** `#[test]`) |
| sha256 (cargo tree) | court `bd032856…5fe0`; `public_prefixes.rs` `bff5aa82…ee5c`; `shui.rs` `d7040f32…78d2` |
| Exact command | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/tip7-serial cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/tip7-serial` **only** (seeded from `/tmp/gate1-taffy-tip6` warm; **never** deleted peer `/tmp/gate1-taffy-tip6`) |
| Budget | **1500s** (25min) hard `timeout` |
| This probe | **EXIT:0** — **4 passed; 0 failed** @ `2026-11-22T19:38:08Z`→`19:38:11Z` (~3s wall; warm target) |
| Prior tip7 thrash | [`SHUI-PUBLIC-PREFIX-TIP7-STANDING.md`](./SHUI-PUBLIC-PREFIX-TIP7-STANDING.md) prior land — **PARTIAL_ALIVE** EXIT:**101** absorb `d35482374` (target wiped mid-`oxrocksdb-sys`) — **superseded** by this EXIT:**0** for tip7 tip-subject |
| Peer gate | Waited until pid `3873369` (`taffy_aot_preflight` `/tmp/gate1-taffy-tip6`) exited — status `TAFFY_EXIT=0` |
| Disk | `df` before orphan clean `113G/141G avail (45%)`; after `112G/142G (45%)`; at cargo `145G/109G (58%)` after warm seed |
| Log / status | `/tmp/cursor/shui-tip7-retry.log` · `/tmp/cursor/shui-tip7-retry.status` → `SHUI_EXIT=0` |
| Prior tip-cargo ALIVE (lineage) | **held** — 4 passed; EXIT 0 @ `a4b3d3fb4` / lineage `008a81d21` (blob `3cda065f`) |
| Crown absorb | **PENDING** tip-wins onto `cursor/ecosystem-spec-27-1-1-bc54` |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Tip7 serial retry after peer Taffy tip6 EXIT:**0** and orphan tip7 dir cleanup: exclusive tip-cargo obtained EXIT **0** with **4** passed on tip `ae34bf4bc` → standing **ALIVE (exact tip-cargo)**. Prior tip7 EXIT:**101** PARTIAL is superseded for tip7 tip-subject; sealed lineage ALIVE @ `a4b3d3fb4` remains held. Do **not** invent photolithography path crown `ALIVE` or Foundry `ALIVE` from this fence alone.

**Honesty law (held):**
`PRESENT` ≠ `lineage_tip_cargo_ALIVE` ≠ `exact_tip_reconfirm_ALIVE` ≠ Foundry `ALIVE`.

---

## Why ALIVE

| Check | Result |
|-------|--------|
| Tip carries strengthened `shui_public_prefix_court` (4 tests; blob `3cda065f`) | **YES** |
| Court blob ≡ prior ALIVE blob `3cda065f` | **YES** |
| Gate: peer pid `3873369` exited before cargo | **YES** — `TAFFY_EXIT=0` |
| Exact isolated tip-cargo EXIT 0 @ live tip within 25min | **YES** — EXIT **0**; **4 passed; 0 failed** |
| Serial `--test-threads=1` / `CARGO_TARGET_DIR=/tmp/tip7-serial` | **held** |
| Lineage tip-cargo ALIVE @ `a4b3d3fb4` / `008a81d21` | **held** |

### Tip-cargo evidence (verbatim)

```text
running 4 tests
test shui_namespace_is_w3c_shacl_ui_slash_iri ... ok
test shui_prefix_is_admitted_and_private_prefixes_remain_refused ... ok
test shui_ui_property_group_and_editor_iris_are_open_admitted ... ok
test unknown_turtle_prefix_is_refused_unknown ... ok

test result: ok. 4 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.00s
SHUI_EXIT=0
```

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ ae34bf4bc; court blob 3cda065f
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/tip7-serial
timeout 1500s cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; 0 failed; EXIT 0 → exact tip-cargo ALIVE (replay)
```

---

## Compact return

```text
tip_base=ae34bf4bcafe1cc2a86fe9598d9769fe1b9dcdc4
branch=cursor/v2711-shui-tip7-retry-506b
bc=bc-e40d4b25
EXIT=0
tests=4
standing=ALIVE
target=/tmp/tip7-serial
peer_exit=0
lineage_alive=a4b3d3fb4/008a81d21
prior_partial=d35482374_EXIT101_superseded
crown_absorb=PENDING ManagePullRequest=not_used force_push=none Foundry_ALIVE=not_claimed
```
