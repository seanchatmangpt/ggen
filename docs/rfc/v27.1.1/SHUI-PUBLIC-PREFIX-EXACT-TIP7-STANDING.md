# shui_public_prefix_court — exact-tip7 push-first honesty (v27.1.1)

```text
tip_base=59b9fae1cfab7ebf263f98068aa76cf42907d7c9
cited_tip_at_kickoff=c4e757a14c91c6512438a8a94bbde304b0a6e921
branch=cursor/v2711-shui-exact-tip7-bc54
bc=bc-7a87dbfb-2ef5-5341-98ea-ba0a6e436091
court=shui_public_prefix_court
budget_s=1500
exit=137
standing=PARTIAL_ALIVE
prior_tip6_partial=c661d228a EXIT137
prior_alive_lineage=008a81d21 (4 passed; tip-cargo ALIVE held)
peer_tip7_cargos=bc-8c208d11 /tmp/gate1-shui-alive + concurrent /tmp/gate1-tip7-shui (not this probe)
crown_absorb=none
ManagePullRequest=not_used
force_push=none
Foundry_ALIVE=not_claimed
```

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-EXACT-TIP7-STANDING` |
| Branch | `cursor/v2711-shui-exact-tip7-bc54` |
| Agent | `bc-7a87dbfb-2ef5-5341-98ea-ba0a6e436091` |
| Observed tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `59b9fae1cfab7ebf263f98068aa76cf42907d7c9` (live FF beyond kickoff cite `c4e757a14…`) |
| Court sources on tip | `crates/ggen-graph/tests/shui_public_prefix_court.rs` blob `3cda065f6564917504f241ac3626c111cd2dfb6a` (≡ prior ALIVE blob) |
| Exact command | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-tip7-shui cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` |
| Target dir | `/tmp/gate1-tip7-shui` (isolated; seeded from `/tmp/ggen-brand-exact-tip6` warm 4.1G; `librocksdb.a` present) |
| Budget | **1500s** (25min) hard `timeout` |
| This probe | **EXIT:137** (SIGKILL) @ `2026-11-22T18:55:15Z`→`19:00:37Z` (~5m22s) — cargo reached `Compiling oxrocksdb-sys` then killed; log 48 bytes. `dmesg` shows prior pod OOM kills of `cc1plus`/`rustc` under workload cgroup |
| Thrash | pre-start waited for `ggen-graph` quiet (`others=0`); during window peer agents relaunched concurrent oxrocksdb builds; loadavg≈3.7→5+ |
| Log / status | `/tmp/cursor/shui-exact-tip7.log` · `/tmp/cursor/shui-exact-tip7.status` → `SHUI_EXIT=137` |
| Prior tip6 | `cursor/v2711-shui-exact-tip6-bc54` — PARTIAL EXIT:**137** — [`SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md) |
| Prior tip-cargo ALIVE (lineage) | **held** — 4 passed; EXIT 0 @ `008a81d21` (ancestor of tip; blob `3cda065f`) — **not** reconfirmed this tip7 |
| Peer tip7 holders | concurrent peers on `/tmp/gate1-shui-alive` and `/tmp/gate1-tip7-shui` after this probe — **not** this agent's EXIT:0 |
| Crown absorb | **none** (EXIT≠0; push-first sidebranch only) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Tip7 push-first honesty: exclusive tip-cargo did **not** obtain EXIT 0 (OOM/SIGKILL mid-`oxrocksdb-sys`) → standing **PARTIAL_ALIVE**. Prior tip-cargo **ALIVE** @ `008a81d21` remains lineage evidence only. Do **not** invent tip-subject `ALIVE`, tip-wins crown absorb, or Foundry `ALIVE` from source presence / lineage alone.

**Honesty law (held):**
`PRESENT` ≠ `lineage_tip_cargo_ALIVE` ≠ `exact_tip_reconfirm_ALIVE` ≠ Foundry `ALIVE`.

---

## Why PARTIAL (not ALIVE)

| Check | Result |
|-------|--------|
| Tip carries strengthened `shui_public_prefix_court` (4 tests) | **YES** |
| Court blob ≡ prior ALIVE blob `3cda065f` | **YES** |
| Exact isolated tip-cargo EXIT 0 @ live tip within 25min | **NO** — EXIT **137** (SIGKILL during `Compiling oxrocksdb-sys`; no `test result:`) |
| Lineage tip-cargo ALIVE @ `008a81d21` | **held** (ancestor; not a tip-subject substitute) |

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ 59b9fae1c (⊇ c4e757a14 ⊇ 008a81d21); court blob 3cda065f
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/gate1-tip7-shui
timeout 1500s cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; 0 failed; EXIT 0 → promote this doc to tip-cargo ALIVE
```

Until that EXIT **0**, keep standing **PARTIAL_ALIVE** — lineage ALIVE @ `008a81d21` is **not** a substitute for tip reconfirm.

---

## Compact return

```text
tip_base=59b9fae1cfab7ebf263f98068aa76cf42907d7c9
branch=cursor/v2711-shui-exact-tip7-bc54
bc=bc-7a87dbfb
EXIT=137
standing=PARTIAL_ALIVE
diagnostic=SIGKILL_oxrocksdb_compile_OOM_under_peer_thrash
next=CARGO_TARGET_DIR=/tmp/gate1-tip7-shui cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1
lineage_alive=008a81d21
crown_absorb=none ManagePullRequest=not_used force_push=none Foundry_ALIVE=not_claimed
```
