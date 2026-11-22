# Truex+shui Gate-1 exact-tip reconfirm — `cursor/v2711-truex-exact-retip-bc54` (v27.1.1)

```text
ggen_tip=a3e5d310940c995be428a7760bfa8d73d9dc4456
sidebranch=cursor/v2711-truex-exact-retip-bc54
truex_sha=68d2d2331696f18949f4877b994320018b6365e5
truex_exit=0
truex_tests=6/6
truex_standing=ALIVE (admit-court reconfirm; not Foundry)
shui_command=CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip4 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1
shui_exit=137
shui_standing=PARTIAL_ALIVE (lineage tip-cargo ALIVE @ 008a81d21; sources ≡ tip; tip EXIT:0 not obtained)
```

| Field | Value |
|-------|-------|
| Document ID | `TRUEX-SHUI-EXACT-RETIP-STANDING` |
| Sidebranch | `cursor/v2711-truex-exact-retip-bc54` |
| Agent | `bc-8832a68a-988a-5a2a-9d29-1d95850379fb` |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `a3e5d310940c995be428a7760bfa8d73d9dc4456` |
| Worktree | `/tmp/truex-exact-retip-wt` |
| Observed at | `2026-11-22T18:10:40Z` |
| Crown absorb | **not performed** (settle agent may absorb) |
| ManagePullRequest | **not used** |
| Force-push | **none** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `UNSUPPORTED` | `REFUSED_*`.

---

## 1. Truex admit-court (product D3) — **ALIVE**

| Field | Value |
|-------|-------|
| Subject | `truex@68d2d2331696f18949f4877b994320018b6365e5` ([truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED** → `main` `b4672131`) |
| Mount | `/tmp/truex` |
| Command | `pnpm exec vitest run test/admit/admit-court.test.ts` |
| Result | **Test Files 1 passed; Tests 6 passed; EXIT 0** |
| Log | `/tmp/truex-admit-court-exact-retip.log` |
| Status | `/tmp/cursor/truex-admit-exact-retip.status` → `EXIT:0` |
| Outcomes | `shui:` **ADMITTED**; private `truex:` **REFUSED_PRIVATE**; unknown **REFUSED_UNKNOWN** |
| Prior tip-wins | [`TRUEX-ADMIT-EXACT-RETIP3-STANDING.md`](./TRUEX-ADMIT-EXACT-RETIP3-STANDING.md) (exact-tip4 absorb) — **held** by this reconfirm |

**Authority.** Sibling Chicago admit-court EXIT **0** (6/6) at pinned SHA. This does **not** mint Foundry `ALIVE`, path-crown `ALIVE`, or ggen fence tip-cargo `ALIVE`.

### Replay

```bash
git -C /tmp/truex rev-parse HEAD
# expect: 68d2d2331696f18949f4877b994320018b6365e5
cd /tmp/truex && pnpm exec vitest run test/admit/admit-court.test.ts
# expect: 6 passed; EXIT 0
```

---

## 2. ggen `shui_public_prefix_court` (fence) — **PARTIAL_ALIVE**

| Field | Value |
|-------|-------|
| Court | `crates/ggen-graph/tests/shui_public_prefix_court.rs` |
| Sources vs prior ALIVE | **IDENTICAL** to crown `008a81d21` (`shui.rs` / `public_prefixes.rs` / `mod.rs` / court) |
| Prior tip-cargo ALIVE | **4 passed; EXIT 0** @ `008a81d21` (`/tmp/ggen-shui-prefix-target2`; [`SHUI-PUBLIC-PREFIX-EXACT-TIP5-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP5-ALIVE-STANDING.md) lineage cite) |
| Exact tip attempts (this agent) | EXIT **101** (SIGTERM mid-`oxrocksdb-sys`, tip `49ebdb27c`, target `/tmp/ggen-truex-shui-private-79fb`); EXIT **137** (OOM SIGKILL, tip `eb28f0eb0`, target `/tmp/ggen-shui-exact-tip4`) |
| Sibling corroboration | tip3 exclusive EXIT **137**; tip5-alive EXIT **137**; tip-9dd1 EXIT **101** |
| Cgroup | `memory.max=15706882048`; `oom_kill` count **37** at evidence time (`/tmp/cursor/shui-exact-retip-oom-evidence.txt`) |
| Tip-subject fence `ALIVE` | **not claimed** — no tip EXIT **0** |

**Authority.** Lineage tip-cargo **ALIVE** @ `008a81d21` + source identity on tip is **not** tip EXIT:0 `ALIVE`. Do not invent tip-cargo `ALIVE` from blob identity alone. Fence ≠ Truex product crown.

### Exact next verifier (tip-cargo promote)

```bash
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/ggen-truex-shui-private-79fb
# Prefer quiet window: loadavg ≤5; no competing oxrocksdb cold builds; cgroup headroom
cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; 0 failed; EXIT 0 → tip-subject fence ALIVE
```

---

## 3. Aggregate Gate-1 hop-1 standing (this reconfirm)

| Leg | Standing | EXIT | Tip SHA cited |
|-----|----------|------|---------------|
| Truex admit-court | **ALIVE** | **0** (6/6) | `truex@68d2d233` on ggen tip `a3e5d3109` |
| ggen `shui:` fence | **PARTIAL_ALIVE** (lineage **ALIVE** @ `008a81d21`; tip EXIT:0 pending) | **137** (last tip attempt) | ggen tip `a3e5d3109` (sources ≡ `008a81d21`) |
| Foundry / path crown | **not claimed** | — | — |

---

## Compact return

```text
base_tip=a3e5d310940c995be428a7760bfa8d73d9dc4456
sidebranch=cursor/v2711-truex-exact-retip-bc54
truex_EXIT=0 truex_standing=ALIVE
shui_EXIT=137 shui_standing=PARTIAL_ALIVE
Foundry=NOT_CLAIMED ManagePullRequest=not_used force_push=none crown_absorb=none
```
