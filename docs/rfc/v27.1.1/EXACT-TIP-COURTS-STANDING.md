# Exact-tip courts honesty — shui + zcode/bound (v27.1.1)

```text
crown_tip=e6d00965a0b0b96df9e284d857de1371c89a3dd2
expect_floor=01eccfffb56c1c59be67a220eadb5bd98783c806
branch=cursor/v2711-exact-tip-courts-bc54
shui=STALLED EXIT137 (also 101/143)
zcode_sjira=STALLED EXIT137
remediation_bound=STALLED EXIT137
standing=STALLED (no invented ALIVE)
pass_counts=none (no test result lines)
ManagePullRequest=not_used
crown_push=none
```

| Field | Value |
|-------|-------|
| Document ID | `EXACT-TIP-COURTS-STANDING` |
| Sidebranch | `cursor/v2711-exact-tip-courts-bc54` |
| Observed crown tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `e6d00965a0b0b96df9e284d857de1371c89a3dd2` (⊇ floor `01eccfffb`; tip advanced under thrash: `01eccfffb` → `2d7b47b2e` → `c4e757a14` → `e6d00965a`) |
| Floor check | `01eccfffb` ⊆ tip (**ancestor_exit=0**) |
| Courts requested | (1) `shui_public_prefix_court`; (2) `zcode_powl_ocel_court`; (3) `remediation_bound` (documented equiv: `cargo test -p ggen-graph remediation_bound --lib`) |
| Exact-tip ALIVE | **not claimed** — every captured EXIT ≠ 0 |
| Lineage held (non-substitute) | `shui:` tip-cargo **ALIVE** @ `008a81d21` (**4**); zcode refuse-path **ALIVE** @ `cd7c2433b` / reconfirm `df9597c18` (**6+4**) |
| Crown push | **none** (sidebranch only) |
| ManagePullRequest | **not used** |

Standing vocabulary (`AGENTS.md` §2): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` |
`BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`. This land adds
`STALLED` for scheduled exact-tip cargo whose agents recorded EXIT≠0 / thrash
without a `test result:` line.

**Authority.** Exact-tip reconfirm of Gate-1 hop-1 fence + hop-5 bounded-refusal
courts. Do **not** invent tip-subject `ALIVE`, Foundry `--live` `ALIVE`, or crown
completion from lineage ALIVE / source presence / empty logs.

---

## Court results (this land)

| Court | Command | EXIT | Pass count | Standing |
|-------|---------|------|------------|----------|
| `shui_public_prefix_court` | `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-shui-prefix-target2 cargo test -p ggen-graph --test shui_public_prefix_court --locked` (+ exclusive/jobs=1 retries) | **101** (ENOSPC) → **143** (SIGTERM mid-compile) → **137** (OOM/SIGKILL) ×N | **none** (no `test result:`) | **STALLED** |
| `zcode_powl_ocel_court` | `cargo test -p ggen-sjira --test zcode_powl_ocel_court --locked` | **137** (OOM/SIGKILL @ `2026-11-22T18:22:53Z`) | **none** | **STALLED** |
| `remediation_bound` | `cargo test -p ggen-graph remediation_bound --lib --locked` (documented equiv of `--lib remediation_bound`) | **137** (OOM/SIGKILL @ `2026-11-22T18:22:53Z`) | **none** | **STALLED** |

Logs / status: `/tmp/cursor/exact-tip-courts-shui.log` · `…-shui.status` ·
`…-zcode-sjira.log` · `…-bound.log` · ledger
`/tmp/cursor/exact-tip-courts-stalled.diag` · summary
`/tmp/cursor/exact-tip-courts-summary.txt`.

---

## Diagnostics (exact)

1. **ENOSPC (EXIT 101):** cold compile into `/tmp/ggen-shui-prefix-target2` —
   `rustc-LLVM ERROR: IO failure on output stream: No space left on device`
   while compiling `purrdf-shapes` (disk was ~100% / 383M free before cleanup).
2. **SIGTERM (EXIT 143):** retry truncated mid-`Compiling` (no `test result:`).
3. **OOM/SIGKILL (EXIT 137):** exclusive flock + isolated worktree + quiet-window
   retries killed under concurrent multi-agent oxrocksdb/`cargo test` thrash
   (`/tmp/gate1-exact-tip3/*`, `/tmp/only-fafc690f-tip3-courts/*`; loadavg ≈5–7;
   quiet window ≥31 min **not** obtained). Final trio all EXIT **137** with empty
   or pre-result logs.
4. **No invented ALIVE:** lineage `shui` **4**/EXIT0 @ `008a81d21` and zcode
   refuse **6+4**/EXIT0 @ `cd7c2433b`/`df9597c18` remain **held** as lineage only.

Companion prior pins (non-substitute):
[`SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md)
(**PARTIAL_ALIVE**/EXIT137),
[`ZCODE-POWL-REFUSAL-RETIP6-STANDING.md`](./ZCODE-POWL-REFUSAL-RETIP6-STANDING.md),
[`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md).

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
TIP=$(git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54)
# expect tip ⊇ e6d00965a ⊇ 01eccfffb
df -h /tmp
# require thrash_n→0 concurrent cargo/oxrocksdb; exclusive jobs=1
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
CARGO_TARGET_DIR=/tmp/ggen-shui-prefix-target2 \
  cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; EXIT 0

cargo test -p ggen-sjira --test zcode_powl_ocel_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 6 passed; EXIT 0

cargo test -p ggen-graph remediation_bound --lib --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; EXIT 0
```

Until all three EXIT **0** with recorded pass counts, keep this land **STALLED** —
do **not** promote tip-subject `ALIVE`.

---

## Compact return

```text
tip=e6d00965a0b0b96df9e284d857de1371c89a3dd2 (≥01eccfffb)
side=cursor/v2711-exact-tip-courts-bc54
shui=STALLED EXIT137 (prior 101/143)
zcode=STALLED EXIT137
bound=STALLED EXIT137
pass_counts=none
standing=STALLED no_invented_ALIVE
lineage_held=shui@008a81d21/4 + zcode@cd7c2433b/6+4
crown_push=none ManagePullRequest=not_used
```
