# brand.toml hot-swap tip-cargo — COMPILING / PARTIAL_ALIVE (exact tip) (v27.1.1)

```text
tip=cc2a4ea662b12b35ac90246b3a2bfbd89db615e4
court=brand_toml_hotswap_court
package=ggen-graph
command=CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_hotswap_court --locked --target-dir /tmp/ggen-brand-hotswap-exact-tip --config build.jobs=1 -- --test-threads=1
EXIT=137
standing=COMPILING / PARTIAL_ALIVE (exact tip not sealed)
fence_ceiling=PARTIAL_ALIVE
production=UNSUPPORTED
lineage_tip_cargo=ALIVE @ 19c08a3b0 / absorb cde02d1f2 (6 passed; exit 0)
```

**Exact-tip attempt (`cursor/v2711-brand-exact-retip-bc54`):** live crown tip
`cc2a4ea66` (retipped through `acd3b8c89` → `8c8b1e6e5` → `167352d0d` →
`2d7b47b2e` → `cc2a4ea66` while cargo contended). Exclusive `CARGO_BUILD_JOBS=1`
runs did **not** reach tip EXIT **0**. Recorded exits: **143** (SIGTERM),
**101** (oxrocksdb-sys SIGTERM), **137** (cgroup OOM SIGKILL; last). Do **not**
invent tip-subject `ALIVE`. Fence API ceiling remains `HOT_SWAP_STANDING_CEILING`
= **PARTIAL_ALIVE**. Production theme remains **UNSUPPORTED** /
`REFUSED_PRODUCTION`.

| Field | Value |
|-------|-------|
| Document ID | `BRAND-HOTSWAP-TIP-CARGO-STANDING` |
| Official evidence branch | `cursor/v2711-brand-exact-retip-bc54` tip-wins onto `cursor/ecosystem-spec-27-1-1-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §8 brand.toml hot-swap |
| Live tip pin | `cc2a4ea662b12b35ac90246b3a2bfbd89db615e4` |
| Exact-tip cargo | **COMPILING** — EXIT **137** (OOM); prior **143**/**101** under thrash |
| Lineage evidence | `19c08a3b0` / absorb `cde02d1f2` — **ALIVE (lineage tip-cargo)** **6 passed; exit 0** (`/tmp/gate1-taffy`) |
| Brand sources vs lineage | **UNCHANGED** (`git diff --stat 19c08a3b0..cc2a4ea66` on `layout/brand.rs` + `brand_toml_hotswap_court.rs` + OEM fixtures → empty) |
| Target dirs attempted | `/tmp/ggen-brand-hotswap-exact-tip`, `/tmp/ggen-uilayout-exact-retip-warm`, `/tmp/gate1-exact-tip2/shui-target` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1`; `--locked`; worktree `/tmp/ggen-brand-exact-retip-wt` |
| Cgroup | workload `memory.max`≈14.6 Gi; `oom_kill`≥37 during window |
| Fence ceiling | **PARTIAL_ALIVE** (`seed_hot_swap_slot` / `hot_swap_brand` receipts) |
| Production theme | **UNSUPPORTED** / `REFUSED_PRODUCTION` — [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md) |
| ManagePullRequest | **not used** (parent owns #849); **no crown absorb** |

**Law:** Exact-tip `ALIVE` requires recorded EXIT **0** for `brand_toml_hotswap_court`
on the live tip under exclusive cargo. Lineage tip-cargo EXIT **0** does **not**
promote the live tip to exact-tip `ALIVE` while thrash/OOM blocks the verifier.
Court assertions themselves refuse the string `ALIVE` on hot-swap receipts
(ceiling **PARTIAL_ALIVE**). Production theme is never `ALIVE` from this fence.

---

## Tip-cargo evidence

| Court | Tip | Result | Exit | Standing |
|-------|-----|--------|------|----------|
| `brand_toml_hotswap_court` (exact tip) | `cc2a4ea66` | no completed run | **137** | **COMPILING** / **PARTIAL_ALIVE** |
| `brand_toml_hotswap_court` (lineage) | `19c08a3b0` / absorb `cde02d1f2` | **6 passed; 0 failed** | **0** | **ALIVE (lineage tip-cargo)** |

Logs: `/tmp/cursor/brand-hotswap-exact-tip{,2,3,4,5,6}*.log` · receipt:
`/tmp/cursor/brand-hotswap-exact-tip-receipt.md`.

---

## Replay (exact)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect quiet cgroup (no concurrent oxrocksdb cargo); jobs=1
df -h /tmp
set -o pipefail
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test brand_toml_hotswap_court --locked \
  --target-dir /tmp/ggen-brand-hotswap-exact-tip \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: 6 passed; EXIT:0 → then promote this receipt to ALIVE (exact tip-cargo)
# until then: keep COMPILING / PARTIAL_ALIVE; fence ceiling PARTIAL_ALIVE; production UNSUPPORTED
```

---

## Compact return line

```text
base_tip=cc2a4ea662b12b35ac90246b3a2bfbd89db615e4
EXIT=137
standing=COMPILING / PARTIAL_ALIVE
fence_ceiling=PARTIAL_ALIVE
production=UNSUPPORTED
lineage=ALIVE @ 19c08a3b0 (6 passed; exit 0)
landed=cursor/v2711-brand-exact-retip-bc54
next=exclusive quiet tip-cargo EXIT:0; production theme remains UNSUPPORTED
```
