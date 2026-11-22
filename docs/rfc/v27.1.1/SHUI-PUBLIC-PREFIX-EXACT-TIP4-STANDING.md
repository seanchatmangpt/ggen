# shui_public_prefix_court — exact-tip4 standing (v27.1.1)

```text
ggen_tip_base=cc2a4ea662b12b35ac90246b3a2bfbd89db615e4
branch=cursor/v2711-shui-exact-tip4-bc54
court=shui_public_prefix_court
package=ggen-graph
prior_lineage_alive=008a81d21 (4 passed; EXIT 0; /tmp/ggen-shui-prefix-target2)
tip_cargo_exit=137
standing=PARTIAL_ALIVE (exact tip EXIT:0 not obtained; SIGKILL under pod thrash)
```

| Field | Value |
|-------|-------|
| Document ID | `SHUI-PUBLIC-PREFIX-EXACT-TIP4-STANDING` |
| Branch | `cursor/v2711-shui-exact-tip4-bc54` |
| User subject | Gate1 shui public-prefix court — tip4 (historically never reached `origin`; prior local `SHUI_EXIT=143`) |
| Tip base (final attempt) | `cc2a4ea662b12b35ac90246b3a2bfbd89db615e4` (`fix(ci): strip docs_deep EOF blank lines (fmt7)` on `origin/cursor/ecosystem-spec-27-1-1-bc54`) |
| Earlier tip samples this branch touched | `8c8b1e6e5` (user pin ~8c8b1e6e5+) → `2d7b47b2e` → retip to live `cc2a4ea66` |
| Observed at | start `2026-11-22T17:28:25Z` (first exclusive attempt) → final retry `2026-11-22T18:05:29Z` → end `2026-11-22T18:05:30Z` |
| Court sources on tip | blob `3cda065f6564917504f241ac3626c111cd2dfb6a` (≡ prior lineage ALIVE blob) |
| sha256 (tip working tree) | court `bd032856…5fe0`; `public_prefixes.rs` `bff5aa82…ee5c`; `shui.rs` `d7040f32…78d2` |
| Prior tip-cargo ALIVE | `cursor/v2711-shui-prefix-alive-bc54` pinning crown reconfirm **ALIVE** @ `008a81d21` (**4 passed; EXIT 0**; target `/tmp/ggen-shui-prefix-target2`; `/tmp/shui_public_prefix_court4.log`) |
| Sibling tip5 honesty (already on crown) | `SHUI-PUBLIC-PREFIX-EXACT-TIP5-STANDING` / `-ALIVE-STANDING` — **PARTIAL_ALIVE** EXIT **137** @ tip `167352d0d` (do not collapse tip4 into tip5) |
| Exact command (final) | `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip4-solo cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` |
| Prior exclusive attempt | same court → `CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip4` under `/tmp/ggen-tip-exclusive.lock` → flock wrapper **EXIT 137** (log `/tmp/cursor/shui-exact-tip4-court2.log`; reached `Compiling oxrocksdb-sys`) |
| EXIT | **137** (SIGKILL) |
| Diagnostic | Immediate SIGKILL on final retry (~1s wall; empty court3 log). Earlier exclusive attempt reached `oxrocksdb-sys` then SIGKILL. Concurrent tip-cargo thrash on shared pod (`gate1-exact-tip3/exclusive`, uilayout/brand/taffy/verify_witness/zcode). `dmesg` retains prior `oom-kill` of `cc1plus`/`rustc` in this workload cgroup. Status: `/tmp/cursor/shui-exact-tip4.status` → `EXIT:137`. **No** `test result:` line — do not invent `ALIVE`. |
| Crown absorb | **not performed** (sidebranch honesty only) |
| ManagePullRequest | **not used** |
| Force-push | **none** |
| Foundry ALIVE | **not claimed** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Lineage tip-cargo **ALIVE** @ `008a81d21` / `26256ca9d` is cited; court blob on tip `cc2a4ea66` matches that lineage. This tip4 pin does **not** mint tip EXIT:0 `ALIVE`, Foundry `ALIVE`, Truex admit-court standing, or photolithography path-crown `ALIVE`. Source presence ≠ tip-cargo `ALIVE`.

---

## Why PARTIAL (not ALIVE)

| Check | Result |
|-------|--------|
| Tip carries strengthened `shui_public_prefix_court` (4 tests) | **YES** |
| Court blob ≡ prior ALIVE blob `3cda065f` | **YES** |
| Exact isolated tip-cargo EXIT 0 @ `cc2a4ea66` | **NO** — EXIT **137** (SIGKILL; thrash / OOM pressure) |
| Historical tip4 remote | **ABSENT** before this land (coordination note from tip5: tip4 never reached origin; local EXIT 143) |

---

## Exact next verifier

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
TIP=$(git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54)
# Prefer quiet window: loadavg ≤3; free Mem available; no competing oxrocksdb cold builds
export CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1
export CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip4-solo
cargo test -p ggen-graph --test shui_public_prefix_court --locked \
  --config build.jobs=1 -- --test-threads=1
# expect: 4 passed; 0 failed; EXIT 0 → promote this doc to tip-cargo ALIVE @ $TIP
```

Replay identity: `git rev-parse HEAD` must be `cc2a4ea662b12b35ac90246b3a2bfbd89db615e4` (or a descendant that still carries court blob `3cda065f`).

---

## Compact return

```text
tip_base=cc2a4ea66
branch=cursor/v2711-shui-exact-tip4-bc54
user_pin=~8c8b1e6e5+
EXIT=137
standing=PARTIAL_ALIVE
lineage=ALIVE @ 008a81d21
diagnostic=SIGKILL_oxrocksdb_thrash
next=quiet CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip4-solo cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1
crown_absorb=none ManagePullRequest=not_used force_push=none foundry_alive=none
```
