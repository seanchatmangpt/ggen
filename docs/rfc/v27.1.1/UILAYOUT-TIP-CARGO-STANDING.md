# ui_layout_court tip-cargo — ALIVE (exact tip-cargo) receipt (v27.1.1)

```text
cargo_tip=a508e6bae9e61d7894ad98df8c507fdb1fb28eb2
court=ui_layout_court
package=ggen-graph
passed=11
failed=0
exit=0
standing=ALIVE (exact tip-cargo)
```

**Exact-tip reconfirm (`bc-70188ac3`):** `CARGO_BUILD_JOBS=1` exclusive `/tmp/ggen-uilayout-exact-tip` EXIT:0 @ `a508e6bae` for `ui_layout_court` (**11 passed; 0 failed**). Live crown at absorb may advance on docs-only tip-wins; `layout/ui_layout` + court sources ≡ through `a508e6bae`. Prior lineage tip-cargo EXIT:0 @ `59c090984` (and `ca52e9c16` / `eaf0dad40`) retained. Sibling `ui-layout-court.ts` remains **UNSUPPORTED**. Path-crown / Foundry crown **not** invented from this fence.

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-TIP-CARGO-STANDING` |
| Official evidence branch | `cursor/uilayout-exact-tip-alive-9f6c` (`bc-70188ac3`) tip-wins onto `cursor/ecosystem-spec-27-1-1-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` hop 3b / D5 in-repo fence |
| Cargo evidence tip | `a508e6bae9e61d7894ad98df8c507fdb1fb28eb2` — **exact tip-cargo ALIVE** (`jobs=1`; **11** passed) |
| Lineage evidence | `59c090984` — prior **ALIVE (lineage tip-cargo)**; prior `ca52e9c16` / `eaf0dad40` |
| Target dir | `/tmp/ggen-uilayout-exact-tip` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1`; `--locked`; isolated worktree `/tmp/ggen-uilayout-exact-tip-wt` |
| Exact-tip reconfirm | **ALIVE** — EXIT:0 @ `a508e6bae` (2026-11-22T15:37:02Z) |
| Thrash | **none** — no competing oxrocksdb cargo at start; exclusive `jobs=1` |
| OOR / sibling `.ts` | **untouched** / **UNSUPPORTED** |
| ManagePullRequest | **not used** (parent owns #849) |

**Law:** Official `ggen-graph` tip-cargo EXIT **0** for `ui_layout_court` on the cargo tip
promotes Gate-1 hop-3b digest-consume fence from **ALIVE (lineage tip-cargo)** to
**ALIVE (exact tip-cargo)**. Do **not** invent photolithography path crown or Foundry
zero-HITL `ALIVE` from this fence alone.

---

## Tip-cargo evidence

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `ui_layout_court` | `/tmp/ggen-uilayout-exact-tip` | **11 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |

Log: `/tmp/cursor/uilayout-exact-tip.log` · status: `/tmp/cursor/uilayout-exact-tip.status` · receipt: `/tmp/cursor/uilayout-exact-tip-receipt.md`.

---

## Replay (exact)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# cargo evidence @ a508e6bae; ui_layout sources ≡ through subsequent docs-only tip-wins
df -h /tmp
set -o pipefail
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test ui_layout_court --locked \
  --target-dir /tmp/ggen-uilayout-exact-tip \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: 11 passed; EXIT:0
```

---

## Compact return line

```text
cargo_tip=a508e6bae9e61d7894ad98df8c507fdb1fb28eb2
EXIT=0
standing=ALIVE (exact tip-cargo)
passed=11
landed=cursor/uilayout-exact-tip-alive-9f6c
bc=bc-70188ac3
next=sibling ui-layout-court.ts UNSUPPORTED; path crown / Foundry zero-HITL not ALIVE
```
