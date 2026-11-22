# ui_layout_court tip-cargo — ALIVE (exact tip-cargo) receipt (v27.1.1)

```text
cargo_tip=376d0d28350f275c6265bd91bd0f98e3eee94787
live_tip=dd61fefb29fa7ea6e1cf789f002ccd9879393988
court=ui_layout_court
package=ggen-graph
passed=11
failed=0
exit=0
standing=ALIVE (exact tip-cargo)
```

**Exact-tip reconfirm (`cursor/v2711-uilayout-exact-retip-bc54`):** package
`cargo test -p ggen-graph --test ui_layout_court` → **11 passed; 0 failed; EXIT:0**
@ cargo tip `376d0d283` (worktree `/tmp/ggen-uilayout-exact-retip-wt`; target
`/tmp/ggen-uilayout-exact-retip-hl`). Live crown may advance on docs-only tip-wins;
`layout/ui_layout` + `tests/ui_layout_court.rs` byte-identical through prior ALIVE
`a508e6bae` and through live tip at docs land (`dd61fefb2`). Prior exact tip-cargo
EXIT:0 @ `a508e6bae` (`bc-70188ac3`) and lineage `59c090984` / `ca52e9c16` /
`eaf0dad40` retained. Sibling `ui-layout-court.ts` remains **UNSUPPORTED**.
Path-crown / Foundry crown **not** invented from this fence. No crown absorb.

| Field | Value |
|-------|-------|
| Document ID | `UILAYOUT-TIP-CARGO-STANDING` |
| Official evidence branch | `cursor/v2711-uilayout-exact-retip-bc54` (exact-tip Gate1 reconfirm; no crown absorb) |
| Prior land | `cursor/uilayout-exact-tip-alive-9f6c` (`bc-70188ac3`) @ `a508e6bae` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` hop 3b / D5 in-repo fence |
| Cargo evidence tip | `376d0d28350f275c6265bd91bd0f98e3eee94787` — **exact tip-cargo ALIVE** (**11** passed; EXIT:0) |
| Live tip at docs land | `dd61fefb29fa7ea6e1cf789f002ccd9879393988` — sources ≡ cargo tip (layout/court unchanged) |
| Lineage evidence | `a508e6bae` — prior **ALIVE (exact tip-cargo)**; `59c090984` / `ca52e9c16` / `eaf0dad40` |
| Target dir | `/tmp/ggen-uilayout-exact-retip-hl` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1`; `--locked`; worktree `/tmp/ggen-uilayout-exact-retip-wt` |
| Exact-tip reconfirm | **ALIVE** — EXIT:0 @ `376d0d283` (2026-11-22T18:45:02Z start; Finished ~15m) |
| Thrash | tip3 shared oxrocks warm target hardlink; waited on cargo build-dir lock; prior attempts EXIT **137**/**143** under exclusive guardian / OOM — **not** standing |
| OOR / sibling `.ts` | **untouched** / **UNSUPPORTED** |
| ManagePullRequest | **not used** (no #849; no crown absorb) |

**Law:** Official `ggen-graph` tip-cargo EXIT **0** for `ui_layout_court` on the cargo tip
reconfirms Gate-1 hop-3b digest-consume fence as **ALIVE (exact tip-cargo)**. Do **not**
invent photolithography path crown or Foundry zero-HITL `ALIVE` from this fence alone.

---

## Tip-cargo evidence

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `ui_layout_court` | `/tmp/ggen-uilayout-exact-retip-hl` | **11 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |

Log: `/tmp/cursor/uilayout-exact-retip-hl.log` · status: `/tmp/cursor/uilayout-exact-retip-hl.status` · receipt: `/tmp/cursor/uilayout-exact-retip-receipt.md`.

---

## Replay (exact)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# cargo evidence @ 376d0d283; ui_layout sources ≡ through subsequent docs-only tip-wins
df -h /tmp
set -o pipefail
CXX=g++ CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test ui_layout_court --locked \
  --target-dir /tmp/ggen-uilayout-exact-retip-hl \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: 11 passed; EXIT:0
```

---

## Compact return line

```text
base_tip=dd61fefb29fa7ea6e1cf789f002ccd9879393988
cargo_tip=376d0d28350f275c6265bd91bd0f98e3eee94787
sidebranch=cursor/v2711-uilayout-exact-retip-bc54
EXIT=0
standing=ALIVE (exact tip-cargo)
passed=11
next=sibling ui-layout-court.ts UNSUPPORTED; path crown / Foundry zero-HITL not ALIVE; no crown absorb
```
