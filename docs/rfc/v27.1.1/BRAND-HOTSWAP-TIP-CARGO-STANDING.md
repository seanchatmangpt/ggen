# brand.toml hot-swap tip-cargo — ALIVE (exact tip-cargo) receipt (v27.1.1)

```text
cargo_tip=c4e757a14c91c6512438a8a94bbde304b0a6e921
court=brand_toml_hotswap_court
package=ggen-graph
passed=6
failed=0
EXIT=0
standing=ALIVE (exact tip-cargo)
fence_ceiling=PARTIAL_ALIVE
production=UNSUPPORTED
lineage_tip_cargo=ALIVE @ 19c08a3b0 / absorb cde02d1f2 (6 passed; exit 0)
```

**Exact-tip reconfirm (`cursor/v2711-brand-exact-retip4-bc54` / `bc-479d54bf`):**
`CARGO_BUILD_JOBS=1` on cargo tip `c4e757a14` (admitted base `~8c8b1e6e5+`;
land tip may advance docs-only) for `brand_toml_hotswap_court` —
**6 passed; 0 failed; EXIT:0** @ 2026-11-22T18:44:13Z
(`/tmp/only-fafc690f-tip3-courts`). Prior exact-retip attempts recorded
EXIT **137**/**143**/**101** under guardian/OOM thrash and correctly stayed
**COMPILING** / **PARTIAL_ALIVE** — see
[`BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING.md`](./BRAND-TOML-HOTSWAP-EXACT-RETIP4-STANDING.md).
Fence API ceiling remains `HOT_SWAP_STANDING_CEILING` = **PARTIAL_ALIVE**.
Production theme remains **UNSUPPORTED** / `REFUSED_PRODUCTION`. Foundry /
zero-HITL crown **not** invented from this fence. Crown absorb **not** performed
on this branch.

| Field | Value |
|-------|-------|
| Document ID | `BRAND-HOTSWAP-TIP-CARGO-STANDING` |
| Official evidence branch | `cursor/v2711-brand-exact-retip4-bc54` against `cursor/ecosystem-spec-27-1-1-bc54` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §8 brand.toml hot-swap |
| Cargo evidence tip | `c4e757a14c91c6512438a8a94bbde304b0a6e921` — **exact tip-cargo ALIVE** |
| Admitted base | `8c8b1e6e5ed7314a4e37f3fdc6078e4383aecd26` |
| Lineage evidence | `19c08a3b0` / absorb `cde02d1f2` — prior **ALIVE (lineage tip-cargo)** **6** |
| Court blob | `22e417ab307428fdcaf228059923fec644ce57c3` (≡ lineage) |
| Target dir | `/tmp/only-fafc690f-tip3-courts` |
| Constraint | `CARGO_BUILD_JOBS=1`; `build.jobs=1`; `--test-threads=1`; `--locked`; `SCCACHE_DISABLE=1` |
| Exact-tip reconfirm | **ALIVE** — EXIT:0 @ `c4e757a14` |
| Fence ceiling | **PARTIAL_ALIVE** |
| Production theme | **UNSUPPORTED** / `REFUSED_PRODUCTION` — [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md) |
| Foundry / zero-HITL | **not ALIVE** |
| ManagePullRequest | **not used** (parent owns #849) |
| Crown absorb | **none** on this branch |

**Law:** Official `ggen-graph` tip-cargo EXIT **0** for `brand_toml_hotswap_court`
on the cargo tip seals Gate-1 OEM hot-swap tip-cargo as
**ALIVE (exact tip-cargo)**. Court assertions themselves refuse the string
`ALIVE` on hot-swap receipts (ceiling **PARTIAL_ALIVE**). Production theme is
never `ALIVE` from this fence. Foundry zero-HITL is never `ALIVE` from this fence.

---

## Tip-cargo evidence

| Court | Target | Result | Exit | Standing |
|-------|--------|--------|------|----------|
| `brand_toml_hotswap_court` | `/tmp/only-fafc690f-tip3-courts` | **6 passed; 0 failed** | **0** | **ALIVE (exact tip-cargo)** |

Tests exercised (all **ok**):

- `fourth_watch_to_cindycat_hot_swap_is_partial_alive`
- `cindycat_back_to_fourth_watch_round_trips`
- `rhythm_refusal_blocks_hot_swap`
- `same_brand_hot_swap_is_refused`
- `seed_of_unknown_oem_is_refused`
- `unknown_oem_brand_id_is_refused`

Log: `/tmp/cursor/brand-exact-retip4.log` · status: `/tmp/cursor/brand-exact-retip4.status` ·
receipt: `/tmp/cursor/brand-exact-retip4-receipt.md`.

---

## Replay (exact)

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
# cargo evidence @ c4e757a14; brand hot-swap sources ≡ through subsequent docs-only tip-wins
set -o pipefail
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test brand_toml_hotswap_court --locked \
  --target-dir /tmp/ggen-brand-hotswap-exact-retip4 \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: 6 passed; EXIT:0; standing string never ALIVE (ceiling PARTIAL_ALIVE)
```

---

## Compact return line

```text
cargo_tip=c4e757a14c91c6512438a8a94bbde304b0a6e921
EXIT=0
standing=ALIVE (exact tip-cargo)
passed=6
fence_ceiling=PARTIAL_ALIVE
production=UNSUPPORTED
foundry=not ALIVE
landed=cursor/v2711-brand-exact-retip4-bc54
bc=bc-479d54bf
next=production theme UNSUPPORTED; no crown absorb
```
