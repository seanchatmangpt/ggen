# brand.toml hot-swap + production theme — push-first honesty retip (v27.1.1)

```text
tip_base=e6d1b839dabee76712cee9c57953626b77212826
branch=cursor/v2711-brand-hotswap-retip-bc54
bc=bc-77f1df1f-0499-5eae-9e4f-68342a2f5414
court=brand_toml_hotswap_court
court_tip_cargo=PARTIAL_ALIVE (no cargo this land; Gate1 tip6 owns thrash)
lineage_alive=19c08a3b0 / cde02d1f2 (tip-cargo 6 passed; blob ≡ tip)
production_theme=UNSUPPORTED (REFUSED_PRODUCTION)
Foundry_ALIVE=not_claimed
crown_absorb=none
ManagePullRequest=not_used
cargo_this_land=none
```

| Field | Value |
|-------|-------|
| Document ID | `BRAND-TOML-HOTSWAP-PROD-RETIP-STANDING` |
| Branch | `cursor/v2711-brand-hotswap-retip-bc54` |
| Agent | `bc-77f1df1f-0499-5eae-9e4f-68342a2f5414` |
| Observed tip base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `e6d1b839dabee76712cee9c57953626b77212826` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1` §8 brand rhythm / hot-swap / production theme |
| Companions | [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md); [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md) |
| Hot-swap court blob on tip | `22e417ab307428fdcaf228059923fec644ce57c3` (≡ lineage ALIVE @ `19c08a3b0`) |
| Court tip-cargo (this land) | **PARTIAL_ALIVE** — docs-only; **no cargo** (Gate1 tip6 owns thrash); do **not** invent tip-subject `ALIVE` |
| Lineage tip-cargo ALIVE | **held** — `brand_toml_hotswap_court` **6 passed; EXIT 0** absorb @ `cde02d1f2` / crown `19c08a3b0` |
| Production runtime theme | **UNSUPPORTED** — `promote_production_theme` → `ProductionThemeRefusal::ProductionUnsupported` (`REFUSED_PRODUCTION`) per tip [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md) |
| Production hot-swap DO | **UNSUPPORTED** (never claimed `ALIVE` / never claimed `PARTIAL_ALIVE` as production admit) |
| Prior sidebranch | `cursor/v2711-brand-hotswap-bc54` (feat land `f6cb18f86`) — absorbed lineage; this land is push-first honesty retip only |
| Crown absorb | **none** |
| ManagePullRequest | **not used** |
| Foundry crown | **not claimed** (no Foundry `ALIVE`) |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority.** Tip docs on `e6d1b839d` already separate (a) hot-swap CONSTRUCT fence lineage tip-cargo **ALIVE** @ `19c08a3b0` from (b) production theme **UNSUPPORTED** / `REFUSED_PRODUCTION`. This retip re-pins that honesty onto the live tip without running cargo and without inventing tip-subject court `ALIVE` or production `ALIVE`.

---

## Standings (court vs production)

| Surface | Standing | Cite |
|---------|----------|------|
| `brand_toml_hotswap_court` tip-subject reconfirm | **PARTIAL_ALIVE** | no EXIT:0 this land; thrash owned by Gate1 tip6 |
| `brand_toml_hotswap_court` lineage tip-cargo | **ALIVE** | **6 passed; EXIT 0** @ `19c08a3b0` / absorb `cde02d1f2`; blob ≡ tip `e6d1b839d` |
| Production theme / production hot-swap | **UNSUPPORTED** | `promote_production_theme` → `REFUSED_PRODUCTION` ([`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md)) |

---

## Exact next verifier — court EXIT:0

Falsifier for promoting hot-swap tip-subject to **ALIVE (exact tip-cargo)** on tip `e6d1b839d` (or its live FF successor with hot-swap sources ≡ `19c08a3b0`):

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git rev-parse origin/cursor/ecosystem-spec-27-1-1-bc54
# expect tip ⊇ e6d1b839d; brand_toml_hotswap_court blob ≡ 22e417ab3
df -h /tmp
# prefer thrash=0 concurrent oxrocksdb cargo (Gate1 tip6 quiet)
set -o pipefail
CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 \
  cargo test -p ggen-graph --test brand_toml_hotswap_court --locked \
  --target-dir /tmp/ggen-brand-hotswap-retip \
  --config build.jobs=1 \
  -- --test-threads=1
# expect: 6 passed; EXIT:0 → tip-subject hot-swap ALIVE
```

Until that EXIT **0**, keep court tip-subject **PARTIAL_ALIVE** — lineage ALIVE @ `19c08a3b0` is **not** a substitute for tip reconfirm.

---

## Exact next verifier — production path (held UNSUPPORTED)

In-repo refuse (production stays **UNSUPPORTED**):

```bash
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-brand-prod \
  cargo test -p ggen-graph --lib layout::brand::production_theme_tests
# expect: EXIT:0; every path → ProductionThemeRefusal::ProductionUnsupported / REFUSED_PRODUCTION
# Never Ok / never ALIVE
```

Promote path (Gate 2+; documented in [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md)):

1. OOR **G2-13** full DTCG inventory court → EXIT **0**
2. OOR **G2-12** CASTLE mint bind → EXIT **0**
3. Production theme apply court (not yet authored) → EXIT **0**

Do **not** invent production theme `ALIVE` from hot-swap / fluid / rhythm courts alone.

---

## Compact return

```text
tip_base=e6d1b839dabee76712cee9c57953626b77212826
remote_sha=<fill after git push + ls-remote>
branch=cursor/v2711-brand-hotswap-retip-bc54
court=PARTIAL_ALIVE
lineage_alive=19c08a3b0/cde02d1f2
production=UNSUPPORTED
absorbed=<fill after tip-wins check>
next_court=exclusive brand_toml_hotswap_court jobs=1 → expect 6/EXIT:0
next_prod=lib layout::brand::production_theme_tests → REFUSED_PRODUCTION; promote=G2-13→G2-12→apply court
crown_absorb=none ManagePullRequest=not_used cargo=none Foundry_ALIVE=not_claimed
```
