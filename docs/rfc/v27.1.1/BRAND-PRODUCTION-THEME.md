# Brand.toml production theme — next verifier (Gate-1 → Gate 2+)

Companion to `ECOSYSTEM-SPEC-27.1.1` §8 and Gate-1 gap matrix row
`Brand .toml rhythm schema + OEM stubs`.

## Standing (this tip)

Tip pin: absorb official `cursor/v2711-brand-toml-tip-dee5` @ `72b762f20` (bc-7f0c72b7) —
completeness+rhythm **ALIVE (lineage tip-cargo)** @ `c91b22561` (jobs=1 `/tmp/ggen-brand-toml-tip`);
fluid+hotswap tip-cargo **ALIVE** @ `19c08a3b0`; production **UNSUPPORTED**. Exact-tip reconfirm
SCHEDULED (exclusive jobs=1 when cargo quiet). Receipt: [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md).

**Hot-swap / production honesty retip** (`cursor/v2711-brand-hotswap-retip-bc54` /
`bc-77f1df1f-0499-5eae-9e4f-68342a2f5414`) onto live tip
`e6d1b839dabee76712cee9c57953626b77212826`: hot-swap tip-subject court
**PARTIAL_ALIVE** (docs-only; no cargo — Gate1 tip6 owns thrash) with lineage
**ALIVE** (**6** @ `19c08a3b0` / absorb `cde02d1f2`; blob ≡ tip). Production
theme / production hot-swap remains **UNSUPPORTED** (`REFUSED_PRODUCTION`) —
do **not** invent `ALIVE`. Full pin:
[`BRAND-TOML-HOTSWAP-PROD-RETIP-STANDING.md`](./BRAND-TOML-HOTSWAP-PROD-RETIP-STANDING.md).

| Surface | Standing | Evidence |
|---------|----------|----------|
| Rhythm schema + OEM stubs | **ALIVE** (lineage tip-cargo) | `brand_toml_rhythm_court` **12 passed; exit 0** @ `c91b22561` ([`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md)) |
| Completeness Incomplete refusals | **ALIVE** (lineage tip-cargo) | `brand_toml_completeness_court` **5 passed; exit 0** @ `c91b22561` (same tip-cargo run) |
| Fluid clamp / cqi slopes | **ALIVE** (tip-cargo **6 passed; exit 0** @ crown `19c08a3b0`) | `brand_fluid_clamp_court` |
| OEM hot-swap fence | lineage **ALIVE** (**6** @ `19c08a3b0`); tip-subject **PARTIAL_ALIVE** | `seed_hot_swap_slot` / `hot_swap_brand` + `brand_toml_hotswap_court` — tip reconfirm deferred ([`BRAND-TOML-HOTSWAP-PROD-RETIP-STANDING.md`](./BRAND-TOML-HOTSWAP-PROD-RETIP-STANDING.md)) |
| Production runtime theme | **UNSUPPORTED** | `promote_production_theme` → `ProductionThemeRefusal::ProductionUnsupported` (code `REFUSED_PRODUCTION`) |

Fluid clamp, hot-swap, and rhythm lineage tip-cargo are Gate-1 CONSTRUCT fences with
lineage standing **ALIVE**. They do **not** authorize production theme DO, CASTLE
publish, or full DTCG color/type inventory `ALIVE`. Tip-subject hot-swap remains
**PARTIAL_ALIVE** until exclusive `brand_toml_hotswap_court` EXIT **0** on the live tip.

## In-repo fail-closed fence

| Path | Role |
|------|------|
| `crates/ggen-graph/src/layout/brand.rs` | `production_theme_standing`, `promote_production_theme`, `ProductionThemeRefusal::ProductionUnsupported`, `REFUSED_PRODUCTION` |
| `layout::brand::production_theme_tests` | Lib-only Chicago refuse court (no oxrocksdb / integration harness) |

Replay (lib-only; preferred under cgroup contention):

```bash
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-brand-prod \
  cargo test -p ggen-graph --lib layout::brand::production_theme_tests
```

Expected: exit **0**; every promote path yields `ProductionThemeRefusal::ProductionUnsupported`
with code `REFUSED_PRODUCTION`. Never `Ok` / never `ALIVE`. (Not `BrandThemeRefusal` —
that type does not exist on this tip.)

Companion lineage fences (not production promote):

```bash
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-brand-toml-tip \
  cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked -- --test-threads=1
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy \
  cargo test -p ggen-graph --test brand_toml_hotswap_court --test brand_fluid_clamp_court --locked
```

## Exact next verifier to promote production theme

Production theme remains **UNSUPPORTED** until **all** of the following exit
**0** on an admitted subject (do not invent tokens in ggen):

1. **Admit full DTCG inventory (Gate 2+)** — OOR **G2-13**
   ([`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md)):
   inventory court loads admitted `brands/fourth-watch.brand.toml` +
   `brands/cindycat.brand.toml` full DTCG color/type/CASTLE sets → exit **0**.
   Owner: brand plane + CASTLE. ggen rhythm court is a non-substitute.

2. **Bind inventory into CASTLE mint (Gate 2+)** — OOR **G2-12** /
   [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md): sibling
   `publish_redaction_court` + brand digest bind from ggen layout/hot-swap
   receipt into mint → exit **0**.

3. **Production theme apply court (not yet authored)** — after (1)+(2), author
   a Chicago court that:
   - seeds / hot-swaps an admitted OEM slot under rhythm law;
   - applies the Gate-2 inventory to a real runtime theme surface (UI plane or
     CASTLE-owned theme apply — not ambient ggen DO);
   - asserts `promote_production_theme` (or its successor production API) returns
     typed admit with standing **not** exceeding what the apply receipt proves;
   - refuses missing inventory / missing apply path with `REFUSED_PRODUCTION`
     (or narrower typed code).

Until step 3 exists and exits **0**, keep calling:

```bash
cargo test -p ggen-graph --lib layout::brand::production_theme_tests
```

as the exact in-repo verifier that production theme stays **UNSUPPORTED** /
`REFUSED_PRODUCTION`.

## Standing discipline

- Hot-swap / rhythm lineage tip-cargo green → **ALIVE** (lineage) only — never production `ALIVE`.
- `promote_production_theme` → always `REFUSED_PRODUCTION` on Gate-1 path.
- Never promote inspection of this markdown file to `ALIVE`.
- Never claim production theme `ALIVE` from rhythm / fluid / hot-swap courts alone.