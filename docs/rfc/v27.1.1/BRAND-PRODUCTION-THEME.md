# Brand.toml production theme — next verifier (Gate-1 → Gate 2+)

Companion to `ECOSYSTEM-SPEC-27.1.1` §8 and Gate-1 gap matrix row
`Brand .toml rhythm schema + OEM stubs`.

## Standing (this tip)

Tip pin: crown `19c08a3b0` (`cursor/v2711-taffy-brand-absorb-a77d`; fluid+hotswap tip-cargo
**ALIVE**; production **UNSUPPORTED**; rhythm lineage retained @ `26256ca9d`).

| Surface | Standing | Evidence |
|---------|----------|----------|
| Rhythm schema + OEM stubs | **ALIVE** (lineage) | `brand_toml_rhythm_court` **12 passed; exit 0** @ `26256ca9d` (Official brand_toml owns cold reconfirm) |
| Fluid clamp / cqi slopes | **ALIVE** (tip-cargo **6 passed; exit 0** @ crown `19c08a3b0`) | `brand_fluid_clamp_court` |
| OEM hot-swap fence | **ALIVE** (tip-cargo **6 passed; exit 0** @ crown `19c08a3b0`) | `seed_hot_swap_slot` / `hot_swap_brand` + `brand_toml_hotswap_court` |
| Production runtime theme | **UNSUPPORTED** | `promote_production_theme` → `ProductionThemeRefusal::ProductionUnsupported` (code `REFUSED_PRODUCTION`) |

Fluid clamp, hot-swap, and rhythm lineage tip-cargo are Gate-1 CONSTRUCT fences with
standing **ALIVE**. They do **not** authorize production theme DO, CASTLE
publish, or full DTCG color/type inventory `ALIVE`.

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
CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-next-brand \
  cargo test -p ggen-graph --test brand_toml_rhythm_court --locked
cargo test -p ggen-graph --test brand_toml_hotswap_court
cargo test -p ggen-graph --test brand_fluid_clamp_court
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