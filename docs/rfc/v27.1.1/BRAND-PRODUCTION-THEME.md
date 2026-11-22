# Brand.toml production theme — next verifier (Gate-1 → Gate 2+)

Companion to `ECOSYSTEM-SPEC-27.1.1` §8 and Gate-1 gap matrix row
`Brand .toml rhythm schema + OEM stubs`.

## Standing (this tip)

Tip pin: absorb `98761f257e9b7a369001a3708148707ad581256d` (~`c11ea7f2a+` / mix-NIF `ba9824207`; `cursor/v2711-absorb-932-936-4b0b`; content from #933 @ `5c08f3312`).

| Surface | Standing | Evidence |
|---------|----------|----------|
| Rhythm schema + OEM stubs | **PARTIAL_ALIVE** | `brand_toml_rhythm_court` |
| Fluid clamp / cqi slopes | **PARTIAL_ALIVE** | `brand_fluid_clamp_court` |
| OEM hot-swap fence | **PARTIAL_ALIVE** | `seed_hot_swap_slot` / `hot_swap_brand` + `brand_toml_hotswap_court` |
| Production runtime theme | **UNSUPPORTED** | `promote_production_theme` → `ProductionThemeRefusal::ProductionUnsupported` (code `REFUSED_PRODUCTION`) |

Hot-swap and fluid-clamp are CONSTRUCT fences with standing ceiling
`PARTIAL_ALIVE`. They do **not** authorize production theme DO, CASTLE publish,
or full DTCG color/type inventory `ALIVE`.

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

Companion PARTIAL fences (not production promote):

```bash
cargo test -p ggen-graph --test brand_toml_hotswap_court
cargo test -p ggen-graph --test brand_fluid_clamp_court
cargo test -p ggen-graph --test brand_toml_rhythm_court
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

- Hot-swap green → `PARTIAL_ALIVE` only.
- `promote_production_theme` → always `REFUSED_PRODUCTION` on Gate-1 path.
- Never promote inspection of this markdown file to `ALIVE`.
- Never claim production theme `ALIVE` from rhythm / fluid / hot-swap courts alone.
