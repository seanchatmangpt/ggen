# Gate-1 gap matrix — ECOSYSTEM-SPEC-27.1.1

Remaining-courts ledger: [`GATE-1-REMAINING-COURTS-STANDING.md`](./GATE-1-REMAINING-COURTS-STANDING.md) (verify_witness / ui-layout / slope_cqi / LayoutRefusal).

Companion to `ECOSYSTEM-SPEC-27.1.1.md` and `REPO-INVENTORY-27.1.1.md`.

**Evidence refresh / live tip pin:** CindyCat dual-schema `[primitive.space]` from `[rhythm]` on `cursor/v2711-hop4-cindycat-space-bc54` (court `oem_fixtures_carry_dtcg_primitive_space_from_rhythm`).

**Observed crown tip (ui_layout exact tip-cargo absorb 2026-11-22):** tip-wins onto
`origin/cursor/ecosystem-spec-27-1-1-bc54` after Smalt honesty `a40338aea` (prior FM-PACK absorb
`e73bf686c` / OOR retip9 `fbfaa3ad9`).
**brand_toml** exact tip-cargo **ALIVE** (unchanged) — completeness **5** + rhythm **12** EXIT:0 @ `77eb46fd0`
([`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md); land `a508e6bae`).
**ui_layout** **ALIVE (exact tip-cargo)** — **11 passed; exit 0** @ cargo tip `a508e6bae`
(`bc-70188ac3`; `/tmp/ggen-uilayout-exact-tip`; sources ≡ live tip; lineage `59c090984` retained)
([`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)).
**FM-PACK-001** **ALIVE (lineage tip-cargo)** absorbed — `composed_packs_e2e` **2 passed; exit 0** @ evidence
`19c08a3b0` / court tip `a508e6bae` ([`FM-PACK-001-ALIVE-STANDING.md`](./FM-PACK-001-ALIVE-STANDING.md)).
Tip-wins retained: Truex **ALIVE** @ `68d2d233`; `shui:` **ALIVE (lineage tip-cargo)** @ `008a81d21`;
BRCE seat + R01 exact tip-cargo **ALIVE** @ `e7569893a`; `write_apply` **7+1+1** @ `cde02d1f2`;
taffy/fluid/hotswap @ `cde02d1f2`/`19c08a3b0`; construct + lifecycle **ALIVE**.
**OOR:** cos **503** / revops **NXDOMAIN** / `--live` **BLOCKED_HITL** / BRCE **V8** **BLOCKED** /
R-01…R-07 bodies **BLOCKED**.

**Clause-2 residual honesty (2026-11-22):** tip-wins absorb `cursor/v2711-brce-v8-bodies-bc54` @ `e54d58bac` onto crown
`fe72be0fb` (⊇ VW absorb `fe72be0fb` + Smalt `9173c6529` + OOR retip11 `f5f44571f` + CASTLE) — pin BRCE **V8** **BLOCKED** (owner **BRCE**; Founder HITL mount+WebAuthn;
OOR **G2-3**) and R-01…R-07 *bodies* **BLOCKED** (owner risk-register; Founder HITL; OOR **G3-5**);
seat + R01 *contract* exact tip-cargo **ALIVE** retained @ `e7569893a`. **No** invented V8/bodies `ALIVE`.
Detail: [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) / [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) /
[`R01-R07-STANDING.md`](./R01-R07-STANDING.md).

**Tip-cargo reconfirm (2026-11-22):** `cursor/v2711-verify-witness-alive-bc54` from crown
`513704d048fcf19dffe5fad82ba23a8b647b6830` — quiet
`CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-verify-witness-target cargo test -p ggen-graph --test verify_witness_size_court --locked`
→ **6 passed; 0 failed; exit 0** (`/tmp/ggen-verify-witness-cargo.log`; loadavg_1m ≤10).
Witness harness sources ≡ crown `513704d04` / `678a6c106`. Production WASM remains **UNSUPPORTED**.

**Tip-cargo reconfirm (2026-11-22):** `cursor/v2711-shui-prefix-alive-bc54` from crown
`008a81d21f14d467be9da7951b39c8d655acb5f0` — quiet
`CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-shui-prefix-target2 cargo test -p ggen-graph --test shui_public_prefix_court --locked`
→ **4 passed; 0 failed; exit 0** (`/tmp/cursor/shui-prefix-alive.status`;
`/tmp/shui_public_prefix_court4.log`). Sources ≡ crown `008a81d21`. Prior lineage
`26256ca9d` retained. Did not compete with ui_layout tip-exclusive target.

**Crown absorb (2026-11-22):** onto `origin/cursor/ecosystem-spec-27-1-1-bc54` past `bc97ceac9` —
`cursor/v2711-hop4-mix-nif2-bc54` @ `dc48630e2` / `fc0d449af`; `cursor/v2711-crown-progress2-bc54` @
`0e25df932`; `cursor/v2711-gate1-next-courts-bc54` @ `26256ca9d413617f0e8b1418d101708ec4fbc2a3`
(quiet tip-cargo **ALIVE** for shui/taffy/rhythm after `toml::Table` fix; hot-swap **6 passed; exit 0**
on same commit; production theme **UNSUPPORTED** — do **not** attribute brand rhythm tip-cargo to
ancestor doc-only `27436608c` pre-fix).

**Tip-exclusive ui_layout reconfirm (2026-11-22):** `cursor/uilayout-retry-alive-dbe1` /
`bc-921ca9e3` — after reclaim of orphan `/tmp/ggen-uilayout-0ae8-protected` (62G; prior
**BLOCKED_ENOSPC** @ `c329b7ee5`), quiet
`CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-retry-9d4a --config build.jobs=1 -- --test-threads=1`
on detached tip → **11 passed; 0 failed; exit 0** @ evidence `59c090984` (harness sources
byte-identical through crown tip `6e78d4347`; receipt `/tmp/cursor/uilayout-retry-9d4a.md`).
Label was **ALIVE (lineage tip-cargo)** pending exact tip; superseded 2026-11-22 by
exact tip-cargo **ALIVE** @ `a508e6bae` (`bc-70188ac3`; [`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)). Not a standing demotion — ENOSPC was environmental.

**Evidence refresh / ui_layout absorb:** `cursor/v2711-uilayout-tip-alive-bc54` @ `8090022f4` → lineage
`120598fb3e395393d0ebe7fd86537bb499fc670d` … crown `678a6c106` (hop-5 runtime **PRESENT** / model access
**REFUSED**; ui_layout tip-cargo **ALIVE** @ `ca52e9c16`; tip-exclusive reconfirm **ALIVE** (lineage tip-cargo) @ `59c090984`).
Truex+shui product admit → **ALIVE** (sibling **6/6 vitest exit 0** @
`truex@68d2d2331696f18949f4877b994320018b6365e5` — [truex#6](https://github.com/seanchatmangpt/truex/pull/6)
**MERGED** to `main` as `b4672131`; mount `/tmp/truex`; receipt
`/tmp/cursor/truex-merged-admit-alive.md`). Hop-4 BrandTheme manufacture →
**ALIVE** (`diff_exit=0` dual-schema tip brand vs igniter fixture; evidence
`/tmp/cursor/igniter-real-cleanroom.md`; prior PARTIAL `:no_primitive_space`
closed). Hop-5 aggregate → **PARTIAL_ALIVE** (default adapter exit **0**;
foundry `--live` **foundry_exit=1** / **REFUSED** (`Model access is not configured`
after Linux tip-matched runtime **PRESENT**) —
[`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md);
[`HOP5-MODEL-ACCESS-STANDING.md`](./HOP5-MODEL-ACCESS-STANDING.md);
[`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md);
[`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md)).
**brand.toml hot-swap** → tip-cargo **ALIVE** (**6 passed; exit 0** absorb @ `cde02d1f2` /
crown `19c08a3b0`); **production theme** → **UNSUPPORTED**.
`taffy_aot_preflight_court` tip-cargo **ALIVE** + `shui_public_prefix_court` tip-cargo
**ALIVE** (lineage) on tip. `brand_toml_rhythm_court` + `brand_toml_completeness_court` →
**ALIVE (exact tip-cargo)** — **12+5** EXIT:0 @ `77eb46fd0` (unchanged;
[`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md); land `a508e6bae`).
`verify_witness_size_court` tip-cargo **ALIVE** — **6 passed; exit 0** @ `c91b22561`;
production WASM **UNSUPPORTED**. **ui_layout ALIVE (exact tip-cargo)** —
`CARGO_BUILD_JOBS=1` exclusive `/tmp/ggen-uilayout-exact-tip` → **11 passed; 0 failed; exit 0**
@ cargo tip `a508e6bae` (`bc-70188ac3`; sources ≡ live tip; receipt
[`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)). Lineage prior
**11** @ `59c090984` / `ca52e9c16` retained. Ancestor
evidence `eaf0dad40287a3347280a541766b3b16f3978cf8` retained. **FM-PACK-001**
`packs/gh-terraform-pack` → **ALIVE** (lineage tip-cargo `composed_packs_e2e` **2 passed; exit 0** @ evidence `19c08a3b0` / court tip `a508e6bae`; absorbed @ crown `e73bf686c`; [`FM-PACK-001-ALIVE-STANDING.md`](./FM-PACK-001-ALIVE-STANDING.md)).
Gate-0 **cos** → **BLOCKED** (external; `/readyz` **503**). Historical court exits remain on ancestor
`0478fde59644cccd60c5a88d64d444ec292e6a3c` (≥ `a54b9803`) and prior ui-layout
evidence `eaf0dad40287a3347280a541766b3b16f3978cf8` — both ancestors of tip.

**Honesty demotion (`cursor/v2711-gate1-honesty-retip-5536` on tip `fdcad534d`):** brand
rhythm + completeness demoted to **COMPILING** / **PARTIAL_ALIVE** (no tip EXIT:0);
`r01_r07` contract fence now **ALIVE (lineage tip-cargo)** (**7** @ `cd7c2433b`); exact-tip reconfirm SCHEDULED. Tip-cargo
**ALIVE** retained only where tip evidence exists: `taffy_aot` / `brand_fluid_clamp` /
`brand_toml_hotswap` @ `cde02d1f2`→`19c08a3b0`; `write_apply` **7+1+1** @ `cde02d1f2`;
construct + lifecycle **ALIVE**; BRCE **V8** **BLOCKED**. In-repo size court tip-cargo
**ALIVE** @ `c91b22561`; production artifact path **UNSUPPORTED** only. Production WASM /
zero-HITL remain **UNSUPPORTED** / **not ALIVE**. Do not invent new `ALIVE`. Do not
promote any row to tip-subject `ALIVE` without a recorded exit **0** for that exact tip.
Horizontal spine HITL ledger: [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md).

**Pass-count footnotes:** `shui_public_prefix_court` **4**; `taffy_aot_preflight_court` **7**
(tip-cargo **ALIVE**); `brand_toml_rhythm_court` **12** (**ALIVE** exact tip-cargo @ `77eb46fd0`);
`brand_toml_completeness_court` **5** (**ALIVE** exact tip-cargo @ `77eb46fd0`);
`brand_toml_hotswap_court` **6**; `brand_fluid_clamp_court` **6** (tip-cargo **ALIVE** absorb @ `cde02d1f2` / crown `19c08a3b0`);
`ui_layout_court` **11** (**ALIVE** exact tip-cargo **11 passed; exit 0** @ `a508e6bae`);
`verify_witness_size_court` **6** (tip-cargo **6 passed; exit 0** @ `c91b22561`). Historical exit-0
pass counts below remain the evidence log (shui **3** / taffy **6** / brand **4** /
ui-layout **8** on their evidence SHAs).

**Crown absorb (2026-11-22):** tip-wins land `cursor/v2711-brand-fixtures-court-bc54` @
`18717e70e` (`BrandRefusal::Incomplete` + `brand_toml_completeness_court`) onto crown tip
`ce6c281c9+`. Completeness standing **COMPILING** / **PARTIAL_ALIVE** until tip EXIT **0**
(no cargo this honesty retip). `write_apply` tip-cargo **ALIVE** **7+1+1** @ `cde02d1f2`
(absorb tip `fdcad534d`).

**Gate 2 commercial priority (not a Gate-1 falsifier):** CindyCat patronage **FIRST** —
LinkedIn direct outreach before content calendar; Fourth Watch as natural follow-on,
never cold cyber open. See [`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md),
[`CINDYCAT-REVERSE-GLOO.md`](./CINDYCAT-REVERSE-GLOO.md),
[`LINKEDIN-DEAL-PLAYBOOK.md`](./LINKEDIN-DEAL-PLAYBOOK.md). Do not promote those docs
to deal `ALIVE`.

## Gate 0 — out of scope for ggen

Full standing matrix + stub citations: [`GATE-0-STANDING.md`](./GATE-0-STANDING.md).

| Requirement | Current evidence | Next exact verifier | Owner |
|-------------|------------------|---------------------|-------|
| GCP billing | none in ggen | external billing console / infra receipt | external (infra) |
| servengine M4 | none in ggen | servengine M4 court in that repo | external (servengine) |
| cos ledger fixes | **BLOCKED** (none in ggen; cos `/readyz` **HTTP 503** tracked in [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) / Foundry spine) | cos ledger replay/verify; `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** | external (cos) |
| year-end SOW | none in ggen | commercial SOW register | external (ops) |

Do not promote any Gate 0 row to `ALIVE` from this repository.

---

## Gate 1 — in-repo / sibling (Dec 6–18)

| Requirement | Standing + live evidence | Tip SHA (subject) | Next exact verifier / command | Owner |
|-------------|--------------------------|-------------------|-------------------------------|-------|
| Admit W3C `shui:` as public vocab (ggen fence) | **ALIVE (lineage tip-cargo)** — **4 passed; exit 0** @ crown `008a81d21` (`CARGO_TARGET_DIR=/tmp/ggen-shui-prefix-target2`; `cursor/v2711-shui-prefix-alive-bc54`; `/tmp/cursor/shui-prefix-alive.status`). Sources ≡ tip `c5101d597` (no `shui*` / `public_prefixes` drift). Lineage `26256ca9d` retained. Ancestor `0478fde59` (**3 passed**). **No exact tip EXIT:0** on live crown — not tip-subject fence `ALIVE`. Truex product admit is a separate row. | tip `c5101d597` (cargo evidence `008a81d21`; prior `26256ca9d`) | `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-shui-prefix-target2 cargo test -p ggen-graph --test shui_public_prefix_court --locked` | **ggen** (fence) |
| Truex runtime admits `shui:` shapes | **ALIVE** — sibling Chicago admit-court **6/6 vitest exit 0** on `seanchatmangpt/truex` @ `68d2d2331696f18949f4877b994320018b6365e5` ([truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED** → `main` merge `b4672131`; mount `/tmp/truex`; receipt `/tmp/cursor/truex-merged-admit-alive.md`; **not re-run** this retip). Companion ggen `shui:` fence **ALIVE (lineage tip-cargo)** @ `008a81d21` (non-substitute; ≠ Truex product crown). | truex@`68d2d233` (merged; on `main` via `b4672131`) | Replay: `pnpm exec vitest run test/admit/admit-court.test.ts` → exit 0 @ `68d2d233`. Companion fence (non-substitute): `cargo test -p ggen-graph --test shui_public_prefix_court` | **truex** |
| Taffy AOT pre-flight (widths 320/375/414/768/1024/1440 + WCAG 1.4.12 1.5×) | **ALIVE** (exact tip-cargo) — **7 passed; exit 0** @ crown `19c08a3b0` absorb @ `cde02d1f2` (`cursor/v2711-taffy-brand-absorb-a77d`; `/tmp/gate1-taffy`). Lineage `26256ca9d` retained. Photolithography / zcode-cli consume still open → path **PARTIAL_ALIVE**. Hop: [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md). | absorb tip `fdcad534d` (cargo `19c08a3b0` / `cde02d1f2`) | `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked` | **ggen** |
| ggen `ui-layout-court` (AotSealReceipt digest consume) | **ALIVE (exact tip-cargo)** — exclusive `CARGO_BUILD_JOBS=1` `/tmp/ggen-uilayout-exact-tip` → **11 passed; 0 failed; exit 0** @ cargo tip `a508e6bae` (`bc-70188ac3`; sources ≡ live tip; [`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)). Lineage prior **11** @ `59c090984` / `ca52e9c16`; evidence `eaf0dad40` (**8**); tip court land `7c0ba2a90`. Sibling `ui-layout-court.ts` remains out-of-repo / **UNSUPPORTED**. | cargo tip `a508e6bae` (lineage `59c090984`; absorb `cursor/uilayout-exact-tip-alive-9f6c`) | Replay: `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-tip --config build.jobs=1 -- --test-threads=1`; next: `pnpm test -- ui-layout-court` in zcode-cli | **ggen** (in-repo fence); **zcode-cli** (TS adapter, separate) |
| FM-PACK-001 `packs/gh-terraform-pack` | **ALIVE** (lineage tip-cargo) — path on tip (`test -d packs/gh-terraform-pack` → **0**; tree `c7799cd6`); `composed_packs_e2e` **2 passed; exit 0** @ evidence `19c08a3b0` / court tip `a508e6bae` (sources ≡ absorb tip `fbfaa3ad9`; loadavg_1m=5.01). Ledger: [`FM-PACK-001-ALIVE-STANDING.md`](./FM-PACK-001-ALIVE-STANDING.md) / [`REPO-INVENTORY-27.1.1.md`](./REPO-INVENTORY-27.1.1.md) | evidence `19c08a3b0` → court `a508e6bae` → absorb tip (land `cursor/v2711-fm-pack-001-alive-bc54` @ `ce613b05d`) | Replay: `CARGO_BUILD_JOBS=1 cargo test -p ggen-engine --test composed_packs_e2e --target-dir /tmp/ggen-fm-pack-001-alive-target --config build.jobs=1 -- --nocapture` → exit **0** | **ggen** (pack + composed e2e) |
| zcode POWL/OCEL bounded remediation (in-repo stub) | **ALIVE** (bounded-refusal tip-cargo) — `zcode_powl_ocel_court` **6 passed; exit 0** + `remediation_bound` **4 passed; exit 0** @ tip `cd7c2433b` (`cursor/v2711-zcode-powl-refusal-alive-bc54`). Foundry `--dry-run` **PARTIAL** (exit **2**). Live `--live` **BLOCKED_HITL**. Ledger: [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) / [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) | tip `cd7c2433b7e124db6606008a92081d23aa3727ce` | Replay: `SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target cargo test -p ggen-sjira --test zcode_powl_ocel_court`; `… remediation_bound --lib`. Live next: provider creds + `bun scripts/powl-foundry-loop.ts --live` → **0** | **ggen** (contract); **zcode-cli** (execution) |
| `curve25519-dalek` v4 / cgap-aoq docs | **PARTIAL_ALIVE** (lock + pin doc) — `Cargo.lock` resolves **4.1.3** and **5.0.0**; policy recorded in [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md). Policy court that refuses undocumented major still absent → not crown `ALIVE`. | tip `120598fb3e395393d0ebe7fd86537bb499fc670d` (lock observation; prior evidence `0478fde59`) | `rg -n 'name = "curve25519-dalek"' -A2 Cargo.lock` (expect 4.1.3 + 5.0.0); next ALIVE: Gate-1 policy court per pin standing §5 | **ggen** (+ cgap-aoq docs owner) |
| `verify_witness` WASM &lt; 250KB | **ALIVE** (exact tip-cargo; size court only) — `set -o pipefail; cargo test -p ggen-graph --test verify_witness_size_court --locked --target-dir /tmp/ggen-verify-witness-tip-ebf4 --config build.jobs=1 -- --test-threads=1` → **6 passed; EXIT=0** on tip-cargo subject `f5f44571f` (absorb base `9173c6529` post-Smalt; side `cursor/v2711-verify-witness-prod-bc54` @ `6c2a9dc4c`; log `/tmp/cursor/verify-witness-prod-size-court-side.log`). Prior reconfirm @ `c91b22561`; cold @ `cd7c2433b`; ancestor `0478fde59`. Placeholder **35** B. Affidavit pin size court **ALIVE** (**28673** B @ `563eb322a`; probe `bc-5f5786ff`) — [`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`](./VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md). ggen production wire ([`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md)) remains **UNSUPPORTED** — affidavit `ALIVE` ≠ ggen production `ALIVE` | tip-cargo `f5f44571f` (sources ≡ `9173c6529`; no cargo this absorb) | Size replay: command above. Affidavit replay (held): `just gatekeeper-wasm-size-court` → EXIT **0**; `wc -c` → **28673**. Exact next: `GGEN_VERIFY_WITNESS_WASM=…` + `verify_production_witness` on tip | **ggen** (size court) + **affidavit** (`affidavit-gatekeeper-wasm`) |
| R-01…R-07 resolution contract | **ALIVE** (exact tip-cargo) — `r01_r07_resolution_court` **7 passed; exit 0** @ `e7569893a` (`/tmp/ggen-gate1-brce-r01-target`; cold `d8eb26e9c`; lineage `cd7c2433b`). Risk *bodies* remain **BLOCKED** — [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) / [`R01-R07-TIP-CARGO-STANDING.md`](./R01-R07-TIP-CARGO-STANDING.md) / OOR **G3-5** | tip `e7569893a` | Contract replay: `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-gate1-brce-r01-target cargo test -p ggen-graph --test r01_r07_resolution_court --locked -- --test-threads=1`. Bodies next: Founder HITL admit sovereign risk-register → per-ID typed receipt/`REFUSED_*` (no invention) | **ggen** (contract); risk-register owner + **Founder HITL** (bodies) |
| Brand `.toml` rhythm schema + OEM stubs (Fourth Watch / CindyCat) | **ALIVE (exact tip-cargo)** (unchanged) — completeness **5** + rhythm **12** EXIT:0 @ `77eb46fd0` ([`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md); land `a508e6bae`; lineage `c91b22561`). `brand_fluid_clamp_court` + `brand_toml_hotswap_court` tip-cargo **ALIVE** (**6+6 passed; exit 0** absorb @ `cde02d1f2` / crown `19c08a3b0`). Full DTCG color/type/CASTLE inventories Gate 2+. **Production theme** → **UNSUPPORTED** (`REFUSED_PRODUCTION`). **Commercial** CindyCat-first ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)). | cargo tip `77eb46fd0` (land `a508e6bae`; fluid/hotswap `19c08a3b0` / `cde02d1f2`) | Replay: `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-brand-exact-tip cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked -- --test-threads=1` → EXIT **0**. Fluid/hot-swap replay: `… brand_fluid_clamp_court` / `… brand_toml_hotswap_court`. Production refuse: `cargo test -p ggen-graph --lib layout::brand::production_theme_tests`. Next promote: [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md). Full inventory: OOR **G2-13** / **G2-12** | **ggen** (rhythm + completeness + hot-swap + REFUSED_PRODUCTION fence) + brand plane (token inventory Gate 2+) |
| Plane 5 front doors | **UNKNOWN** / out of DO | n/a | Gate 2–3 — not a Gate-1 falsifier; commercial open is CindyCat patronage before Fourth Watch cold cyber ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)) | external |
| BRCE + WebAuthn AS-01 (`:prime256v1`) ungated-DO fence | **ALIVE** (exact tip-cargo seat fence) — `brce_as01_seat_court` **6 passed; exit 0** @ `e7569893a` (`/tmp/ggen-gate1-brce-r01-target`; cold `d8eb26e9c`; ancestor `0478fde59`). Crypto verify **UNSUPPORTED**; live BRCE **V8** **BLOCKED**; fence ≠ broker `ALIVE`. Standing [`BRCE-AS01-SEAT-TIP-CARGO-STANDING.md`](./BRCE-AS01-SEAT-TIP-CARGO-STANDING.md) / [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) | tip `e7569893a` | Fence replay: `CARGO_BUILD_JOBS=1 cargo test -p ggen-abb-sbb --test brce_as01_seat_court --locked`. **V8** next (owner **BRCE**; Founder HITL): mounted broker court — without AS-01 → `REFUSED_*`; with seat → receipt+replay → **0** (OOR **G2-3**). Then **V9** / G2-4/G2-5 | **ggen** (fence); **BRCE** + Founder HITL (V8); Plane-5 (V9) |
| ggen_igniter clean-room / manufacture | **ALIVE** — tip dual-schema `fourth-watch.brand.toml` BrandTheme emit vs igniter fixture → `diff -rq` **0** + compare-subject `git diff --exit-code` → **0** (`/tmp/cursor/igniter-real-cleanroom.md`; `bc-4861c787`). Mount PRESENT; clean-tree checkpoint 0. Jobs/C4 residual **ALIVE** (admitted `--pack-dir sample-pack … --check` exit **0**; NIF **ALIVE**; bare `--check` **REFUSED_INVOCATION** antifalsifier — [`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md)). In-repo contract **PARTIAL_ALIVE** [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md); hop ledger [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) | tip `f1492ed55+` (hop-4 check-alive `cursor/v2711-hop4-mix-check-alive-bc54`; manufacture land `cursor/v2711-hop4-manufacture-bc54`) | **Replay:** BrandTheme emit tip + igniter fixture (canon path) → `git diff --exit-code` **0**. **Jobs/C4 replay:** admitted `--pack-dir test/fixtures/sample-pack … --check` → 0. Checkpoint: `cd /home/ubuntu/ggen_igniter && git diff --exit-code`. In-repo: `cargo test -p ggen-sjira --test igniter_cleanroom_court` | **ggen_igniter** (crown); **ggen** (dual-schema tip brand) |

### Command evidence log (this refresh)

Observed crown tip: `e73bf686c416b7c96b27e9f93b4b07f02e5490a3` (FM-PACK-001 ALIVE absorb after OOR retip9
`fbfaa3ad9`; BRCE seat + R01 exact tip-cargo ALIVE @ `e7569893a`; write_apply `cde02d1f2`;
taffy/fluid/hotswap `cde02d1f2`→`19c08a3b0`).
Historical exit-0 evidence tip: `0478fde59644cccd60c5a88d64d444ec292e6a3c` (≥ `a54b9803`; ancestor).
ui-layout ancestor ALIVE: `eaf0dad40287a3347280a541766b3b16f3978cf8`.
**Tip-cargo `ui_layout` ALIVE (exact tip-cargo):** exclusive `/tmp/ggen-uilayout-exact-tip` → **11 passed; exit 0** @ `a508e6bae` (`bc-70188ac3`; sources ≡ live tip).
Tip-cargo **ALIVE** retained: `taffy_aot` / fluid / hotswap @ `cde02d1f2`; `shui` **ALIVE (lineage tip-cargo)** @ `008a81d21`;
`write_apply` **7+1+1** @ `cde02d1f2`; construct + lifecycle **ALIVE**; `r01_r07` **ALIVE** exact tip @ `e7569893a`;
BRCE seat fence **ALIVE** exact tip @ `e7569893a`. Brand rhythm/completeness → **ALIVE (exact tip-cargo)** @ `77eb46fd0` (unchanged).
**FM-PACK-001** → **ALIVE (lineage tip-cargo)** absorbed. `verify_witness` tip-cargo **ALIVE** @ `c91b22561`.
**OOR:** cos **503** / revops **NXDOMAIN** / `--live` **BLOCKED_HITL** / BRCE **V8** **BLOCKED** / R-01…R-07 bodies **BLOCKED**.

| Subject | Command / binary | Result |
|---------|------------------|--------|
| `shui:` fence | `shui_public_prefix_court` tip-cargo (`/tmp/ggen-shui-prefix-target2`) | **ALIVE (lineage tip-cargo)** — **4 passed; exit 0** @ crown `008a81d21` (`shui-prefix-alive.status`); sources ≡ tip `c5101d597`; lineage `26256ca9d`; ancestor `0478fde59`; **no tip EXIT:0** on live crown |
| Truex admit-court | `pnpm exec vitest run test/admit/admit-court.test.ts` @ `truex@68d2d233` | **ALIVE** — **6/6** exit 0; [truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED** `b4672131`; not re-run this retip |
| `taffy_aot` | `taffy_aot_preflight_court` tip-cargo (`/tmp/gate1-taffy`) | **ALIVE** (exact tip) — **7 passed; exit 0** @ crown `19c08a3b0` absorb @ `cde02d1f2` |
| `brand.toml` rhythm | `brand_toml_rhythm_court` exact tip-cargo (`/tmp/ggen-brand-exact-tip`) | **ALIVE (exact tip-cargo)** — **12 passed; exit 0** @ `77eb46fd0` (unchanged; [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md); land `a508e6bae`) |
| brand.toml completeness | `brand_toml_completeness_court` exact tip-cargo (`/tmp/ggen-brand-exact-tip`) | **ALIVE (exact tip-cargo)** — **5 passed; exit 0** @ `77eb46fd0` (unchanged; land `a508e6bae`) |
| brand.toml fluid / hot-swap | `brand_fluid_clamp_court` + `brand_toml_hotswap_court` tip-cargo (`/tmp/gate1-taffy`) | **ALIVE** — **6+6 passed; exit 0** absorb @ `cde02d1f2` / crown `19c08a3b0`; production `promote_production_theme` → **UNSUPPORTED** / `REFUSED_PRODUCTION` |
| `write_apply` CCS #2 | `write_apply_confirm_test` + pipeline CP17 courts | **ALIVE** — **7+1+1 passed; exit 0** @ observed `cde02d1f2` (evidence `42c5b9ba4`; absorb tip `fdcad534d`) |
| construct + lifecycle | `construct_hash_gate` + `lifecycle.rs` tip-cargo | **ALIVE** — construct **4** @ `861a6e73a`; lifecycle **PRESENT**/tip-cargo **6/6** @ `513704d04`/`acd9705c1`; BRCE **V8** **BLOCKED** |
| `verify_witness` size court | `verify_witness_size_court` tip-cargo (`/tmp/ggen-verify-witness-tip-ebf4`) | **ALIVE** (exact tip-cargo) — **6 passed; EXIT=0** @ `f5f44571f` (absorb base `9173c6529`; sources ≡); prior `c91b22561`; cold @ `cd7c2433b`; ancestor `0478fde59`; affidavit pin **ALIVE** @ `563eb322a`; ggen production wire **UNSUPPORTED** |
| R-01…R-07 contract | `r01_r07_resolution_court` tip-cargo (`/tmp/ggen-gate1-brce-r01-target`) | **ALIVE** (exact tip-cargo) — **7 passed; exit 0** @ `e7569893a`; lineage `cd7c2433b`; bodies **BLOCKED** |
| BRCE AS-01 seat | `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` | **ALIVE** (exact tip-cargo) — **6 passed; exit 0** @ `e7569893a`; **V8** **BLOCKED** |
| zcode POWL stub | `zcode_powl_ocel_court` + `remediation_bound` tip-cargo | **ALIVE** (bounded refusal) — **6+4 passed; exit 0** @ tip `cd7c2433b`; live `--live` **BLOCKED_HITL** |
| `ui-layout-court` | exact-tip `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-tip --config build.jobs=1 -- --test-threads=1` | **ALIVE (exact tip-cargo)** **11 passed; 0 failed; exit 0** @ `a508e6bae` (`bc-70188ac3`; sources ≡ live tip; [`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)); lineage prior **11** @ `59c090984` / `ca52e9c16`; evidence **8** @ `eaf0dad40`; tip source **11** `#[test]` |
| `ui-layout-court.ts` (contract stub) | `docs/rfc/v27.1.1/fixtures/ui-layout-court.ts` | stub PRESENT; zcode-cli runner still **UNSUPPORTED** |
| `verify_witness.wasm` (production) | `find . \( -name 'verify_witness*' -o -name '*witness*.wasm' \)` | placeholder **35** B only in ggen → ggen production **UNSUPPORTED**; affidavit pin size court **ALIVE** (**28673** B @ `563eb322a`); wire open |
| `curve25519-dalek` | `rg -n 'name = "curve25519-dalek"' -A2 Cargo.lock` + pin standing | versions **4.1.3** + **5.0.0** present; pin doc → **PARTIAL_ALIVE**; policy court still open |
| FM-PACK-001 | `composed_packs_e2e` lineage tip-cargo + inventory | **ALIVE** (**2 passed; exit 0** @ `19c08a3b0` / court `a508e6bae`) |
| ggen_igniter checkout / manufacture | local mount + BrandTheme emit tip dual-schema vs igniter fixture + `git diff --exit-code` + admitted mix `--check` | mount **PRESENT**; clean-tree exit **0**; manufacture compare `diff_exit=0` → hop-4 **ALIVE**; Jobs/C4 admitted `--check` **ALIVE** (antifalsifier: bare post-clone diff ≠ `ALIVE`; NIF `.so` / bare exit-2 ≠ Jobs/C4 `ALIVE`) |

Replay via Cargo (preferred CI falsifier; same courts; `ui_layout` tip-cargo **ALIVE**; other fences tip-cargo PENDING until quiet exclusive exit 0):

```text
cargo test -p ggen-graph --test shui_public_prefix_court
cargo test -p ggen-graph --test taffy_aot_preflight_court
cargo test -p ggen-graph --test ui_layout_court
cargo test -p ggen-graph --test brand_toml_rhythm_court
cargo test -p ggen-graph --test brand_toml_completeness_court
cargo test -p ggen-graph --test brand_fluid_clamp_court
cargo test -p ggen-graph --test brand_toml_hotswap_court
cargo test -p ggen-graph --lib layout::brand::production_theme_tests
cargo test -p ggen-graph --test verify_witness_size_court
cargo test -p ggen-graph --test r01_r07_resolution_court
cargo test -p ggen-abb-sbb --test brce_as01_seat_court
cargo test -p ggen-sjira --test zcode_powl_ocel_court
```

---

## Recommended next single falsifier (Gate 1)

Tip-wins `ui_layout` exact tip-cargo absorb on tip after Smalt honesty `a40338aea`.
Tip-cargo **ALIVE**: brand_toml exact tip-cargo **ALIVE** (unchanged) @ `77eb46fd0`;
`ui_layout` **ALIVE (exact tip-cargo)** **11 passed; exit 0** @ `a508e6bae` (`bc-70188ac3`;
[`UILAYOUT-TIP-CARGO-STANDING.md`](./UILAYOUT-TIP-CARGO-STANDING.md)); **FM-PACK-001** **ALIVE (lineage tip-cargo)** absorbed;
`brce_as01_seat_court` **6** + `r01_r07_resolution_court` **7** EXIT:0 @ `e7569893a`;
`taffy_aot` / fluid / hotswap @ `cde02d1f2`→`19c08a3b0`; `write_apply` **7+1+1** @ `cde02d1f2`;
construct + lifecycle **ALIVE**; `verify_witness` @ `c91b22561`.
**OOR:** cos **503** / revops **NXDOMAIN** / `--live` **BLOCKED_HITL** / BRCE **V8** **BLOCKED** /
R-01…R-07 bodies **BLOCKED**.

- Truex product admit → **ALIVE** (sibling merged @ `truex@68d2d233` / `b4672131`; not re-run); ggen `shui:` fence → **ALIVE (lineage tip-cargo)** (**4** @ `008a81d21`; sources ≡ tip; **no tip EXIT:0** on live crown)
- brand rhythm + completeness → **ALIVE (exact tip-cargo)** @ `77eb46fd0` (unchanged); fluid+hot-swap → **ALIVE** (exact tip **6+6** @ `19c08a3b0` / absorb `cde02d1f2`); production theme → **UNSUPPORTED** / `REFUSED_PRODUCTION` — [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md)
- Taffy AOT six-width seal → **ALIVE** (exact tip-cargo **7** @ `19c08a3b0`; photolithography consume still open)
- `write_apply` CCS #2 → **ALIVE** (**7+1+1** @ `cde02d1f2`); construct + lifecycle → **ALIVE**; BRCE **V8** → **BLOCKED**
- `verify_witness` size court → **ALIVE** (exact tip-cargo **6** @ `c91b22561`; production WASM **UNSUPPORTED**)
- BRCE AS-01 seat → **ALIVE** (exact tip-cargo fence **6** @ `e7569893a`; crypto / live BRCE still open; fence ≠ broker `ALIVE`; **V8** **BLOCKED**)
- R-01…R-07 contract → **ALIVE** (exact tip-cargo **7** @ `e7569893a`; bodies **BLOCKED**)
- zcode POWL in-repo stub → **ALIVE** (bounded refusal); foundry `--live` → **BLOCKED_HITL**
- FM-PACK-001 → **ALIVE** (lineage tip-cargo absorbed; `composed_packs_e2e` **2 passed; exit 0** @ `19c08a3b0` / court `a508e6bae`)
- Gate-0 cos → **BLOCKED** (`/readyz` **503**); revops → **BLOCKED** (**NXDOMAIN**)
- zero-HITL fab / production WASM / production brand theme swap → **not** `ALIVE` (in-repo hot-swap fence is tip-cargo **ALIVE**; production theme **UNSUPPORTED**)

ggen in-repo `ui-layout-court` digest consume → **ALIVE (exact tip-cargo)** (**11 passed;
exit 0** @ `a508e6bae`; `bc-70188ac3`; sources ≡ live tip). Lineage prior `59c090984` /
`eaf0dad40` retained.

Next **new** Gate-1 falsifier to author (pick one):

1. Hop-5 foundry `--live`: `bun scripts/powl-foundry-loop.ts --live` → exit **0** (adapter already 0; **BLOCKED_HITL** — [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md); [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md)). `ui_layout` exact tip-cargo sealed.
2. Or advance OOR / sibling rows: cos `/readyz` → **200**; revops DNS; production `verify_witness.wasm` on affidavit plane.
3. Gate1 aggregate remains **PARTIAL** (production theme **UNSUPPORTED**; BRCE fence ≠ broker `ALIVE`; **V8** **BLOCKED**; R-01…R-07 bodies **BLOCKED**).
