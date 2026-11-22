# Gate-1 gap matrix — ECOSYSTEM-SPEC-27.1.1

Remaining-courts ledger: [`GATE-1-REMAINING-COURTS-STANDING.md`](./GATE-1-REMAINING-COURTS-STANDING.md) (verify_witness / ui-layout / slope_cqi / LayoutRefusal).

Companion to `ECOSYSTEM-SPEC-27.1.1.md` and `REPO-INVENTORY-27.1.1.md`.

**Evidence refresh / live tip pin:** CindyCat dual-schema `[primitive.space]` from `[rhythm]` on `cursor/v2711-hop4-cindycat-space-bc54` (court `oem_fixtures_carry_dtcg_primitive_space_from_rhythm`).

**Observed crown tip (read-only audit 2026-11-22):** `678a6c106ad9aa78f0d198146ef8021a388fd8a4`
(`origin/cursor/ecosystem-spec-27-1-1-bc54`). Court harness sources for Gate-1 falsifiers are
byte-identical from evidence commits through this tip except where noted below; no `cargo` replay
was run on `678a6c106` in this pass.

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

**Evidence refresh / ui_layout absorb:** `cursor/v2711-uilayout-tip-alive-bc54` @ `8090022f4` → lineage
`120598fb3e395393d0ebe7fd86537bb499fc670d` … crown `678a6c106` (hop-5 runtime **PRESENT** / model access
**REFUSED**; ui_layout tip-cargo **ALIVE** @ `ca52e9c16`).
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
**brand.toml hot-swap** → tip-cargo **ALIVE** (**6 passed; exit 0** on crown); **production theme** → **UNSUPPORTED**.
`taffy_aot_preflight_court` tip-cargo **ALIVE** + `shui_public_prefix_court` tip-cargo
**ALIVE** + `brand_toml_rhythm_court` tip-cargo **ALIVE** on evidence `26256ca9d` via
`cursor/v2711-gate1-next-courts-bc54` ([`GATE-1-NEXT-COURTS-STANDING.md`](./GATE-1-NEXT-COURTS-STANDING.md);
quiet `CARGO_BUILD_JOBS=1` + separate `/tmp` targets). `verify_witness_size_court` tip-cargo **ALIVE**
(lineage) — **6 passed; exit 0** on crown `513704d04` (`cursor/v2711-verify-witness-alive-bc54`);
production WASM **UNSUPPORTED**. **ui_layout tip-cargo ALIVE** —
exclusive quiet `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked` under flock `/tmp/ggen-uilayout-tip-alive-protected` →
**11 passed; 0 failed; exit 0** on tip lineage `ca52e9c1679d66d38d6cf7e196195ee758bee597` (ui_layout sources
byte-identical through crown `678a6c106`; absorb `8090022f4` / `cursor/v2711-uilayout-tip-alive-bc54`). Remaining Gate-1 fences
(BRCE seat tip-cargo, R-01 tip-cargo, …) stay **PENDING**. Ancestor evidence `eaf0dad40287a3347280a541766b3b16f3978cf8`
retained. **FM-PACK-001** `packs/gh-terraform-pack` → **PRESENT** (presence only;
composed e2e SKIPPED). Gate-0 **cos** → **BLOCKED** (external). Historical court exits remain on ancestor
`0478fde59644cccd60c5a88d64d444ec292e6a3c` (≥ `a54b9803`) and prior ui-layout
evidence `eaf0dad40287a3347280a541766b3b16f3978cf8` — both ancestors of tip.

**Honesty demotion (`cursor/v2711-ccs-ops-honesty-bc54` + retip2):** BRCE seat /
brand rhythm / prior over-promotions demoted to match dedicated standings
([`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md), ECOSYSTEM §8/§10,
[`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md)). In-repo size court tip-cargo
**ALIVE** (lineage) on crown `513704d04`; production artifact path **UNSUPPORTED** only.
`shui` / `taffy_aot` / `brand_toml_rhythm` tip-cargo exits **0** recorded on
`26256ca9d` (shui/taffy also green on pre-fix `27436608c`; brand rhythm requires post-fix
`26256ca9d`) — [`GATE-1-NEXT-COURTS-STANDING.md`](./GATE-1-NEXT-COURTS-STANDING.md).
In-repo brand.toml hot-swap fence is tip-cargo **ALIVE** on crown (not production
theme/CASTLE `ALIVE`). Production WASM / zero-HITL remain **UNSUPPORTED** /
**not ALIVE**. Do not invent new `ALIVE`. Do not promote any row to tip-subject
`ALIVE` without a recorded exit **0** for that exact tip. Horizontal spine HITL
ledger: [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md).

**Pass-count footnotes:** `shui_public_prefix_court` **4**; `taffy_aot_preflight_court` **7**;
`brand_toml_rhythm_court` **12** (tip-cargo **ALIVE** @ evidence `c91b22561`);
`brand_toml_completeness_court` **5** (official tip-cargo **ALIVE** @ `c91b22561`);
`brand_toml_hotswap_court` **6**; `brand_fluid_clamp_court` **6** (tip-cargo **ALIVE** @ crown `19c08a3b0`);
`ui_layout_court` **11** (tip-cargo **11 passed; exit 0**); `verify_witness_size_court` **6** (tip-cargo **6 passed; exit 0** @ crown `513704d04`). Historical exit-0
pass counts below remain the evidence log (shui **3** / taffy **6** / brand **4** /
ui-layout **8** on their evidence SHAs).

**Crown absorb (2026-11-22):** tip-wins land `cursor/v2711-brand-fixtures-court-bc54` @
`18717e70e` (`BrandRefusal::Incomplete` + `brand_toml_completeness_court`) onto crown tip
`ce6c281c9+`. Official tip-cargo closed **ALIVE** on evidence tip `c91b22561`
(`cursor/v2711-brand-toml-tip-dee5`; `/tmp/ggen-brand-toml-tip`; completeness **5** +
rhythm **12**; EXIT **0** — [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md)).
Prior PARTIAL/BUILD_BROKEN (oxrocksdb OOM) superseded. Brand sources ≡ live crown.
OOR / `write_apply` untouched.

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
| Admit W3C `shui:` as public vocab (ggen fence) | **ALIVE** (tip-cargo on crown) — **4 passed; exit 0** @ crown `008a81d21` (`CARGO_TARGET_DIR=/tmp/ggen-shui-prefix-target2`; `cursor/v2711-shui-prefix-alive-bc54`). Lineage evidence `26256ca9d` retained. Ancestor `0478fde59` (**3 passed**). Truex product admit is a separate row (**ALIVE**, sibling merged). | crown `008a81d21` (prior cargo `26256ca9d`) | `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-shui-prefix-target2 cargo test -p ggen-graph --test shui_public_prefix_court --locked` | **ggen** (fence) |
| Truex runtime admits `shui:` shapes | **ALIVE** — sibling Chicago admit-court **6/6 vitest exit 0** on `seanchatmangpt/truex` @ `68d2d2331696f18949f4877b994320018b6365e5` ([truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED** → `main` merge `b4672131`; mount `/tmp/truex`; not re-run in this audit). ggen `shui:` fence tip-cargo **ALIVE** (lineage) @ `26256ca9d`. | truex@`68d2d233` (merged; on `main` via `b4672131`) | Replay: `pnpm exec vitest run test/admit/admit-court.test.ts` → exit 0 @ `68d2d233`. Companion fence (non-substitute): `cargo test -p ggen-graph --test shui_public_prefix_court` | **truex** |
| Taffy AOT pre-flight (widths 320/375/414/768/1024/1440 + WCAG 1.4.12 1.5×) | **ALIVE** (exact tip-cargo) — **7 passed; exit 0** @ crown `19c08a3b0` (`cursor/v2711-taffy-brand-absorb-a77d`; `/tmp/gate1-taffy`). Lineage `26256ca9d` retained. Photolithography / zcode-cli consume still open → path **PARTIAL_ALIVE**. Hop: [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md). | crown `19c08a3b0` (lineage `26256ca9d`) | `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked` | **ggen** |
| ggen `ui-layout-court` (AotSealReceipt digest consume) | **ALIVE** (lineage tip-cargo) — exclusive quiet `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked` under `/tmp/ggen-uilayout-tip-alive-protected` → **11 passed; 0 failed; exit 0** @ `ca52e9c16` (harness sources identical through crown `678a6c106`). Prior evidence `eaf0dad40` (**8 passed; exit 0**); tip court land `7c0ba2a90`. Sibling `ui-layout-court.ts` remains out-of-repo / **UNSUPPORTED**. | crown `678a6c106` (cargo evidence `ca52e9c16`; absorb `8090022f4`) | Replay: `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked`; next: `pnpm test -- ui-layout-court` in zcode-cli | **ggen** (in-repo fence); **zcode-cli** (TS adapter, separate) |
| FM-PACK-001 `packs/gh-terraform-pack` | **PRESENT** — path on tip (`test -d packs/gh-terraform-pack` → **0**; tree `c7799cd6`); consumers retargeted. Crown `composed_packs_e2e` **SKIPPED** (loadavg ≥10) → not crown `ALIVE`. Inventory: [`REPO-INVENTORY-27.1.1.md`](./REPO-INVENTORY-27.1.1.md) | tip `120598fb3e395393d0ebe7fd86537bb499fc670d` (restore `5f28d0f90`) | When loadavg &lt;10: `timeout 600s cargo test -p ggen-engine --test composed_packs_e2e -- --nocapture` → exit **0** | **ggen** (pack presence); e2e for crown |
| zcode POWL/OCEL bounded remediation (in-repo stub) | **ALIVE** (bounded-refusal tip-cargo) — `zcode_powl_ocel_court` **6 passed; exit 0** + `remediation_bound` **4 passed; exit 0** @ tip `cd7c2433b` (`cursor/v2711-zcode-powl-refusal-alive-bc54`). Foundry `--dry-run` **PARTIAL** (exit **2**). Live `--live` **BLOCKED_HITL**. Ledger: [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) / [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) | tip `cd7c2433b7e124db6606008a92081d23aa3727ce` | Replay: `SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-target cargo test -p ggen-sjira --test zcode_powl_ocel_court`; `… remediation_bound --lib`. Live next: provider creds + `bun scripts/powl-foundry-loop.ts --live` → **0** | **ggen** (contract); **zcode-cli** (execution) |
| `curve25519-dalek` v4 / cgap-aoq docs | **PARTIAL_ALIVE** (lock + pin doc) — `Cargo.lock` resolves **4.1.3** and **5.0.0**; policy recorded in [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md). Policy court that refuses undocumented major still absent → not crown `ALIVE`. | tip `120598fb3e395393d0ebe7fd86537bb499fc670d` (lock observation; prior evidence `0478fde59`) | `rg -n 'name = "curve25519-dalek"' -A2 Cargo.lock` (expect 4.1.3 + 5.0.0); next ALIVE: Gate-1 policy court per pin standing §5 | **ggen** (+ cgap-aoq docs owner) |
| `verify_witness` WASM &lt; 250KB | **ALIVE** (exact tip-cargo; size court only) — `set -o pipefail; cargo test -p ggen-graph --test verify_witness_size_court --locked --target-dir /tmp/ggen-verify-witness-tip-ebf4 --config build.jobs=1 -- --test-threads=1` → **6 passed; EXIT=0** on tip `c91b22561` (`cursor/v2711-verify-witness-tipalive-bc54`; log `/tmp/cursor/verify-witness-tip-c91b-reconfirm.log`). Cold build EXIT=0 @ `cd7c2433b`. Ancestor **6 passed; exit 0** @ `0478fde59`. Production WASM ([`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md)) remains **UNSUPPORTED** — never production-artifact `ALIVE` from the size fence | tip `c91b22561` (exact tip-cargo) | Size replay: command above. Production: `cargo build --release --target wasm32-unknown-unknown -p verify_witness`; `wc -c verify_witness.wasm` → **&lt; 262144** | **ggen** (size court) + external (witness/affidavit production build) |
| R-01…R-07 resolution contract | **PARTIAL_ALIVE** (contract court source) — historical `r01_r07_resolution_court` **7 passed; exit 0** on ancestor `0478fde59`; crown source still **7** `#[test]`. **Tip-cargo reconfirm PENDING** on `678a6c106` — do **not** crown-row `ALIVE` without exit **0** on observed tip. Risk *bodies* remain **BLOCKED** — [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) | crown `678a6c106` (evidence `0478fde59`) | `cargo test -p ggen-graph --test r01_r07_resolution_court` | **ggen** (contract); risk-register owner (bodies) |
| Brand `.toml` rhythm schema + OEM stubs (Fourth Watch / CindyCat) | **ALIVE** (official tip-cargo) — `brand_toml_completeness_court` **5** + `brand_toml_rhythm_court` **12** → EXIT **0** @ evidence tip `c91b22561` (`/tmp/ggen-brand-toml-tip`; [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md); brand sources ≡ live crown). Hot-swap lineage **ALIVE** (`brand_toml_hotswap_court` **6** @ `26256ca9d` / fluid **6** @ `19c08a3b0`). `BrandRefusal::Incomplete` (TouchFloor/Gap/Grid) closed under official oxrocksdb path. Full DTCG color/type/CASTLE inventories Gate 2+. **Production theme** → **UNSUPPORTED** (`REFUSED_PRODUCTION`). **Commercial** CindyCat-first ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)). | tip-cargo evidence `c91b22561` (sources ≡ crown; land `cursor/v2711-brand-toml-tip-dee5`) | Replay: `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --target-dir /tmp/ggen-brand-toml-tip --config build.jobs=1 -- --test-threads=1` → EXIT **0**. Production refuse: `cargo test -p ggen-graph --lib layout::brand::production_theme_tests`. Next promote: [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md). Full inventory: OOR **G2-13** / **G2-12** | **ggen** (rhythm + completeness + hot-swap + REFUSED_PRODUCTION fence) + brand plane (token inventory Gate 2+) |
| Plane 5 front doors | **UNKNOWN** / out of DO | n/a | Gate 2–3 — not a Gate-1 falsifier; commercial open is CindyCat patronage before Fourth Watch cold cyber ([`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md)) | external |
| BRCE + WebAuthn AS-01 (`:prime256v1`) ungated-DO fence | **PARTIAL_ALIVE** (seat fence court) — historical `brce_as01_seat_court` **6 passed; exit 0** on evidence `0478fde59` (ancestor); tip source still **6** `#[test]`; tip-cargo PENDING. Crypto verify **UNSUPPORTED**; live BRCE **BLOCKED**; fence ≠ broker `ALIVE`. Standing [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) | tip `120598fb3e395393d0ebe7fd86537bb499fc670d` (evidence `0478fde59`) | Fence replay: `cargo test -p ggen-abb-sbb --test brce_as01_seat_court`. Crypto/live: BRCE **V7/V8/V9** + OOR **G2-3/G2-4/G2-5** | **ggen** (fence); BRCE/Plane-5 own verify+actuate |
| ggen_igniter clean-room / manufacture | **ALIVE** — tip dual-schema `fourth-watch.brand.toml` BrandTheme emit vs igniter fixture → `diff -rq` **0** + compare-subject `git diff --exit-code` → **0** (`/tmp/cursor/igniter-real-cleanroom.md`; `bc-4861c787`). Mount PRESENT; clean-tree checkpoint 0. Jobs/C4 residual **ALIVE** (admitted `--pack-dir sample-pack … --check` exit **0**; NIF **ALIVE**; bare `--check` **REFUSED_INVOCATION** antifalsifier — [`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md)). In-repo contract **PARTIAL_ALIVE** [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md); hop ledger [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) | tip `f1492ed55+` (hop-4 check-alive `cursor/v2711-hop4-mix-check-alive-bc54`; manufacture land `cursor/v2711-hop4-manufacture-bc54`) | **Replay:** BrandTheme emit tip + igniter fixture (canon path) → `git diff --exit-code` **0**. **Jobs/C4 replay:** admitted `--pack-dir test/fixtures/sample-pack … --check` → 0. Checkpoint: `cd /home/ubuntu/ggen_igniter && git diff --exit-code`. In-repo: `cargo test -p ggen-sjira --test igniter_cleanroom_court` | **ggen_igniter** (crown); **ggen** (dual-schema tip brand) |

### Command evidence log (this refresh)

Observed crown tip: `678a6c106ad9aa78f0d198146ef8021a388fd8a4` (ui_layout absorb `8090022f4`;
gate1-next tip-cargo `26256ca9d`; historical evidence `0478fde59` ancestor).
Historical exit-0 evidence tip: `0478fde59644cccd60c5a88d64d444ec292e6a3c` (≥ `a54b9803`; ancestor).
ui-layout ancestor ALIVE: `eaf0dad40287a3347280a541766b3b16f3978cf8`.
**Tip-cargo `ui_layout` ALIVE** (lineage): exclusive flock `/tmp/ggen-uilayout-tip-alive-protected`;
`CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked` → **11 passed; exit 0** @ `ca52e9c16` (harness sources identical through crown `678a6c106`).
Tip-cargo **ALIVE** (lineage) @ evidence `26256ca9d` for `shui` / `taffy` / brand rhythm / brand hot-swap
(`brand.rs` fix on `26256ca9d`; sources ≡ crown). `verify_witness` tip-cargo **ALIVE** (exact tip) @ `c91b22561`; BRCE / R-01 tip-cargo remain **PENDING** on crown.

| Subject | Command / binary | Result |
|---------|------------------|--------|
| `shui:` fence | `shui_public_prefix_court` tip-cargo (`/tmp/ggen-shui-prefix-target2`) | **ALIVE** (crown) — **4 passed; exit 0** @ crown `008a81d21` (`shui-prefix-alive.status`); lineage `26256ca9d`; ancestor `0478fde59` |
| `taffy_aot` | `taffy_aot_preflight_court` tip-cargo (`/tmp/gate1-taffy`) | **ALIVE** (exact tip) — **7 passed; exit 0** @ crown `19c08a3b0`; lineage `26256ca9d` |
| `brand.toml` rhythm | `brand_toml_rhythm_court` tip-cargo (`/tmp/ggen-brand-toml-tip`) | **ALIVE** (exact tip) — **12 passed; exit 0** @ evidence `c91b22561` (with completeness **5**); lineage `26256ca9d` (`toml::Table` fix) retained |
| brand.toml completeness | `brand_toml_completeness_court` tip-cargo (`/tmp/ggen-brand-toml-tip`) | **ALIVE** — **5 passed; exit 0** @ evidence tip `c91b22561` (with rhythm **12**; [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md)); prior BUILD_BROKEN OOM closed |
| brand.toml fluid / hot-swap | `brand_fluid_clamp_court` + `brand_toml_hotswap_court` tip-cargo (`/tmp/gate1-taffy`) | **ALIVE** — **6+6 passed; exit 0** @ crown `19c08a3b0`; production `promote_production_theme` → **UNSUPPORTED** / `REFUSED_PRODUCTION` |
| `verify_witness` size court | `verify_witness_size_court` tip-cargo (`/tmp/ggen-verify-witness-tip-ebf4`) | **ALIVE** (exact tip) — **6 passed; EXIT=0** @ `c91b22561`; cold @ `cd7c2433b`; ancestor `0478fde59`; production **UNSUPPORTED** |
| R-01…R-07 contract | tip-synced `r01_r07_resolution_court` harness | evidence `7 passed; 0 failed` → exit **0** @ `0478fde59`; tip source **7**; tip-cargo PENDING |
| BRCE AS-01 seat | `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` | evidence `6 passed; 0 failed` → exit **0** @ `0478fde59`; tip source **6**; tip-cargo PENDING |
| zcode POWL stub | `zcode_powl_ocel_court` + `remediation_bound` tip-cargo | **ALIVE** (bounded refusal) — **6+4 passed; exit 0** @ tip `cd7c2433b`; live `--live` **BLOCKED_HITL** |
| `ui-layout-court` | exclusive `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked` (`/tmp/ggen-uilayout-tip-alive-protected`) | **ALIVE** (lineage) **11 passed; 0 failed; exit 0** @ `ca52e9c16` (sources ≡ crown `678a6c106`); prior evidence **8 passed; exit 0** @ `eaf0dad40`; tip source **11** `#[test]` |
| `ui-layout-court.ts` (contract stub) | `docs/rfc/v27.1.1/fixtures/ui-layout-court.ts` | stub PRESENT; zcode-cli runner still **UNSUPPORTED** |
| `verify_witness.wasm` (production) | `find . \( -name 'verify_witness*' -o -name '*witness*.wasm' \)` | placeholder only → production **UNSUPPORTED** |
| `curve25519-dalek` | `rg -n 'name = "curve25519-dalek"' -A2 Cargo.lock` + pin standing | versions **4.1.3** + **5.0.0** present; pin doc → **PARTIAL_ALIVE**; policy court still open |
| FM-PACK-001 | `test -d packs/gh-terraform-pack` + inventory | **PRESENT** on tip; composed e2e SKIPPED |
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

ggen `ui_layout` tip-cargo **ALIVE** (lineage) `ca52e9c16`→crown `678a6c106`
(**11 passed; exit 0**; exclusive quiet cargo; absorb `8090022f4`). Shui/taffy/brand/hot-swap **ALIVE**
(lineage) @ `26256ca9d`. `verify_witness` tip-cargo **ALIVE** @ tip `c91b22561`; BRCE / R-01 crown tip-cargo **PENDING** (ancestor `0478fde59` exit **0**).
Honesty: Truex+shui **ALIVE** @ `68d2d233` (sibling merged); hop-4 manufacture
**ALIVE** (`diff=0`); hop-5 **PARTIAL_ALIVE** (adapter 0; `--live` open); FM-PACK-001 → **PRESENT**; production theme → **UNSUPPORTED**:

- Truex+shui → **ALIVE** (sibling merged @ `truex@68d2d233` / `b4672131`; not re-run here); ggen `shui:` fence → **ALIVE** (lineage tip-cargo **4** @ `26256ca9d`)
- brand rhythm + completeness → **ALIVE** (exact tip-cargo **12+5** @ `c91b22561`; [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md)); fluid+hot-swap → **ALIVE** (exact tip **6+6** @ `19c08a3b0`); production theme → **UNSUPPORTED** / `REFUSED_PRODUCTION` — [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md)
- Taffy AOT six-width seal → **ALIVE** (exact tip-cargo **7** @ `19c08a3b0`; photolithography consume still open)
- `verify_witness` size court → **ALIVE** (exact tip-cargo **6** @ `c91b22561`; production WASM **UNSUPPORTED**)
- BRCE AS-01 seat → **PARTIAL_ALIVE** (crypto / live BRCE still open; fence ≠ path `ALIVE`)
- R-01…R-07 contract → **PARTIAL_ALIVE** (bodies **BLOCKED**; crown tip-cargo **PENDING**)
- zcode POWL in-repo stub → **PARTIAL_ALIVE** (live CLI open; not in this exit set)
- FM-PACK-001 → **PRESENT** (composed e2e SKIPPED)
- Gate-0 cos → **BLOCKED** (external)
- zero-HITL fab / production WASM / production brand theme swap → **not** `ALIVE` (in-repo hot-swap fence is tip-cargo **ALIVE**; production theme **UNSUPPORTED**)

ggen in-repo `ui-layout-court` digest consume → **ALIVE** tip-cargo (**11 passed;
exit 0** @ `ca52e9c16`; sources ≡ crown `678a6c106`). Prior evidence `eaf0dad40`
(**8 passed; exit 0**; `bc-c7c9d554`). Tip court land `7c0ba2a90`.

Next **new** Gate-1 falsifier to author (pick one):

1. Quiet exclusive tip-cargo reconfirm on tip for `brce_as01_seat_court`, `r01_r07_resolution_court` (`verify_witness` + brand completeness/rhythm tip-cargo **ALIVE** @ `c91b22561`; shui/taffy/ui_layout lineage **ALIVE**). Gate1 aggregate remains **PARTIAL** (production theme **UNSUPPORTED**; BRCE fence ≠ broker `ALIVE`).
2. Hop-5 foundry `--live`: `bun scripts/powl-foundry-loop.ts --live` → exit **0** (adapter already 0; Jobs/C4 still open — [`HOP5-MIX-SUBJECT-STANDING.md`](./HOP5-MIX-SUBJECT-STANDING.md); [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md)).
3. Or advance sibling / docs rows still open: sync stale companions (`ECOSYSTEM-SPEC` §7–8 shui/hot-swap pins; [`GATE-1-REMAINING-COURTS-STANDING.md`](./GATE-1-REMAINING-COURTS-STANDING.md) `verify_witness` row); `curve25519-dalek` Gate-1 **policy court**; production `verify_witness.wasm` on affidavit plane.
