# FOUNDRY-FINISH-STANDING — Horizontal spine without daily founder HITL

| Field | Value |
|-------|-------|
| Document ID | `FOUNDRY-FINISH-STANDING` |
| Companion to | `ECOSYSTEM-SPEC-27.1.1` (target GA **2027-01-01**) |
| Status | STANDING LEDGER (not an execution receipt; not a Gate-1 rewrite) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `77eb46fd05d5bc847050a8f0a9147e1133e52c83` (foundry-finish honesty `cursor/v2711-foundry-finish-honesty-de6d` atop Gate1 BRCE+R01 exact tip-cargo `e7569893a` / Truex+shui `c5101d597+`; **no cargo**). See **Tip-pinned spine inventory** below. Zero-HITL crown **PARTIAL** — **not** crown `ALIVE`. Daily founder HITL **not** cleared. |
| Prior tip note | Was foundry-spine honesty pin @ `59c090984` (`97c613452+`; write_apply still **PENDING**; BRCE/R-01 tip-cargo **PENDING**). This retip absorbs tip evidence: r01+BRCE seat exact-tip **ALIVE** @ `e7569893a`; CCS `write_apply` **ALIVE** @ `cde02d1f2`; brand exact-tip **COMPILING**/lineage; OOR blockers held — without inventing zero-HITL `ALIVE`. |
| Base (main) | `origin/main` @ `d173cad5792e837bd4fec730d120c582b9bc5d4c` |
| Repo scope | `seanchatmangpt/ggen` names the spine; sibling planes are **tracked out-of-repo** |

Standing vocabulary (AGENTS.md / ECOSYSTEM-SPEC):
`UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority rule.** This document is the **source of truth for horizontal Foundry-finish spine standing**. It does **not** rewrite `GATE-1-GAP-MATRIX.md`. Gate-1 keeps requirement → evidence → verifier → owner rows; this ledger answers whether the full spine can run **without daily founder HITL**.

**Crown claim (Foundry finish).** Horizontal spine
`ggen → Taffy → zcode-cli → CASTLE → Smalt → revops` runs as an admitted, receipt-bound loop with **no ambient daily founder HITL**.

| Crown claim standing | Reason |
|----------------------|--------|
| **PARTIAL** | Zero-HITL crown remains **PARTIAL** on crown `77eb46fd0` (evidence tip `e7569893a`; lineage `c5101d597` / `59c090984` / `f1492ed55`). Tip-pinned spine: ggen+Taffy tip-cargo **ALIVE** (taffy **7** @ `19c08a3b0`; ui_layout **11** exact tip-cargo @ `a508e6bae`); igniter clean-room **ALIVE** EXIT:0 @ `f1492ed55` (`bc-53b0e099`); Hop4 mix admitted `--check` **ALIVE** EXIT:0 @ `97c613452` (`bc-54b308e2`); CASTLE/Smalt **ALIVE** (prior); zcode-refusal tip-cargo **ALIVE** @ `cd7c2433b` (Hop5 `--live` **BLOCKED_HITL**); CCS `write_apply` tip-cargo **ALIVE** **7+1+1** @ `cde02d1f2` + construct **ALIVE** @ `861a6e73a`; r01+BRCE seat exact-tip **ALIVE** @ `e7569893a`; brand exact-tip **COMPILING**/lineage @ `c91b22561`; revops **NXDOMAIN BLOCKED**; cos **503 BLOCKED**; BRCE **V8 BLOCKED**. Founder HITL still required for: **cos DB**, **WebAuthn live** (BRCE V8/V9), **foundry `--live`**. Clause-4 GA falsifiers in §1 would still fire today. **Do not invent Foundry crown `ALIVE`.** |

Do not promote source presence, fleet-census rows, workflow definitions, or historical jira prose to crown `ALIVE`. ggen+Taffy tip-cargo **ALIVE** ≠ Foundry crown `ALIVE`. CASTLE sibling **ALIVE** ≠ digest-bound spine crown. Smalt RAID / GA economic face **ALIVE** (4-cloud+FinOps+residual) ≠ zero-HITL / Foundry crown `ALIVE`. zcode adapter exit 0 ≠ foundry `--live` `ALIVE`. Igniter clean-room EXIT:0 ≠ manufacture / zero-HITL crown `ALIVE`.

### Tip-pinned spine inventory (crown `77eb46fd0` / evidence `e7569893a`)

| Component | Standing | Evidence pin |
|-----------|----------|--------------|
| **ggen** + **Taffy AOT** | tip-cargo **ALIVE** | taffy **7** @ `19c08a3b0` / absorb `cde02d1f2`; ui_layout **11** exact tip-cargo @ `a508e6bae`; shui lineage **4** @ `008a81d21`; `verify_witness` **6** @ `c91b22561`; fluid+hotswap **6+6** @ `19c08a3b0` |
| **igniter clean-room** | **ALIVE** EXIT:**0** | `git diff --exit-code` @ ggen_igniter on tip lineage `f1492ed55` — agent `bc-53b0e099` (`/tmp/cursor/igniter-cleanroom-alive-tip.md`) |
| **Hop4 mix** admitted `--check` | **ALIVE** EXIT:**0** | Jobs/C4 admitted `--pack-dir sample-pack … --check` @ `97c613452` — agent `bc-54b308e2` ([`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md)) |
| **CASTLE** / **Smalt** | **ALIVE** (prior) | castle `2774039ea`; smalt `91997da76f0f` (4-cloud + FinOps + residual b4b exits **0,0,0,0**) — ≠ Foundry crown |
| **zcode-cli** | Hop5 `--live` **BLOCKED_HITL**; in-repo bounded-refusal tip-cargo **ALIVE** | foundry `--live` **foundry_exit=1** / `Model access is not configured`; tip-cargo sjira **6**/bound **4** @ `cd7c2433b` — do **not** invent hop-5 crown `ALIVE` from refuse-path alone |
| **CCS** | tip-cargo **ALIVE** (subset) | `write_apply` **7+1+1** @ `cde02d1f2` / evidence `42c5b9ba4`; construct_hash **4** @ `861a6e73a`; lifecycle **6** @ `acd9705c1` — ≠ Plane-5 DO ([`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md)) |
| **r01** + **BRCE seat** | exact-tip **ALIVE** @ tip | `r01_r07_resolution_court` **7** + `brce_as01_seat_court` **6** EXIT:0 @ `e7569893a` (`/tmp/ggen-gate1-brce-r01-target`); bodies **BLOCKED**; fence ≠ broker |
| **brand** rhythm/completeness | exact-tip **COMPILING** / lineage **ALIVE** | lineage **5+12** @ `c91b22561`; **no exact tip EXIT:0** on live crown — do **not** invent tip-subject `ALIVE` ([`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md)) |
| **revops** | **NXDOMAIN BLOCKED** | `getent hosts revops.fnox.io` fail / `host` **NXDOMAIN** (OOR held) |
| **cos** | **503 BLOCKED** | `/readyz` **HTTP 503** `DB_UNAVAILABLE` (OOR held) |
| **BRCE V8** | **BLOCKED** | broker AS-01 seat not live; seat fence **ALIVE** ≠ broker ([`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5) |

**Exact next verifiers (HITL / BLOCKED residuals — not cargo):**

```text
# cos readiness (Plane-5 adjacency)
curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz   # expect HTTP 200

# revops front door
getent hosts revops.fnox.io
curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz   # DNS ∧ HTTP 200

# Hop5 foundry --live (BLOCKED_HITL until provider creds)
# provider /login or ~/.zcode/v2/provider_config.json →
bun scripts/powl-foundry-loop.ts --live   # expect exit 0

# BRCE V8 broker then V9 Plane-5 (see BRCE-WEBAUTHN-STANDING §5)
# brand exact-tip reconfirm (COMPILING until EXIT:0):
#   cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court
```

---

## 1. Horizontal spine table

| Component | Role on spine | Standing | HITL residual (founder) | Next exact verifier | GA `2027-01-01` falsifier |
|-----------|---------------|----------|-------------------------|---------------------|---------------------------|
| **ggen** | Manufacture kernel (`μ`): packs, graphs, layout pre-flight, brand rhythm, public-prefix fences, witness size court, R-01…R-07 contract, ui-layout digest consume, `ggen-layout-court` CLI | **ALIVE** (tip-cargo / lineage fences) + **PRESENT** (spine role) — crown `77eb46fd0`; taffy/fluid/hotswap **ALIVE** @ `19c08a3b0`/`cde02d1f2`; ui-layout exact tip-cargo **ALIVE** (**11** @ `a508e6bae`); shui lineage **ALIVE** (**4** @ `008a81d21`); `verify_witness` **ALIVE** (**6** @ `c91b22561`); r01+BRCE seat exact-tip **ALIVE** @ `e7569893a`; brand rhythm/completeness exact-tip **COMPILING**/lineage @ `c91b22561`; CCS `write_apply` **ALIVE** @ `cde02d1f2`. Zero-HITL fab **not** sealed — ≠ Foundry crown `ALIVE` | Founder still gates brand exact-tip EXIT:0 + Gate-1→GA promote; R-01…R-07 bodies **BLOCKED** out-of-repo | Exact next (remaining Gate-1): brand exact-tip reconfirm → exit **0**; OOR/HITL: cos `/readyz` **200**, foundry `--live` → **0**, BRCE **V8** ([`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md)) | If on **2027-01-01** a founder must still manually admit ordinary spine manufacture (SELECT/CONSTRUCT) that courts already cover, crown claim is **falsified** |
| **Taffy** | AOT layout pre-flight (widths `320/375/414/768/1024/1440` + WCAG 1.4.12 @ 1.5×) inside `ggen-graph::layout::taffy_aot` | **ALIVE** (exact tip-cargo) — **7 passed; exit 0** @ crown `19c08a3b0`; lineage `26256ca9d`; digest consume fence **ALIVE (exact tip-cargo)** (`ui_layout` @ `a508e6bae`) | Founder still retries consumer bind when receipt law drifts | Replay: `CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked` | If on **2027-01-01** geometry law still requires founder-edited exceptions instead of typed `LayoutRefusal` / seal receipts, Taffy row is **falsified** |
| **zcode-cli** | Consume ggen layout court via sibling `UiLayoutCourtAdapter`; POWL/OCEL bounded remediations | **PARTIAL** — in-repo bounded-refusal tip-cargo **ALIVE** (sjira **6**/bound **4** @ `cd7c2433b`; [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md)); in-process bun **ALIVE** (**35 passed; exit 0**); `ggen-layout-court` **PRESENT**; adapter exit **0** (mix-SUBJECT); Gall `--live` @ `f32df08` injects `createZcodeAgentExecutor`; Linux tip-matched runtime **PRESENT**; Hop5 `--live` **BLOCKED_HITL** / **foundry_exit=1** / **REFUSED** (`Model access is not configured`). Do **not** invent hop-5 / Foundry crown `ALIVE` | Founder must provision provider creds before hop-5 exits 0 | Exact next (**BLOCKED_HITL** foundry `--live` until creds): provider `/login` or `~/.zcode/v2/provider_config.json` → `bun scripts/powl-foundry-loop.ts --live` → **0**. Receipt: `/tmp/cursor/hop5-zcode-runtime-receipt.md`; ledger [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md); bounded court [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) | If on **2027-01-01** UI courts still need founder to paste/override layout digests or remediations run unbounded, zcode row is **falsified** |
| **CASTLE** | Consequence / publish plane (`CONSEQUENTIAL_ADMISSIBILITY`); Gate 2–3 public mint | **ALIVE** (sibling) @ castle `2774039ea` + digest-bind wire **WIRED**/ALIVE @ `cursor/digest-bind-admit-ca8a` `e0d99c6` (`publish_digest_bind_court` **6/6** exit **0**; CLI `--ggen-layout-digest`; `digest_bind.crown_wire=WIRED`). Spine wire **PARTIAL_ALIVE**. Foundry/zero-HITL crown still **PARTIAL**/**UNKNOWN** — wire != Foundry crown `ALIVE` ([`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §13). In-ggen publish **UNSUPPORTED**. Do **not** promote Foundry crown | Founder still needed for LinkedIn AND-gate (cos readiness) + Foundry HITL residuals | Exact next (orthogonal): LinkedIn AND-gate on cos `/readyz` **200** AND 2x durable LOW; Foundry `--live` / WebAuthn production remain open | If on **2027-01-01** CASTLE publish still depends on daily founder token edits with no receipt, CASTLE row is **falsified** |
| **Smalt** | 4-cloud RAID / economic-ops substrate (Azure·AWS·GCP·Twilio/fnox) | **ALIVE** @ smalt `91997da76f0f` (parent `875efbb87`) — doctor/aws/gcp/azure/twilio exits **0,0,0,0,0** @ `875efbb87`; FinOps **ALIVE** (`mix test test/fin_ops_calculator_test.exs` → exit **0**, **19 passed**). Smalt RAID / GA economic face **ALIVE** (residual b4b treasury/tax/erp/rates exits **0,0,0,0** @ `91997da76f0f` — 8/8/8/7 passed; `bc-c700dafb`) — ≠ Foundry / zero-HITL crown | Residual closed; Foundry crown still separate | Replay residual: [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) §4.3/§6b @ `91997da76f0f`. Replay FinOps + §4.1 4-cloud | If on **2027-01-01** Smalt RAID still requires daily founder HITL to keep the membrane green, Smalt row is **falsified** |
| **revops** | Plane-5 front door (`revops.fnox.io`) + cos adjacency — sole DO via **BRCE + WebAuthn AS-01** | **BLOCKED** — `revops.fnox.io` **NXDOMAIN** (**BLOCKED**); cos `/readyz` **HTTP 503** `DB_UNAVAILABLE`. In-repo BRCE AS-01 fence **PARTIAL**; live BRCE **V8** **BLOCKED**; WebAuthn production **BLOCKED**/**UNSUPPORTED** | Founder HITL: **cos DB** restore + **WebAuthn live** (V8/V9) before Plane-5 DO | Exact next (**BLOCKED**): `getent hosts revops.fnox.io`; `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** (not `DB_UNAVAILABLE`); then BRCE **V8** broker + **V9** Plane-5 court ([`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5). Fence-only: `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` | If on **2027-01-01** Plane-5 still depends on daily founder HITL for ordinary DO, revops row (and crown claim) is **falsified** |

### 1a. Zero-HITL crown + exact next for BLOCKED/PENDING

| Crown / residual | Standing | Exact next verifier |
|------------------|----------|---------------------|
| **Zero-HITL Foundry crown** | **PARTIAL** | Promote only when §6 closure holds; today founder HITL still required for cos DB, WebAuthn live, foundry `--live` |
| tip-cargo (`ui_layout` + Taffy AOT + CCS) | **ALIVE** | taffy **7** @ `19c08a3b0`; ui_layout **11** exact tip-cargo @ `a508e6bae`; shui lineage **4** @ `008a81d21`; `verify_witness` **6** @ `c91b22561`; `write_apply` **7+1+1** @ `cde02d1f2`; r01+BRCE seat exact-tip **ALIVE** @ `e7569893a`; brand exact-tip **COMPILING**/lineage |
| igniter clean-room | **ALIVE** EXIT:**0** @ `f1492ed55` | `cd …/ggen_igniter && git diff --exit-code` → **0** (`bc-53b0e099`); ≠ manufacture / Foundry crown |
| Hop4 mix admitted `--check` | **ALIVE** EXIT:**0** @ `97c613452` | admitted `--pack-dir sample-pack … --check` → **0** (`bc-54b308e2`); bare `--check` **REFUSED_INVOCATION** antifalsifier |
| foundry `--live` (hop-5) | **BLOCKED_HITL** / **REFUSED** | **foundry_exit=1** / `Model access is not configured` @ zcode `f32df08`+; in-repo bounded-refusal tip-cargo **ALIVE** @ `cd7c2433b`; next: provider `/login` or `~/.zcode/v2/provider_config.json` → `bun scripts/powl-foundry-loop.ts --live` → **0** |
| `write_apply` tip-cargo | **ALIVE** | tip-cargo **7+1+1** exit **0** @ `cde02d1f2` (evidence `42c5b9ba4`); ≠ Plane-5 DO |
| brand rhythm/completeness | exact-tip **COMPILING** / lineage **ALIVE** | lineage **5+12** @ `c91b22561`; exact-tip: `cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court` → **0** |
| cos DB / `/readyz` | **503 BLOCKED** | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** |
| WebAuthn live / BRCE V8 | **BLOCKED** (seat fence exact-tip **ALIVE**) | Seat **6** @ `e7569893a` ≠ broker; BRCE **V8** then **V9** per [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5 + OOR G2-3/G2-5 |
| revops.fnox.io DNS | **NXDOMAIN BLOCKED** | `getent hosts revops.fnox.io`; `curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz` → DNS ∧ HTTP **200** |

---

## 1b. Adjacent Gate-1 / authority fences (not spine members; block zero-HITL)

| Subject | Standing on crown `77eb46fd0` (evidence `e7569893a`; lineage `c5101d597` / `59c090984` / `cde02d1f2` / `c91b22561`) | Next exact verifier |
|---------|-----------------------------|---------------------|
| BRCE + WebAuthn AS-01 ungated-DO refusal ([#861](https://github.com/seanchatmangpt/ggen/pull/861)) | Seat fence exact-tip **ALIVE** (**6 passed; exit 0** @ `e7569893a`); crypto verify **UNSUPPORTED**; live BRCE **V8 BLOCKED** — fence ≠ broker | Replay: `cargo test -p ggen-abb-sbb --test brce_as01_seat_court`; next live: BRCE **V8** |
| `verify_witness` WASM &lt;250KB size court | **ALIVE** (tip-cargo **6 passed; exit 0** @ `c91b22561`); production WASM [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) **UNSUPPORTED** | `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-verify-witness-target cargo test -p ggen-graph --test verify_witness_size_court --locked` |
| `shui:` public-prefix fence | **ALIVE** (lineage tip-cargo) — **4 passed; exit 0** @ `008a81d21`; Truex product admit-court **ALIVE** @ `68d2d233` (fence ≠ Truex crown alone) | `CARGO_TARGET_DIR=/tmp/ggen-shui-prefix-target2 cargo test -p ggen-graph --test shui_public_prefix_court --locked` |
| R-01…R-07 resolution contract | exact-tip **ALIVE** (**7 passed; exit 0** @ `e7569893a`); risk **bodies** **BLOCKED** [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) / [`R01-R07-TIP-CARGO-STANDING.md`](./R01-R07-TIP-CARGO-STANDING.md) | Replay: `cargo test -p ggen-graph --test r01_r07_resolution_court` |
| Brand `.toml` rhythm + hot-swap | fluid+hot-swap tip-cargo **ALIVE** (**6+6** @ `19c08a3b0`); rhythm+completeness exact-tip **COMPILING** / lineage **ALIVE** (**5+12** @ `c91b22561`); **production theme UNSUPPORTED** (`REFUSED_PRODUCTION`); full DTCG/CASTLE inventory Gate 2+ | Exact-tip brand reconfirm per [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md); production refuse: `cargo test -p ggen-graph --lib layout::brand::production_theme_tests` |
| ggen `ui-layout-court` / `layout::ui_layout` / LayoutRefusal / fluid-clamp | **ALIVE (exact tip-cargo)** **11 passed; exit 0** @ `a508e6bae` (`bc-70188ac3`; prior lineage `59c090984` / `ca52e9c16` / `eaf0dad40`); tip source **11** `#[test]` | Replay: `CXX=g++ SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court --locked --target-dir /tmp/ggen-uilayout-exact-tip --config build.jobs=1 -- --test-threads=1` |
| `ggen-layout-court` bin + sibling live adapter | Package **PRESENT** @ `d37271961`; in-process bun **35/exit 0**; live adapter exit **0** on tip evidence; prebuilt `layout_court` on PATH exit **0** (no cargo under load&gt;8) | Replay: `layout_court fixtures/ui-layout-court/product-subject.json` → 0; `UiLayoutCourtAdapter.verifyPostTransition` → 0 |
| ggen_igniter clean-room / manufacture | Clean-room **ALIVE** EXIT:**0** @ `f1492ed55` (`bc-53b0e099`); BrandTheme manufacture compare **ALIVE** (`diff_exit=0`); Jobs/C4 admitted `--check` **ALIVE** EXIT:**0** @ `97c613452` (`bc-54b308e2`) [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md); hop ledger [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) / [`HOP4-MIX-NIF-STANDING.md`](./HOP4-MIX-NIF-STANDING.md) | Replay: `git diff --exit-code` → 0; manufacture compare → 0; Jobs/C4 admitted `--pack-dir sample-pack … --check` → 0; in-repo: `cargo test -p ggen-sjira --test igniter_cleanroom_court` |
| FM-PACK-001 `packs/gh-terraform-pack` | **ALIVE** on tip (lineage tip-cargo `composed_packs_e2e` **2 passed; exit 0** @ evidence `19c08a3b0` / court tip `a508e6bae`; sources ≡ absorb) — [`FM-PACK-001-ALIVE-STANDING.md`](./FM-PACK-001-ALIVE-STANDING.md) / [`REPO-INVENTORY-27.1.1.md`](./REPO-INVENTORY-27.1.1.md) | Replay: `CARGO_BUILD_JOBS=1 cargo test -p ggen-engine --test composed_packs_e2e --target-dir /tmp/ggen-fm-pack-001-alive-target --config build.jobs=1 -- --nocapture` → 0 |
| cos (Gate-0 / Plane-5 adjacency) | **BLOCKED** — cos `/readyz` **HTTP 503** `DB_UNAVAILABLE`; none in ggen | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** |
| Live zcode POWL/OCEL / foundry | Hop5 `--live` **BLOCKED_HITL** — adapter exit **0**; runtime **PRESENT**; **foundry_exit=1** / `Model access is not configured`; in-repo bounded-refusal tip-cargo **ALIVE** @ `cd7c2433b`; aggregate **PARTIAL** — not Foundry crown `ALIVE` | Provision provider creds then `bun scripts/powl-foundry-loop.ts --live` → 0; receipt `/tmp/cursor/hop5-zcode-runtime-receipt.md` |
| CCS `write_apply` / authority matrix | Ledger **MATERIALIZED** [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md); `write_apply` tip-cargo **ALIVE** **7+1+1** @ `cde02d1f2`; construct **ALIVE** @ `861a6e73a`; Plane-5 DO still not ALIVE | Replay write_apply courts → **0**; do not promote Plane-5 from tip-cargo |

---

## 2. HITL residual summary (why daily founder is still required)

| Residual class | Where it bites | Clearance condition |
|----------------|----------------|---------------------|
| tip-cargo fences ALIVE | taffy/`ui_layout`(exact)/shui/verify_witness/write_apply/r01/BRCE-seat tip-cargo **ALIVE**; brand exact tip-cargo **ALIVE** @ `77eb46fd0` | hop-5 foundry `--live` / cos / BRCE V8 (ui_layout sealed) |
| foundry `--live` | hop-5 **BLOCKED_HITL**; runtime **PRESENT**; **foundry_exit=1** / `Model access is not configured`; bounded-refusal tip-cargo **ALIVE** @ `cd7c2433b` | provider creds → `bun scripts/powl-foundry-loop.ts --live` → **0** |
| cos DB | cos `/readyz` **503** `DB_UNAVAILABLE` | `curl … https://cos.fnox.io/readyz` → HTTP **200** |
| WebAuthn live / BRCE V8 | Seat fence exact-tip **ALIVE** @ `e7569893a`; broker **BLOCKED**; production AS-01 ceremony open | BRCE **V8** then **V9** ([`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) §5) |
| mix sync NIF / Jobs/C4 | **ALIVE** EXIT:**0** @ `97c613452` (`bc-54b308e2`; bare `--check` antifalsifier held) | Replay pack/template `--check` → **0** |
| `write_apply` tip-cargo | **ALIVE** **7+1+1** @ `cde02d1f2` | Replay write_apply courts → **0**; ≠ Plane-5 DO |
| GA risk register | R-01 … R-07 bodies (contract fence exact-tip **ALIVE**) | Typed receipt or typed refusal per risk before GA |

Foundry finish is **not** “all crates compile.” It is **zero daily founder HITL** on the horizontal spine while preserving `SELECT ≠ CONSTRUCT ≠ DO`. Zero-HITL crown today: **PARTIAL**.

---

---

## 3. Next aggregate verifier (this ledger)

Replay cheapest high-information path first (all against crown `77eb46fd0` / evidence `e7569893a`; tip-cargo **ALIVE** retained for taffy/`ui_layout`(exact @ `a508e6bae`)/shui/verify_witness/write_apply/r01/BRCE-seat; brand exact tip-cargo **ALIVE** @ `77eb46fd0`):

```text
# Gate-1 in-repo fences on #849 tip (exit reconfirm)
cargo test -p ggen-graph --test shui_public_prefix_court
cargo test -p ggen-graph --test taffy_aot_preflight_court
cargo test -p ggen-graph --test ui_layout_court
cargo test -p ggen-graph --test brand_toml_completeness_court
cargo test -p ggen-graph --test brand_toml_rhythm_court
cargo test -p ggen-graph --test verify_witness_size_court
cargo test -p ggen-graph --test r01_r07_resolution_court
cargo test -p ggen-abb-sbb --test brce_as01_seat_court
cargo test -p ggen-sjira --test zcode_powl_ocel_court
cargo test -p ggen-sjira --test igniter_cleanroom_court

# This ledger’s presence / cross-link integrity (always)
test -f docs/rfc/v27.1.1/FOUNDRY-FINISH-STANDING.md
rg -n 'GA `2027-01-01` falsifier|HITL residual|PARTIAL_ALIVE' docs/rfc/v27.1.1/FOUNDRY-FINISH-STANDING.md

# ggen ui-layout fence PRESENT (must succeed)
test -f crates/ggen-graph/tests/ui_layout_court.rs
test -f crates/ggen-graph/src/layout/ui_layout.rs

# ggen-layout-court PRESENT on tip (must succeed after d37271961)
test -f crates/ggen-layout-court/src/bin/layout_court.rs
test -f fixtures/ui-layout-court/product-subject.json
rg -n 'ggen-layout-court' Cargo.toml

# Exact next live hop-5 (do not invent ALIVE until exits 0)
cargo run -p ggen-layout-court --bin layout_court -- fixtures/ui-layout-court/product-subject.json
```

Out-of-repo next verifiers remain owned by sibling ledgers; ggen must **track** them without claiming their ALIVE standing. Consolidated Gate 0/2/3 index: [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md).

---

## 4. Cross-links (companions + swarm standing ledgers)

### 4.1 On PR #849 tip `d05a131c7521d5a6325ade2e8295fee650659d5a` (foundry standing retip; layout_court land `d37271961`)

| Path | Role vs this ledger |
|------|---------------------|
| [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) | Requirements / architecture binding; defines GA **2027-01-01** and Gate schedule |
| [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md) | Gate-1 requirement matrix — **not rewritten here**; Foundry finish is horizontal spine source |
| [`REPO-INVENTORY-27.1.1.md`](./REPO-INVENTORY-27.1.1.md) | Path census + component standing seeds for Taffy / zcode / CASTLE / revops |
| [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) | Taffy → igniter → zcode consume path |
| [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) | CASTLE publish / consequential admissibility |
| [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) | Smalt 4-cloud RAID / economic ops |
| [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) | revops + cos Plane-5 |
| [`GATE-0-STANDING.md`](./GATE-0-STANDING.md) | Gate 0 billing / cos / servengine / SOW |
| [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) | Gate 0/2/3 out-of-repo standing + next verifier commands through GA |
| [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md) | Seven Gate-3 release courts (D1–D7) |
| [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) / [`BRCE-AS01-SEAT-TIP-CARGO-STANDING.md`](./BRCE-AS01-SEAT-TIP-CARGO-STANDING.md) | DO authority edge (AS-01 `:prime256v1`) — seat fence exact-tip **ALIVE** @ `e7569893a`; live **V8** **BLOCKED** |
| [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) / [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) | zcode-cli POWL·OCEL bounded refusal — tip-cargo **ALIVE** (sjira **6**/bound **4** @ `cd7c2433b`); `--live` **BLOCKED_HITL** |
| [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) | Founder HITL `--live` exact next verifier pin (OOR **G2-9**) — **BLOCKED_HITL** until `bun scripts/powl-foundry-loop.ts --live` → **0**; **not** hop-5 / Foundry `ALIVE` |
| [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md) | CCS authority matrix — `write_apply` tip-cargo **ALIVE** @ `cde02d1f2`; Plane-5 DO **BLOCKED** |
| [`R01-R07-TIP-CARGO-STANDING.md`](./R01-R07-TIP-CARGO-STANDING.md) / [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md) | r01 exact-tip **ALIVE** @ `e7569893a`; brand exact-tip **COMPILING**/lineage @ `c91b22561` |
| [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) | igniter clean-room contract; sibling crown **BLOCKED** |
| [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) | Production witness ownership (out of ggen) |
| [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) | R-01…R-07 resolution contract |

If a sibling ledger disagrees with the spine table in §1, **do not silently merge standings**. Record the conflict as `UNKNOWN` / contradictory evidence until one court wins.

### 4.2 Related in-repo doctrine (not spine ALIVE)

| Path | Note |
|------|------|
| `AGENTS.md` | Evidence vocabulary + BRCE-only DO |
| `docs/architecture-foundry/REPOSITORY_ROLES.md` | ggen vs corpus ownership |
| `docs/architecture/CI-CD-ARD-PRD-v26.7.31.md` | CI as CONSTRUCT; production DO behind BRCE |
| `docs/gh-terraform/FLEET-CENSUS.md` | Names sibling repos; never ALIVE by itself |
| `docs/rfc/v26.9.26/abb-sbb-implementation.md` | ABB/SBB kernel standing (orthogonal slice) |

---

## 5. Explicit non-claims

- Gate-0 (GCP billing, servengine M4, cos ledger, year-end SOW) is **OUT OF SCOPE** for ggen ALIVE claims.
- Zero-DOM / WebGPU / FFB renderers are **UNSUPPORTED** inside ggen.
- Truex product admission of `shui:` is **ALIVE** @ `truex@68d2d233` (sibling); ggen `shui:` fence **ALIVE (lineage tip-cargo)** @ `008a81d21` ≠ Truex substitute.
- Affidavit pack manufacture / placeholder size court PARTIAL ≠ production `verify_witness` WASM &lt;250KB ALIVE.
- BRCE AS-01 seat fence exact-tip **ALIVE** @ `e7569893a` ≠ live BRCE broker **V8** ALIVE; Ed25519 SovereignLease ≠ WebAuthn `:prime256v1`.
- ggen `ui_layout_court` **ALIVE (exact tip-cargo)** (**11** @ `a508e6bae`; lineage `59c090984` / `ca52e9c16` / `eaf0dad40`) ≠ live sibling `ui-layout-court.ts` ALIVE; ≠ Foundry path crown ALIVE. CASTLE/Smalt sibling **ALIVE** ≠ Foundry crown.
- zcode bounded-refusal tip-cargo **ALIVE** @ `cd7c2433b` ≠ hop-5 `--live` / Foundry crown `ALIVE`.
- Brand exact-tip **COMPILING**/lineage ≠ tip-subject brand `ALIVE`; CCS `write_apply` **ALIVE** ≠ Plane-5 DO.
- This ledger’s existence is **PARTIAL_ALIVE** documentation standing only — not Foundry-finish crown `ALIVE`.

---

## 6. Closure rule for crown ALIVE

Promote Foundry-finish crown claim to `ALIVE` only when **all** hold:

1. Gate-1 aggregate rows that own ggen fences are `ALIVE` (or typed `REFUSED_*` with remediation closed) per `GATE-1-GAP-MATRIX.md` — including `ui-layout` exact tip-cargo **ALIVE** @ `a508e6bae`, taffy/CCS/r01/BRCE-seat tip-cargo **ALIVE**, and brand exact-tip EXIT **0** (today still **COMPILING**/lineage).
2. Every §1 spine component is either `ALIVE` or has an out-of-repo tracked standing doc with an exact next verifier and a dated receipt tip.
3. Daily founder HITL is no longer required for ordinary spine SELECT/CONSTRUCT; Plane-5 DO is BRCE+AS-01 only (live, not doctrine-only).
4. GA date **2027-01-01** falsifiers in §1 would not fire if evaluated that day.

Until then, overall goal standing remains:

```text
FOUNDRY_FINISH = PARTIAL   # Zero-HITL crown PARTIAL — daily founder HITL not cleared
  ∧ ggen+Taffy tip-cargo ALIVE (taffy @ 19c08a3b0; ui_layout exact tip-cargo @ a508e6bae)
  ∧ zcode-refusal tip-cargo ALIVE @ cd7c2433b  # Hop5 --live BLOCKED_HITL ≠ crown
  ∧ CASTLE sibling ALIVE @ 2774039ea  # ≠ Foundry crown
  ∧ Smalt ALIVE @ 91997da76f0f (4-cloud + FinOps)  # ≠ Foundry crown
  ∧ CCS write_apply ALIVE @ cde02d1f2 + construct ALIVE @ 861a6e73a  # ≠ Plane-5 DO
  ∧ r01+BRCE seat exact-tip ALIVE @ e7569893a  # bodies BLOCKED; V8 BLOCKED
  ∧ brand exact-tip COMPILING / lineage ALIVE @ c91b22561
  ∧ igniter clean-room ALIVE EXIT:0 @ f1492ed55 (bc-53b0e099)
  ∧ Hop4 mix admitted --check ALIVE EXIT:0 @ 97c613452 (bc-54b308e2)
  ∧ revops NXDOMAIN BLOCKED ∧ cos 503 BLOCKED
  ∧ BRCE V8 BLOCKED ∧ foundry --live BLOCKED_HITL
  ∧ founder HITL still required: cos DB, WebAuthn live, foundry --live
  ∧ clause-4 GA falsifiers would still fire if evaluated today
⇒ crown claim incomplete (zero daily founder HITL not ALIVE)
```

---

## 7. Provenance

| Event | Note |
|-------|------|
| Materialized | 2026-11-22 — branch `cursor/v2711-foundry-finish-bc54` |
| Refresh | 2026-11-22 — branch `cursor/v2711-foundry-finish-refresh-bc54` against tip `e5ff3f822` |
| Ops refresh | 2026-11-22 — branch `cursor/v2711-ops-standing-refresh-bc54` against tip `7bf148d82` |
| Foundry+photo refresh | 2026-11-22 — branch `cursor/foundry-photo-standing-8a81` against tip `c5c6e28aa` (ui-layout land `479fc83a9` / pin `cbfd94582`) |
| Ui-layout ALIVE pin | 2026-11-22 — branch `cursor/v2711-uilayout-alive-ff-bc54` evidence `eaf0dad40` (**8 passed; exit 0**; `bc-c7c9d554`); tip court land `7c0ba2a90`; tip-sync pending |
| Foundry retip | 2026-11-22 — branch `cursor/v2711-foundry-retip-bc54` Observed tip → `5a39ea90a` (via absorb `0ae8d8108`); crown remains **PARTIAL_ALIVE**; ui-layout **ALIVE** @ `eaf0dad40` exit 0; tip-sync pending `/tmp/ggen-uilayout-0ae8` |
| Gate1 retip | 2026-11-22 — branch `cursor/v2711-gate1-retip-bc54` Observed tip → `5e00619a422a0d8e451029f0aaf8b7a63e27da46`; crown remains **PARTIAL_ALIVE**; ui-layout **ALIVE** @ `eaf0dad40` exit 0 (ancestor); tip source **11** `#[test]`; **tip-cargo PENDING** quiet cgroup — not tip exit 0; no new ALIVE invented |
| Evidence | Evidence tip `eaf0dad40`; ggen `ui_layout_court` tip-cargo **ALIVE** (**11 passed; exit 0** @ `ca52e9c16` ≡ tip `120598fb3`); tip court land `7c0ba2a90`; tip source **11**; sibling `ui-layout-court.ts` still ABSENT in ggen; CASTLE sibling courts **ALIVE** @ `2774039ea` (`bc-1bffb072`) — crown wire **UNKNOWN**; Smalt 4-cloud **ALIVE** @ `91997da76f0f` (parent `875efbb87`; FinOps **ALIVE** exit 0 / 19 passed); revops DNS fail + cos `/readyz` 503 → **UNKNOWN**/**BLOCKED**; igniter manufacture **ALIVE** (NIF residual **BLOCKED**); no Plane-5 / revops / zero-HITL / Foundry crown claim asserted as `ALIVE` |
| Hop5 live after #884 | 2026-11-22 — `cursor/v2711-hop5-live-bc54` tip `704025461`; `layout_exit=0`; default `adapter_exit=2` (`mix` SUBJECT_MISSING); foundry `--live` foundry_exit=1; hop-5 **PARTIAL** (demoted false PARTIAL_ALIVE/adapter ALIVE); receipt `/tmp/cursor/hop5-live-after-884.md` |
| Hop5 cross-mount | 2026-11-22 — branch `cursor/v2711-hop5-standing-bc54` Observed tip `f6cb18f86` (evidence tip `07b689c97` / ancestor `c9e224d33`); hop-5 **PARTIAL** (in-process bun **ALIVE** 35/exit 0) + live **BLOCKED** (`ggen-layout-court` ABSENT; adapter exit 2; foundry live exit 1); receipt `/tmp/cursor/hop5-crossmount.md`; no live/path-crown `ALIVE` |
| CASTLE sibling ALIVE | 2026-11-22 — branch `cursor/v2711-castle-alive-bc54` against tip `d0e3ba473`; castle tip `2774039ea` via `bc-1bffb072` — `publish_redaction_court` **13/13**, `cli_publish_brand` **6/6**, spine-probe `redaction_court=ALIVE`, exits **0,0,0**; Foundry crown remains **PARTIAL_ALIVE**; zero-HITL **not** `ALIVE` |
| Smalt PARTIAL_ALIVE | 2026-11-22 — branch `cursor/v2711-smalt-partial-bc54` against tip `8a68da481` (post-`5480d7ada`); smalt tip `875efbb87` via `bc-ad9ebffa`/`bc-1e876087` — RAID columns **ALIVE** (doctor/aws/gcp/azure/twilio exits **0,0,0,0,0**); fin_ops **PARTIAL_ALIVE** (burn-path exit 0; canonical ABSENT — restore still next); Foundry crown remains **PARTIAL_ALIVE**; full RAID/`ALIVE` **not** invented |
| Foundry honesty retip | 2026-11-22 — branch `cursor/v2711-foundry-honesty-bc54` Observed tip → `4b22529df2939412c41107fe5e9eea66ba6a4dad`; corrects stale ABSENT claim after `ggen-layout-court` land `d37271961` (**PRESENT**; live hop-5 tip-exit **PENDING** — no cargo); demotes Smalt spine row to **PARTIAL** (column exits ≠ RAID/`ALIVE`); crown **PARTIAL**; spine ggen/Taffy/zcode/CASTLE/Smalt **PARTIAL**, revops **BLOCKED**; clause-4 GA falsifiers still fire; tip-cargo / live adapter / canonical FinOps / digest-bind / Plane-5 DO / zero-HITL **not** invented as `ALIVE` |
| Hop5 foundry `--live` | 2026-11-22 — branch `cursor/v2711-hop5-foundry-live-bc54` Observed tip → `6fdcf4665d52931f7de25909366127bc9128a230`; `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=1** / **REFUSED** (`live executor not configured (SUBJECT_MISSING)`; ENOENT overlay after rollback); layout_court preflight exit **0** (prebuilt on PATH; no cargo); hop-5 aggregate **PARTIAL**; Foundry crown stays **PARTIAL**; receipt `/tmp/cursor/hop5-foundry-live-receipt.md`; no hop-5 / path / zero-HITL `ALIVE` |
| Hop5 foundry live2 | 2026-11-22 — branch `cursor/v2711-hop5-foundry-live2-bc54` absorbed onto tip with photo-retip2; zcode `f32df08` wires `createZcodeAgentExecutor` into Gall `--live`; **foundry_exit=1** / **REFUSED** (`ZCode runtime is missing`); hop-5 **PARTIAL_ALIVE**; Foundry crown **PARTIAL**; receipt `/tmp/cursor/hop5-foundry-live2-receipt.md`; ledger [`HOP5-FOUNDRY-LIVE2-STANDING.md`](./HOP5-FOUNDRY-LIVE2-STANDING.md); no hop-5 / path / zero-HITL `ALIVE` |
| Hop5 zcode runtime | 2026-11-22 — branch `cursor/v2711-hop5-zcode-runtime-bc54` (#917) absorbed onto tip; Linux tip-matched 3.14.4 `.deb` → `vendor/zcode.cjs` **PRESENT**; `--live` **foundry_exit=1** / **REFUSED** (`Model access is not configured`); hop-5 **PARTIAL_ALIVE**; Foundry crown **PARTIAL**; receipt `/tmp/cursor/hop5-zcode-runtime-receipt.md`; ledger [`HOP5-ZCODE-RUNTIME-STANDING.md`](./HOP5-ZCODE-RUNTIME-STANDING.md); no hop-5 / path / zero-HITL `ALIVE` |
| Ui-layout tip-cargo ALIVE | 2026-11-22 — absorb `cursor/v2711-uilayout-tip-alive-bc54` @ `8090022f4` onto tip `120598fb3` (`cursor/v2711-gate1-uilayout-alive-bc54`); exclusive quiet cargo → **11 passed; exit 0** @ `ca52e9c16` (sources ≡ tip); prior evidence `eaf0dad40`; zero-HITL remains **PARTIAL**; Gate1 overall **PARTIAL**; no ManagePullRequest |
| Companion honesty retip | 2026-11-22 — branch `cursor/v2711-companion-honesty-bc54` atop crown `678a6c106` / Gate-1 honesty `b565b8e83`: ECOSYSTEM §7–8 shui/taffy/hot-swap lineage **ALIVE** @ `26256ca9d`; production theme **UNSUPPORTED**; FOUNDRY hop-5 **REFUSED** (not crown **ALIVE**); OOR tip pin refresh; no cargo |
| OOR honesty retip8 | 2026-11-22 — `cursor/v2711-oor-honesty-retip8-bc54` @ `9d4abfa67` pin blockers held (cos 503 / revops NXDOMAIN / Hop5 HITL / BRCE V8); light FOUNDRY header pin @ `97c613452` — **not** tip-pinned spine inventory |
| Foundry spine honesty retip | 2026-11-22 — branch `cursor/v2711-foundry-spine-retip-bc54` atop tip `59c090984` (`97c613452+`): tip-pinned spine inventory — ggen+Taffy tip-cargo **ALIVE** (prior); igniter clean-room **ALIVE** EXIT:0 @ `f1492ed55` (`bc-53b0e099`); Hop4 mix `--check` **ALIVE** EXIT:0 @ `97c613452` (`bc-54b308e2`); CASTLE/Smalt **ALIVE** (prior); zcode Hop5 `--live` **BLOCKED_HITL** (`bc-45fb88ed` may be in flight); revops NXDOMAIN / cos 503 **BLOCKED**; BRCE V8 **BLOCKED**; `write_apply` **PENDING** (`bc-a4aaf86a`); crown **PARTIAL**; no cargo; parent owns #849 body |
| Foundry-finish honesty retip | 2026-11-22 — branch `cursor/v2711-foundry-finish-honesty-de6d` atop tip `77eb46fd0` (evidence `e7569893a`): absorb tip-cargo honesty — ggen/Taffy/zcode-refusal/CASTLE/Smalt/CCS **ALIVE** where cites exist; Zero-HITL **PARTIAL**; OOR `--live` **BLOCKED_HITL** / cos **503** / revops **NXDOMAIN** / BRCE **V8 BLOCKED**; r01+BRCE seat exact-tip **ALIVE** @ `e7569893a`; brand exact-tip **COMPILING**/lineage @ `c91b22561`; clears stale `write_apply` **PENDING**; no cargo; no ManagePullRequest |
| CASTLE crown-wire probe | 2026-11-22 — side branch `cursor/v2711-castle-wire-probe-bc54` from crown `6a3a623ba` (does **not** push crown): sibling castle **REACHABLE** @ `2774039ea`; digest-bind admit **ABSENT**/**UNSUPPORTED** on publish wire; crown wire **UNKNOWN**; LinkedIn **BLOCKED** (cos **503**); no cargo; no ManagePullRequest — [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §11 |
| CASTLE digest-bind wire | 2026-11-22 — ggen sidebranch `cursor/v2711-castle-digest-bind-bc54` (**no** crown push; no ManagePullRequest on #849); castle `cursor/digest-bind-admit-ca8a` @ `e0d99c6`: wire bind **WIRED**; Chicago court **6/6** exit **0**; CLI `--ggen-layout-digest`; spine wire **PARTIAL_ALIVE**; Foundry crown still **PARTIAL**/**UNKNOWN** — [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §13 |
| Smalt GA residual honesty absorb | 2026-11-22 — tip-wins absorb `cd28efed7` (`cursor/v2711-smalt-ga-residual-bc54`) onto crown `f5f44571f` via `cursor/v2711-smalt-ga-residual-absorb-da2d`: b4b register+modules **PRESENT** @ smalt `91997da76f0f`; residual courts then **UNKNOWN** (mix **UNSUPPORTED**); KD **UNVERIFIED**/SOURCE-CHECK **PARTIAL**; 6-column spine **ALIVE** held; Foundry crown stay **PARTIAL** — **not** promoted; OOR retip11 + CASTLE wire content preserved; no cargo; no ManagePullRequest |
| Smalt residual mix courts | 2026-11-22 — sidebranch `cursor/v2711-smalt-mix-courts-bc54` @ `7334aa971` (`bc-c700dafb`) tip-wins absorbed onto crown `79b64496b` via `cursor/v2711-smalt-mix-absorb-7abb`: smalt `91997da76f0f` treasury/tax/erp/rates exits **0,0,0,0** (8/8/8/7) ⇒ residual **ALIVE**; Smalt RAID / GA economic face **ALIVE**; Foundry / zero-HITL crown stays **PARTIAL** — **not** invented (RAID ALIVE ≠ Foundry zero-HITL ALIVE); OOR retip12 cos **503** / revops **NXDOMAIN** **BLOCKED** held; no cargo; no ManagePullRequest |
| Founder HITL `--live` next-verifier tip-wins absorb | 2026-11-22 — tip-wins absorb `cursor/v2711-hitl-live-next-bc54` @ `723c3a14f` (`bc-ea2b3d10`) onto crown `00b53cec9` via `cursor/v2711-hitl-live-absorb-8df9` (⊇ Smalt residual mix / OOR retip12 / affidavit / BRCE V8+R01 / VW / CASTLE): pin foundry `--live` **BLOCKED_HITL** / **REFUSED** (`Model access is not configured`); owner **Founder HITL** + **zcode-cli**; falsifier `cd /agent/repos/zcode-cli && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0**; **no** invented `--live` `ALIVE`; docs-only; no cargo; no ManagePullRequest — [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) |
