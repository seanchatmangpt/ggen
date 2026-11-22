# FOUNDRY-FINISH-STANDING — Horizontal spine without daily founder HITL

| Field | Value |
|-------|-------|
| Document ID | `FOUNDRY-FINISH-STANDING` |
| Companion to | `ECOSYSTEM-SPEC-27.1.1` (target GA **2027-01-01**) |
| Status | STANDING LEDGER (not an execution receipt; not a Gate-1 rewrite) |
| Observed tip | PR [#849](https://github.com/seanchatmangpt/ggen/pull/849) / `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `d05a131c7521d5a6325ade2e8295fee650659d5a` (past ~`28c3f4d845`; retip `cursor/v2711-gate1-photo-retip2-bc54`; Truex **ALIVE** @ `68d2d233`; hop-4 manufacture **ALIVE** `diff=0`; hop-5 **PARTIAL_ALIVE** adapter **0** / `--live` open; brand.toml hot-swap **PARTIAL_ALIVE** / production **UNSUPPORTED**; ui_layout tip-cargo **PENDING**; FM-PACK **PRESENT**/**PARTIAL_ALIVE**; cos **BLOCKED**; `ggen-layout-court` **PRESENT**; CASTLE sibling courts **ALIVE** @ `2774039ea`; Smalt 4-cloud exit 0 @ `875efbb87` / fin_ops **PARTIAL**; Foundry crown stays **PARTIAL**) |
| Prior tip note | Was `704025461` / live1 `live executor not configured (SUBJECT_MISSING)`; hop-5 mix-SUBJECT closed adapter to **0**; `--live` still open — **do not** invent live hop-5 / tip-cargo / full RAID / zero-HITL `ALIVE`. |
| Base (main) | `origin/main` @ `d173cad5792e837bd4fec730d120c582b9bc5d4c` |
| Repo scope | `seanchatmangpt/ggen` names the spine; sibling planes are **tracked out-of-repo** |

Standing vocabulary (AGENTS.md / ECOSYSTEM-SPEC):
`UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority rule.** This document is the **source of truth for horizontal Foundry-finish spine standing**. It does **not** rewrite `GATE-1-GAP-MATRIX.md`. Gate-1 keeps requirement → evidence → verifier → owner rows; this ledger answers whether the full spine can run **without daily founder HITL**.

**Crown claim (Foundry finish).** Horizontal spine
`ggen → Taffy → zcode-cli → CASTLE → Smalt → revops` runs as an admitted, receipt-bound loop with **no ambient daily founder HITL**.

| Crown claim standing | Reason |
|----------------------|--------|
| **PARTIAL** | Tip `d05a131c7`: Truex **ALIVE** @ `68d2d233`; hop-4 BrandTheme manufacture **ALIVE** (`diff=0`); hop-5 **PARTIAL_ALIVE** (adapter **0**; foundry `--live` open); brand.toml hot-swap **PARTIAL_ALIVE** / production **UNSUPPORTED**; ui_layout tip-cargo **PENDING**; FM-PACK **PRESENT**/**PARTIAL_ALIVE**; cos **BLOCKED**. In-repo fences: `shui:` / Taffy / brand / verify_witness / R-01…R-07 / BRCE AS-01 / zcode POWL stub / **ggen `ui-layout-court` ALIVE** (evidence `eaf0dad40`; tip source **11**; tip-cargo PENDING). `ggen-layout-court` **PRESENT** @ `d37271961` ≠ live hop-5 crown. CASTLE sibling courts **ALIVE** @ `2774039ea`; Smalt columns exit 0 @ `875efbb87` / fin_ops **PARTIAL** — Smalt spine **PARTIAL**. Plane-5 revops / cos **BLOCKED**. Clause-4 GA falsifiers still fire. **Do not invent crown `ALIVE`.** Zero daily founder HITL is **not** `ALIVE`. |

Do not promote source presence, fleet-census rows, workflow definitions, or historical jira prose to crown `ALIVE`. The ggen ui-layout in-repo fence is **ALIVE** on evidence tip `eaf0dad40` (matches [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md)); tip court land `7c0ba2a90` + tip source **11** + **tip-cargo PENDING** on `4b22529df2939412c41107fe5e9eea66ba6a4dad` — that does **not** promote the Foundry-finish path crown. `ggen-layout-court` **PRESENT** ≠ live hop-5 / Foundry crown `ALIVE`. Smalt column exits 0 ≠ Smalt spine / RAID / Foundry crown `ALIVE`.

---

## 1. Horizontal spine table

| Component | Role on spine | Standing | HITL residual (founder) | Next exact verifier | GA `2027-01-01` falsifier |
|-----------|---------------|----------|-------------------------|---------------------|---------------------------|
| **ggen** | Manufacture kernel (`μ`): packs, graphs, layout pre-flight, brand rhythm, public-prefix fences, witness size court, R-01…R-07 contract, ui-layout digest consume, `ggen-layout-court` CLI | **PARTIAL** — tip `4b22529df2939412c41107fe5e9eea66ba6a4dad`; `ui_layout_court` **ALIVE** on evidence `eaf0dad40` (**8 passed; exit 0**); tip source **11**; **tip-cargo PENDING**; `ggen-layout-court` **PRESENT** @ `d37271961` (presence ≠ tip-cargo `ALIVE`). Zero-HITL fab not sealed | Daily merge/admission judgment; founder still gates “promote Gate-1 → GA”; risk bodies R-01…R-07 still **BLOCKED** out-of-repo | Exact next: quiet tip-cargo `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test ui_layout_court` → exit 0 on `4b22529df` (also replay `shui_public_prefix_court` / `taffy_aot_preflight_court` / `brand_toml_rhythm_court` / `verify_witness_size_court` / `r01_r07_resolution_court`) | If on **2027-01-01** a founder must still manually admit ordinary spine manufacture (SELECT/CONSTRUCT) that courts already cover, crown claim is **falsified** |
| **Taffy** | AOT layout pre-flight (widths `320/375/414/768/1024/1440` + WCAG 1.4.12 @ 1.5×) inside `ggen-graph::layout::taffy_aot` | **PARTIAL** — six-width seal court present; in-repo digest consume fence **ALIVE**; path crown not ALIVE until hop-4/5 bind | Founder still wires / retries zcode or igniter consume when receipt law drifts | Exact next: `cargo test -p ggen-graph --test taffy_aot_preflight_court` → exit 0 on tip `4b22529df` | If on **2027-01-01** geometry law still requires founder-edited exceptions instead of typed `LayoutRefusal` / seal receipts, Taffy row is **falsified** |
| **zcode-cli** | Consume ggen layout court via sibling `UiLayoutCourtAdapter`; POWL/OCEL bounded remediations | **PARTIAL_ALIVE** — in-process bun **ALIVE** (**35 passed; exit 0**); `ggen-layout-court` **PRESENT** @ `d37271961`; default live adapter exit **0** (mix-SUBJECT fix; Jobs/C4 deferred); foundry `--live` still **open** (not hop-5 crown `ALIVE`). Do **not** invent live hop-5 / Foundry crown `ALIVE` | Founder must close foundry `--live` (+ Jobs/C4 `mix` when required) before hop-5 exits 0 | Exact next: `bun scripts/powl-foundry-loop.ts --live` → 0; replay `layout_court` + adapter → 0. Receipts: `/tmp/cursor/hop5-foundry-live-receipt.md`; [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) | If on **2027-01-01** UI courts still need founder to paste/override layout digests or remediations run unbounded, zcode row is **falsified** |
| **CASTLE** | Consequence / publish plane (`CONSEQUENTIAL_ADMISSIBILITY`); Gate 2–3 public mint | **PARTIAL** — sibling courts **ALIVE** @ castle `2774039ea` (`bc-1bffb072`); in-ggen publish **UNSUPPORTED**; spine crown wire **UNKNOWN** (no ggen digest bind). Do **not** promote Foundry crown | Founder still invents token inventories without admitted CASTLE sources; digest-bind + LinkedIn AND-gate open | Exact next crown: bind ggen layout/brand digest into mint, then replay `cargo test --test publish_redaction_court`; `cargo test --test cli_publish_brand`; `./target/debug/castle publish post --subject spine-probe --public-anonymize --brand brands/fourth-watch.brand.toml` → 0 / `redaction_court=ALIVE` | If on **2027-01-01** CASTLE publish still depends on daily founder token edits with no receipt, CASTLE row is **falsified** |
| **Smalt** | 4-cloud RAID / economic-ops substrate (Azure·AWS·GCP·Twilio/fnox) | **PARTIAL** @ smalt `875efbb87` — doctor/aws/gcp/azure/twilio exits **0,0,0,0,0**; fin_ops burn **PARTIAL** / canonical **ABSENT**/**UNSUPPORTED**. Column exits ≠ full RAID/`ALIVE`; ≠ Foundry crown | Founder still restores canonical FinOps court; no hands-free RAID/`ALIVE` receipt | Exact next: restore `test/fin_ops_calculator_test.exs` then `mix test test/fin_ops_calculator_test.exs` → 0 | If on **2027-01-01** Smalt RAID still requires daily founder HITL to keep the membrane green, Smalt row is **falsified** |
| **revops** | Plane-5 front door (`revops.fnox.io`) + cos adjacency — sole DO via **BRCE + WebAuthn AS-01** | **BLOCKED** for Plane-5 DO / readiness — `revops.fnox.io` DNS fail (**UNKNOWN**); cos `/readyz` **HTTP 503** `DB_UNAVAILABLE`. In-repo BRCE AS-01 fence **PARTIAL**; live BRCE **BLOCKED**; WebAuthn/`prime256v1` **UNSUPPORTED** | Founder still actuates Plane-5 without AS-01 runtime | Exact next: `getent hosts revops.fnox.io`; `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200**; then BRCE+AS-01 Plane-5 court. Fence-only: `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` | If on **2027-01-01** Plane-5 still depends on daily founder HITL for ordinary DO, revops row (and crown claim) is **falsified** |
---

## 1b. Adjacent Gate-1 / authority fences (not spine members; block zero-HITL)

| Subject | Standing on tip `4b22529df2939412c41107fe5e9eea66ba6a4dad` (ui-layout evidence `eaf0dad40`; tip-cargo PENDING; layout_court PRESENT @ `d37271961`) | Next exact verifier |
|---------|-----------------------------|---------------------|
| BRCE + WebAuthn AS-01 ungated-DO refusal ([#861](https://github.com/seanchatmangpt/ggen/pull/861)) | **PARTIAL_ALIVE** (presence/shape fence; Gate-1 honesty demotion aligns matrix to PARTIAL + tip-cargo PENDING); crypto verify **UNSUPPORTED**; live BRCE **BLOCKED** | `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` |
| `verify_witness` WASM &lt;250KB size court | **PARTIAL_ALIVE** (offline size court; historical exit 0 on `0478fde59`; tip-cargo PENDING) — production WASM out-of-repo [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) remains **UNSUPPORTED**; never production-artifact `ALIVE` | `cargo test -p ggen-graph --test verify_witness_size_court` |
| `shui:` public-prefix fence | **PARTIAL_ALIVE** (Gate-1 matrix: **ALIVE** on evidence `0478fde59` / tip source **4**; **tip-cargo PENDING** quiet cgroup) — Truex product admit **ALIVE** @ `truex@68d2d233` (separate sibling row) | `cargo test -p ggen-graph --test shui_public_prefix_court` |
| R-01…R-07 resolution contract | **PARTIAL_ALIVE** (merged contract court; Gate-1 matrix: evidence-ancestor **ALIVE** + tip-cargo PENDING); risk **bodies** **BLOCKED** / **UNSUPPORTED** [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) | `cargo test -p ggen-graph --test r01_r07_resolution_court` |
| Brand `.toml` rhythm (OEM stubs) | **PARTIAL_ALIVE** — evidence `0478fde59` **4 passed; exit 0**; tip source **11**; tip-cargo PENDING; hot-swap fence **PARTIAL_ALIVE** (`brand_toml_hotswap_court` PRESENT @ `f6cb18f86`; ceiling PARTIAL); **production theme UNSUPPORTED** (`REFUSED_PRODUCTION`); full DTCG/CASTLE inventory Gate 2+ | Rhythm/hot-swap: `cargo test -p ggen-graph --test brand_toml_rhythm_court`; `cargo test -p ggen-graph --test brand_toml_hotswap_court`. Production refuse: `cargo test -p ggen-graph --lib layout::brand::production_theme_tests`. Next promote: [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md) |
| ggen `ui-layout-court` / `layout::ui_layout` / LayoutRefusal / fluid-clamp | **ALIVE** on evidence `eaf0dad40` (**8 passed; exit 0**; `bc-c7c9d554`; ancestor); tip court land `7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo PENDING** quiet cgroup (not tip exit 0) | `cargo test -p ggen-graph --test ui_layout_court` |
| `ggen-layout-court` bin + sibling live adapter | Package **PRESENT** @ `d37271961`; in-process bun **35/exit 0**; live adapter exit **0** on tip evidence; prebuilt `layout_court` on PATH exit **0** (no cargo under load&gt;8) | Replay: `layout_court fixtures/ui-layout-court/product-subject.json` → 0; `UiLayoutCourtAdapter.verifyPostTransition` → 0 |
| ggen_igniter clean-room / manufacture | BrandTheme manufacture compare **ALIVE** (`diff_exit=0`; dual-schema tip brand); mount **PRESENT**; `mix` NIF / sync --check still **BLOCKED** [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md); hop ledger [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) | Replay manufacture compare → 0; residual: `mix ggen_igniter.sync --check` → 0; in-repo: `cargo test -p ggen-sjira --test igniter_cleanroom_court` |
| FM-PACK-001 `packs/gh-terraform-pack` | **PRESENT** / **PARTIAL_ALIVE** on tip (presence court; composed e2e SKIPPED) — [`REPO-INVENTORY-27.1.1.md`](./REPO-INVENTORY-27.1.1.md) | When loadavg &lt;10: `timeout 600s cargo test -p ggen-engine --test composed_packs_e2e -- --nocapture` → 0 |
| cos (Gate-0 / Plane-5 adjacency) | **BLOCKED** — cos `/readyz` **HTTP 503** `DB_UNAVAILABLE`; none in ggen | `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** |
| Live zcode POWL/OCEL / foundry | **PARTIAL_ALIVE** aggregate — adapter exit **0**; foundry `--live` still **open** (not crown `ALIVE`) | `bun scripts/powl-foundry-loop.ts --live` → 0; receipt `/tmp/cursor/hop5-foundry-live-receipt.md` |
| CCS authority matrix | Ledger **MATERIALIZED** [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md); Plane-5 DO still not ALIVE | See CCS next verifiers; do not promote from prose |

---

## 2. HITL residual summary (why daily founder is still required)

| Residual class | Where it bites | Clearance condition |
|----------------|----------------|---------------------|
| Gate-1 photolithography open | ui-layout **ALIVE** evidence + tip-cargo **PENDING**; hop-4 manufacture **ALIVE** (`diff=0`); hop-5 **PARTIAL_ALIVE** (adapter **0**; `--live` open); `mix` NIF residual **BLOCKED** | Close foundry `--live` → 0; optional `mix ggen_igniter.sync --check` → 0 |
| Live zcode / live BRCE | Chicago POWL court unmounted; BRCE broker unmounted; AS-01 crypto **UNSUPPORTED** | Mounted sibling courts + BRCE+AS-01 runtime receipt — fence alone is not ALIVE |
| Out-of-repo planes unmounted | CASTLE, Smalt, revops, Truex product admit | Tracked standing docs + exact sibling verifiers (see §4) |
| GA risk register open | R-01 … R-07 bodies | Typed receipt or typed refusal per risk before GA (contract fence ≠ body closure) |
| Swarm merge HITL | Multiple Gate-1 slices → #849 | Merge coordinator lands courts without founder re-authoring law; tip exit reconfirm |

Foundry finish is **not** “all crates compile.” It is **zero daily founder HITL** on the horizontal spine while preserving `SELECT ≠ CONSTRUCT ≠ DO`.

---

## 3. Next aggregate verifier (this ledger)

Replay cheapest high-information path first (all against tip `4b22529df2939412c41107fe5e9eea66ba6a4dad` or successor; **tip-cargo PENDING** before claiming tip exit 0; this honesty retip did **not** run cargo):

```text
# Gate-1 in-repo fences on #849 tip (exit reconfirm)
cargo test -p ggen-graph --test shui_public_prefix_court
cargo test -p ggen-graph --test taffy_aot_preflight_court
cargo test -p ggen-graph --test ui_layout_court
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

### 4.1 On PR #849 tip `4b22529df2939412c41107fe5e9eea66ba6a4dad` (foundry honesty retip; layout_court land `d37271961`)

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
| [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md) | DO authority edge (AS-01 `:prime256v1`) — **PARTIAL_ALIVE** fence on tip via [#861](https://github.com/seanchatmangpt/ggen/pull/861) |
| [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) | zcode-cli POWL·OCEL bounded refusal — **PARTIAL_ALIVE** in-repo on tip |
| [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md) | CCS authority matrix |
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
- Truex product admission of `shui:` is **ALIVE** @ `truex@68d2d233` (sibling); ggen fence tip-cargo PENDING ≠ Truex substitute.
- Affidavit pack manufacture / placeholder size court PARTIAL ≠ production `verify_witness` WASM &lt;250KB ALIVE.
- BRCE AS-01 fence PARTIAL ≠ live BRCE broker ALIVE; Ed25519 SovereignLease ≠ WebAuthn `:prime256v1`.
- ggen `ui_layout_court` **ALIVE** on evidence `eaf0dad40` (tip land `7c0ba2a90`; tip source **11**; **tip-cargo PENDING** on `5e00619a4`) ≠ live sibling `ui-layout-court.ts` ALIVE; ≠ Foundry path crown ALIVE. Do **not** invent Smalt / revops / igniter `ALIVE`.
- zcode in-repo stub PARTIAL ≠ live `bun test` / sibling `ui-layout-court` ALIVE.
- This ledger’s existence is **PARTIAL_ALIVE** documentation standing only — not Foundry-finish crown `ALIVE`.

---

## 6. Closure rule for crown ALIVE

Promote Foundry-finish crown claim to `ALIVE` only when **all** hold:

1. Gate-1 aggregate rows that own ggen fences are `ALIVE` (or typed `REFUSED_*` with remediation closed) per `GATE-1-GAP-MATRIX.md` — including `ui-layout` consume (**ALIVE** on evidence `eaf0dad40`; **8 passed; exit 0**; tip land `7c0ba2a90`; tip source **11**; **tip-cargo PENDING** on `5e00619a4`).
2. Every §1 spine component is either `ALIVE` or has an out-of-repo tracked standing doc with an exact next verifier and a dated receipt tip.
3. Daily founder HITL is no longer required for ordinary spine SELECT/CONSTRUCT; Plane-5 DO is BRCE+AS-01 only (live, not doctrine-only).
4. GA date **2027-01-01** falsifiers in §1 would not fire if evaluated that day.

Until then, overall goal standing remains:

```text
FOUNDRY_FINISH = PARTIAL
  ∧ Truex ALIVE @ 68d2d233
  ∧ hop-4 manufacture ALIVE (diff=0)
  ∧ hop-5 PARTIAL_ALIVE (adapter 0; --live open)  # ≠ hop-5 / path crown ALIVE
  ∧ brand.toml hot-swap PARTIAL_ALIVE ∧ production UNSUPPORTED
  ∧ Gate1 ui-layout ALIVE  # evidence eaf0dad40; tip-cargo PENDING on d05a131c7
  ∧ FM-PACK PRESENT / PARTIAL_ALIVE  # presence ≠ composed e2e ALIVE
  ∧ ggen-layout-court PRESENT @ d37271961  # ≠ live hop-5 ALIVE
  ∧ CASTLE sibling spine courts ALIVE @ 2774039ea  # ≠ crown
  ∧ ¬(CASTLE spine crown ALIVE)
  ∧ Smalt 4-cloud courts exit 0 @ 875efbb87 + fin_ops PARTIAL/ABSENT  # spine PARTIAL
  ∧ ¬(Smalt RAID / economic-ops ALIVE)
  ∧ cos BLOCKED
  ∧ ¬(revops ALIVE)
  ∧ ¬(foundry --live / live BRCE ALIVE)
  ∧ ¬(zero daily founder HITL ALIVE)
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
| Evidence | Evidence tip `eaf0dad40`; ggen `ui_layout_court` **ALIVE** (**8 passed; exit 0**); tip court land `7c0ba2a90`; tip source **11**; **tip-cargo PENDING** on `5e00619a4`; sibling `ui-layout-court.ts` still ABSENT in ggen; CASTLE sibling courts **ALIVE** @ `2774039ea` (`bc-1bffb072`) — crown wire **UNKNOWN**; Smalt 4-cloud **PARTIAL_ALIVE** @ `91997da76f0f` (parent `875efbb87`; FinOps **ALIVE** exit 0 / 19 passed; 4-cloud @ `875efbb87`); revops DNS fail + cos `/readyz` 503 → **UNKNOWN**/**BLOCKED**; igniter manufacture open; no Plane-5 / full Smalt RAID/`ALIVE` / revops / zero-HITL / Foundry crown claim asserted as `ALIVE` |
| Hop5 live after #884 | 2026-11-22 — `cursor/v2711-hop5-live-bc54` tip `704025461`; `layout_exit=0`; default `adapter_exit=2` (`mix` SUBJECT_MISSING); foundry `--live` foundry_exit=1; hop-5 **PARTIAL** (demoted false PARTIAL_ALIVE/adapter ALIVE); receipt `/tmp/cursor/hop5-live-after-884.md` |
| Hop5 cross-mount | 2026-11-22 — branch `cursor/v2711-hop5-standing-bc54` Observed tip `f6cb18f86` (evidence tip `07b689c97` / ancestor `c9e224d33`); hop-5 **PARTIAL** (in-process bun **ALIVE** 35/exit 0) + live **BLOCKED** (`ggen-layout-court` ABSENT; adapter exit 2; foundry live exit 1); receipt `/tmp/cursor/hop5-crossmount.md`; no live/path-crown `ALIVE` |
| CASTLE sibling ALIVE | 2026-11-22 — branch `cursor/v2711-castle-alive-bc54` against tip `d0e3ba473`; castle tip `2774039ea` via `bc-1bffb072` — `publish_redaction_court` **13/13**, `cli_publish_brand` **6/6**, spine-probe `redaction_court=ALIVE`, exits **0,0,0**; Foundry crown remains **PARTIAL_ALIVE**; zero-HITL **not** `ALIVE` |
| Smalt PARTIAL_ALIVE | 2026-11-22 — branch `cursor/v2711-smalt-partial-bc54` against tip `8a68da481` (post-`5480d7ada`); smalt tip `875efbb87` via `bc-ad9ebffa`/`bc-1e876087` — RAID columns **ALIVE** (doctor/aws/gcp/azure/twilio exits **0,0,0,0,0**); fin_ops **PARTIAL_ALIVE** (burn-path exit 0; canonical ABSENT — restore still next); Foundry crown remains **PARTIAL_ALIVE**; full RAID/`ALIVE` **not** invented |
| Foundry honesty retip | 2026-11-22 — branch `cursor/v2711-foundry-honesty-bc54` Observed tip → `4b22529df2939412c41107fe5e9eea66ba6a4dad`; corrects stale ABSENT claim after `ggen-layout-court` land `d37271961` (**PRESENT**; live hop-5 tip-exit **PENDING** — no cargo); demotes Smalt spine row to **PARTIAL** (column exits ≠ RAID/`ALIVE`); crown **PARTIAL**; spine ggen/Taffy/zcode/CASTLE/Smalt **PARTIAL**, revops **BLOCKED**; clause-4 GA falsifiers still fire; tip-cargo / live adapter / canonical FinOps / digest-bind / Plane-5 DO / zero-HITL **not** invented as `ALIVE` |
| Hop5 foundry `--live` | 2026-11-22 — branch `cursor/v2711-hop5-foundry-live-bc54` Observed tip → `6fdcf4665d52931f7de25909366127bc9128a230`; `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=1** / **REFUSED** (`live executor not configured (SUBJECT_MISSING)`; ENOENT overlay after rollback); layout_court preflight exit **0** (prebuilt on PATH; no cargo); hop-5 aggregate **PARTIAL**; Foundry crown stays **PARTIAL**; receipt `/tmp/cursor/hop5-foundry-live-receipt.md`; no hop-5 / path / zero-HITL `ALIVE` |
