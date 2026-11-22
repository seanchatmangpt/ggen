# FOUNDRY-FINISH-STANDING — Horizontal spine without daily founder HITL

| Field | Value |
|-------|-------|
| Document ID | `FOUNDRY-FINISH-STANDING` |
| Companion to | `ECOSYSTEM-SPEC-27.1.1` (target GA **2027-01-01**) |
| Status | STANDING LEDGER (not an execution receipt; not a Gate-1 rewrite) |
| Observed tip | PR [#849](https://github.com/seanchatmangpt/ggen/pull/849) / `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `5e00619a422a0d8e451029f0aaf8b7a63e27da46` (2026-11-22 ui-layout **ALIVE** evidence `eaf0dad40` exit 0 / 8 passed — ancestor; tip court land `7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo reconfirm PENDING** quiet cgroup — not invented as tip exit 0) |
| Prior tip note | Was `7bf148d82` at ops refresh; tip advanced (`e5ff3f822` → `7bf148d82` → `cbfd94582` → `eaf0dad40` LayoutRefusal/brand fluid + court exit 0 → `7c0ba2a90` viewport/expect-free court land → `43e8541c8` → `6b66659c4` → `b96100521` → `0ae8d8108` ui-layout ALIVE pin → `5a39ea90a` OOR retip → `61311cb53` → `6ab75e6a3` ui-layout JSON/anti-vacuity → `5e00619a4` rustfmt tip). Spine reconfirm on this tip — **do not** claim tip unchanged. |
| Base (main) | `origin/main` @ `d173cad5792e837bd4fec730d120c582b9bc5d4c` |
| Repo scope | `seanchatmangpt/ggen` names the spine; sibling planes are **tracked out-of-repo** |

Standing vocabulary (AGENTS.md / ECOSYSTEM-SPEC):
`UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority rule.** This document is the **source of truth for horizontal Foundry-finish spine standing**. It does **not** rewrite `GATE-1-GAP-MATRIX.md`. Gate-1 keeps requirement → evidence → verifier → owner rows; this ledger answers whether the full spine can run **without daily founder HITL**.

**Crown claim (Foundry finish).** Horizontal spine
`ggen → Taffy → zcode-cli → CASTLE → Smalt → revops` runs as an admitted, receipt-bound loop with **no ambient daily founder HITL**.

| Crown claim standing | Reason |
|----------------------|--------|
| **PARTIAL_ALIVE** | In-repo Gate-1 fences advanced on #849 tip (`shui:` / Taffy / brand / verify_witness / R-01…R-07 / BRCE AS-01 / zcode POWL stub / **ggen `ui-layout-court` ALIVE** — **8 passed; exit 0** on evidence tip `eaf0dad40` via `bc-c7c9d554`; tip court land `7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo PENDING** quiet cgroup on `5e00619a4` — ALIVE retained, not demoted for thrash/OOM). Live igniter clean-room, live zcode Chicago / sibling `ui-layout-court.ts`, CASTLE publish, Smalt RAID, and Plane-5 revops remain **BLOCKED / UNSUPPORTED / UNKNOWN**. Crown stays **incomplete** until out-of-repo hops close with typed receipts (or typed refusals). **Do not invent crown `ALIVE`.** Zero daily founder HITL is **not** `ALIVE`. |

Do not promote source presence, fleet-census rows, workflow definitions, or historical jira prose to crown `ALIVE`. The ggen ui-layout in-repo fence is **ALIVE** on evidence tip `eaf0dad40` (matches [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md)); tip court land `7c0ba2a90` + tip source **11** + **tip-cargo PENDING** on `5e00619a422a0d8e451029f0aaf8b7a63e27da46` — that does **not** promote the Foundry-finish path crown.

---

## 1. Horizontal spine table

| Component | Role on spine | Standing | HITL residual (founder) | Next exact verifier | GA `2027-01-01` falsifier |
|-----------|---------------|----------|-------------------------|---------------------|---------------------------|
| **ggen** | Manufacture kernel (`μ`): packs, graphs, layout pre-flight, brand rhythm, public-prefix fences, witness size court, R-01…R-07 contract, ui-layout digest consume | **PARTIAL_ALIVE** — live workspace on tip `5e00619a422a0d8e451029f0aaf8b7a63e27da46`; `shui:` / Taffy / brand / verify_witness / R-01…R-07 / BRCE AS-01 / zcode POWL stub / **`ui_layout_court` ALIVE** (**8 passed; exit 0** on evidence `eaf0dad40`; tip court land `7c0ba2a90`; tip source **11**; **tip-cargo PENDING**). Full zero-HITL fab not sealed | Daily merge/admission judgment; founder still gates “promote Gate-1 → GA”; risk bodies R-01…R-07 still **BLOCKED** out-of-repo | Replay: `cargo test -p ggen-graph --test shui_public_prefix_court`; `… taffy_aot_preflight_court`; `… ui_layout_court`; `… brand_toml_rhythm_court`; `… verify_witness_size_court`; `… r01_r07_resolution_court`; `just check` | If on **2027-01-01** a founder must still manually admit ordinary spine manufacture (SELECT/CONSTRUCT) that courts already cover, crown claim is **falsified** |
| **Taffy** | AOT layout pre-flight (widths `320/375/414/768/1024/1440` + WCAG 1.4.12 @ 1.5×) inside `ggen-graph::layout::taffy_aot` | **PARTIAL_ALIVE** — six-width seal court present on tip; in-repo digest consume fence **ALIVE** (see ui-layout row); photolithography path crown not ALIVE until hop-4/5 siblings bind | Founder still wires / retries zcode or igniter consume when receipt law drifts; no hands-free photolithography loop | `cargo test -p ggen-graph --test taffy_aot_preflight_court` | If on **2027-01-01** geometry law still requires founder-edited exceptions instead of typed `LayoutRefusal` / seal receipts, Taffy row is **falsified** |
| **zcode-cli** | Consume ggen `AotSealReceipt` via `ui-layout-court.ts`; POWL/OCEL bounded autonomic remediations | **PARTIAL_ALIVE** (in-repo contract only) — [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) + `remediation_bound` + `ggen-sjira` IO-free stub landed on tip (integrate of `cursor/v2711-zcode-powl-ocel-bc54` / [#862](https://github.com/seanchatmangpt/ggen/pull/862)). **`ui-layout-court.ts` ABSENT** in this tree → **UNSUPPORTED**. Live sibling Chicago court **UNKNOWN** / **UNSUPPORTED** (checkout unmounted). Distinct from ggen `ui_layout_court.rs` (**ALIVE** evidence `eaf0dad40`; tip land `7c0ba2a90`; tip source **11**; **tip-cargo PENDING** on `5e00619a4`). | Founder must land/run sibling `ui-layout-court.ts` + `bun test test/powl-ocel-chicago.test.ts`; ggen cannot self-prove sibling consume or live remediation | In-repo: `cargo test -p ggen-sjira --test zcode_powl_ocel_court`; `cargo test -p ggen-graph --test ui_layout_court`; `cargo test -p ggen-graph remediation_bound --lib`. Out-of-repo: `pnpm test -- ui-layout-court`; `bun test test/powl-ocel-chicago.test.ts` | If on **2027-01-01** UI courts still need founder to paste/override layout digests or remediations run unbounded, zcode row is **falsified** |
| **CASTLE** | Consequence / publish plane (`CONSEQUENTIAL_ADMISSIBILITY`); Gate 2–3 public mint + brand inventory beyond Gate-1 rhythm stubs | **UNKNOWN** in ggen (no publish/court surface); sibling ledger [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) tracks castle tip without promoting ALIVE | Founder still invents or hand-publishes token inventories without admitted CASTLE sources (explicitly **REFUSED** in ECOSYSTEM-SPEC non-goals) | CASTLE-plane `castle publish` / RedactionCourt replay (out of ggen DO); see CASTLE-SPINE-STANDING next verifier | If on **2027-01-01** CASTLE publish still depends on daily founder token edits with no receipt, CASTLE row is **falsified** |
| **Smalt** | 4-cloud RAID / economic-ops substrate (Azure·AWS·GCP·Twilio/fnox) | **UNKNOWN** + **BLOCKED** from ggen (sibling not mounted); see [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) | Founder still runs Smalt gate / KD / RAID waves by hand; no ggen-admitted hands-free RAID receipt | Out-of-repo: Smalt 4-cloud parity / economic-ops courts in `seanchatmangpt/smalt` (track tip SHA + exit 0); do not claim ALIVE from ggen jira citations alone | If on **2027-01-01** Smalt RAID still requires daily founder HITL to keep the membrane green, Smalt row is **falsified** |
| **revops** | Plane-5 front door (`revops.fnox.io`) + cos adjacency — sole DO via **BRCE + WebAuthn AS-01** | **UNKNOWN** / **OUT OF SCOPE** for ggen DO — live probe: `revops.fnox.io` DNS fail (**UNKNOWN**); cos `/readyz` **BLOCKED** (`DB_UNAVAILABLE`) per [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md). In-repo BRCE AS-01 ungated-DO fence is **PARTIAL_ALIVE** ([#861](https://github.com/seanchatmangpt/ggen/pull/861); [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md)); live BRCE broker **BLOCKED**; WebAuthn/`prime256v1` crypto **UNSUPPORTED** | Founder still actuates Plane-5 without AS-01 runtime; doctrine+fence ≠ live DO | In-repo: `cargo test -p ggen-abb-sbb --test brce_as01_seat_court`. Out-of-repo: revops / fnox Plane-5 court that proves BRCE+AS-01 admits DO and refuses ambient founder actuation; cos `/readyz` → 200 without founder DB HITL | If on **2027-01-01** Plane-5 still depends on daily founder HITL for ordinary DO, revops row (and crown claim) is **falsified** |

---

## 1b. Adjacent Gate-1 / authority fences (not spine members; block zero-HITL)

| Subject | Standing on tip `5e00619a422a0d8e451029f0aaf8b7a63e27da46` (ui-layout evidence `eaf0dad40`; tip-cargo PENDING) | Next exact verifier |
|---------|-----------------------------|---------------------|
| BRCE + WebAuthn AS-01 ungated-DO refusal ([#861](https://github.com/seanchatmangpt/ggen/pull/861)) | **PARTIAL_ALIVE** (presence/shape fence; Gate-1 honesty demotion aligns matrix to PARTIAL + tip-cargo PENDING); crypto verify **UNSUPPORTED**; live BRCE **BLOCKED** | `cargo test -p ggen-abb-sbb --test brce_as01_seat_court` |
| `verify_witness` WASM &lt;250KB size court | **PARTIAL_ALIVE** (offline size court; historical exit 0 on `0478fde59`; tip-cargo PENDING) — production WASM out-of-repo [`VERIFY-WITNESS-PRODUCTION.md`](./VERIFY-WITNESS-PRODUCTION.md) remains **UNSUPPORTED**; never production-artifact `ALIVE` | `cargo test -p ggen-graph --test verify_witness_size_court` |
| `shui:` public-prefix fence | **PARTIAL_ALIVE** (Gate-1 matrix: **ALIVE** on evidence `0478fde59` / tip source **4**; **tip-cargo PENDING** quiet cgroup) — Truex product admit remains **UNKNOWN** | `cargo test -p ggen-graph --test shui_public_prefix_court` |
| R-01…R-07 resolution contract | **PARTIAL_ALIVE** (merged contract court; Gate-1 matrix: evidence-ancestor **ALIVE** + tip-cargo PENDING); risk **bodies** **BLOCKED** / **UNSUPPORTED** [`R01-R07-STANDING.md`](./R01-R07-STANDING.md) | `cargo test -p ggen-graph --test r01_r07_resolution_court` |
| Brand `.toml` rhythm (OEM stubs) | **PARTIAL_ALIVE** — evidence `0478fde59` **4 passed; exit 0**; tip source **11** `#[test]`; Gate-1 honesty demotion keeps rhythm **PARTIAL_ALIVE** + tip-cargo PENDING; **hot-swap ABSENT/UNSUPPORTED**; full DTCG/CASTLE inventory Gate 2+ | `cargo test -p ggen-graph --test brand_toml_rhythm_court` |
| ggen `ui-layout-court` / `layout::ui_layout` / LayoutRefusal / fluid-clamp | **ALIVE** on evidence `eaf0dad40` (**8 passed; exit 0**; `bc-c7c9d554`; ancestor); tip court land `7c0ba2a90`; tip source **11** `#[test]`; **tip-cargo PENDING** quiet cgroup (not tip exit 0) | `cargo test -p ggen-graph --test ui_layout_court` |
| Sibling `ui-layout-court.ts` (zcode-cli) | **ABSENT** in ggen tree → **UNSUPPORTED** (`find` zero under repo root; fixture stubs ≠ live runner) | Sibling `pnpm test -- ui-layout-court` in `seanchatmangpt/zcode-cli` |
| ggen_igniter clean-room | **BLOCKED** (sibling unmounted) — in-repo contract **PARTIAL_ALIVE** [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) | In sibling: `cd ggen_igniter && git diff --exit-code`; in-repo: `cargo test -p ggen-sjira --test igniter_cleanroom_court` |
| Live zcode POWL/OCEL Chicago court | **UNKNOWN** / **UNSUPPORTED** (sibling not mounted) | `bun test test/powl-ocel-chicago.test.ts` in `seanchatmangpt/zcode-cli` |
| CCS authority matrix | Ledger **MATERIALIZED** [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md); Plane-5 DO still not ALIVE | See CCS next verifiers; do not promote from prose |

---

## 2. HITL residual summary (why daily founder is still required)

| Residual class | Where it bites | Clearance condition |
|----------------|----------------|---------------------|
| Gate-1 photolithography open | ggen ui-layout fence **ALIVE** (exit 0 on evidence `eaf0dad40`; tip land `7c0ba2a90`; tip source **11**; **tip-cargo PENDING** on `5e00619a4`); sibling `ui-layout-court.ts` **ABSENT**; igniter hop **BLOCKED** | Replay `cargo test -p ggen-graph --test ui_layout_court`; sibling `ui-layout-court` exit 0 on a pinned ggen receipt; igniter `git diff --exit-code` → 0 |
| Live zcode / live BRCE | Chicago POWL court unmounted; BRCE broker unmounted; AS-01 crypto **UNSUPPORTED** | Mounted sibling courts + BRCE+AS-01 runtime receipt — fence alone is not ALIVE |
| Out-of-repo planes unmounted | CASTLE, Smalt, revops, Truex product admit | Tracked standing docs + exact sibling verifiers (see §4) |
| GA risk register open | R-01 … R-07 bodies | Typed receipt or typed refusal per risk before GA (contract fence ≠ body closure) |
| Swarm merge HITL | Multiple Gate-1 slices → #849 | Merge coordinator lands courts without founder re-authoring law; tip exit reconfirm |

Foundry finish is **not** “all crates compile.” It is **zero daily founder HITL** on the horizontal spine while preserving `SELECT ≠ CONSTRUCT ≠ DO`.

---

## 3. Next aggregate verifier (this ledger)

Replay cheapest high-information path first (all against tip `5e00619a422a0d8e451029f0aaf8b7a63e27da46` or successor; **tip-cargo PENDING** quiet cgroup before claiming tip exit 0):

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

# Sibling TS adapter still ABSENT in ggen (must stay empty)
find . -name 'ui-layout-court.ts' -not -path './target/*' -not -path './docs/*'
```

Out-of-repo next verifiers remain owned by sibling ledgers; ggen must **track** them without claiming their ALIVE standing. Consolidated Gate 0/2/3 index: [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md).

---

## 4. Cross-links (companions + swarm standing ledgers)

### 4.1 On PR #849 tip `5e00619a422a0d8e451029f0aaf8b7a63e27da46` (gate1 retip; prior foundry retip `5a39ea90a` / ui-layout absorb `0ae8d8108`)

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
- Truex product admission of `shui:` is a **sibling** claim; ggen fence PARTIAL ≠ Truex ALIVE.
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
FOUNDRY_FINISH = PARTIAL_ALIVE
  ∧ Gate1 ui-layout ALIVE  # evidence eaf0dad40; 8 passed; exit 0 (bc-c7c9d554); tip land 7c0ba2a90; tip source 11; tip-cargo PENDING on 5e00619a4
  ∧ ¬(igniter clean-room ALIVE)   # BLOCKED unmounted — do not invent ALIVE
  ∧ ¬(Smalt / revops ALIVE)       # UNKNOWN/BLOCKED — do not invent ALIVE
  ∧ ¬(live zcode / sibling ui-layout-court.ts / live BRCE ALIVE)
  ∧ ¬(out-of-repo residuals fully tracked to receipts)
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
| Evidence | Evidence tip `eaf0dad40`; ggen `ui_layout_court` **ALIVE** (**8 passed; exit 0**); tip court land `7c0ba2a90`; tip source **11**; **tip-cargo PENDING** on `5e00619a4`; sibling `ui-layout-court.ts` still ABSENT in ggen; smalt/castle unmounted → **BLOCKED**; revops DNS fail + cos `/readyz` 503 → **UNKNOWN**/**BLOCKED**; igniter clean-room **BLOCKED**; no Plane-5 / CASTLE / Smalt / revops / igniter / live zcode / live BRCE / Foundry crown claim asserted as `ALIVE` |
