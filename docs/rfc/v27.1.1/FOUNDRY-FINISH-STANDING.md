# FOUNDRY-FINISH-STANDING — Horizontal spine without daily founder HITL

| Field | Value |
|-------|-------|
| Document ID | `FOUNDRY-FINISH-STANDING` |
| Companion to | `ECOSYSTEM-SPEC-27.1.1` (target GA **2027-01-01**) |
| Status | STANDING LEDGER (not an execution receipt; not a Gate-1 rewrite) |
| Observed base | `origin/main` @ `d173cad5792e837bd4fec730d120c582b9bc5d4c` (2026-11-22) |
| Gate-1 evidence tip | PR [#849](https://github.com/seanchatmangpt/ggen/pull/849) / `cursor/ecosystem-spec-27-1-1-bc54` (Taffy / brand / `shui:` PARTIAL_ALIVE) |
| Repo scope | `seanchatmangpt/ggen` names the spine; sibling planes are **tracked out-of-repo** |

Standing vocabulary (AGENTS.md / ECOSYSTEM-SPEC):
`UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Authority rule.** This document is the **source of truth for horizontal Foundry-finish spine standing**. It does **not** rewrite `GATE-1-GAP-MATRIX.md`. Gate-1 keeps requirement → evidence → verifier → owner rows; this ledger answers whether the full spine can run **without daily founder HITL**.

**Crown claim (Foundry finish).** Horizontal spine
`ggen → Taffy → zcode-cli → CASTLE → Smalt → revops` runs as an admitted, receipt-bound loop with **no ambient daily founder HITL**.

| Crown claim standing | Reason |
|----------------------|--------|
| **PARTIAL_ALIVE** | In-repo Gate-1 fences advancing on #849; photolithography consume, CASTLE publish, Smalt RAID, and Plane-5 revops remain out-of-repo / non-ALIVE. **Not complete** until Gate-1 aggregate is `ALIVE` **and** out-of-repo residual is tracked to typed receipts or typed refusals. |

Do not promote source presence, fleet-census rows, workflow definitions, or historical jira prose to `ALIVE`.

---

## 1. Horizontal spine table

| Component | Role on spine | Standing | HITL residual (founder) | Next exact verifier | GA `2027-01-01` falsifier |
|-----------|---------------|----------|-------------------------|---------------------|---------------------------|
| **ggen** | Manufacture kernel (`μ`): packs, graphs, layout pre-flight, brand rhythm, public-prefix fences | **PARTIAL_ALIVE** — live workspace; Gate-1 `shui:` / Taffy / brand.toml fences land on #849 as PARTIAL; full zero-HITL fab not sealed | Daily merge/admission judgment across swarm PRs; R-01…R-07 still open; founder still gates “promote Gate-1 → GA” | In-repo: `cargo test -p ggen-graph --test taffy_aot_preflight_court`; `cargo test -p ggen-graph --test brand_toml_rhythm_court`; `cargo test -p ggen-graph --test shui_public_prefix_court` (on #849 tip); replay `just check` | If on **2027-01-01** a founder must still manually admit ordinary spine manufacture (SELECT/CONSTRUCT) that courts already cover, crown claim is **falsified** |
| **Taffy** | AOT layout pre-flight (widths `320/375/393/600/840/1280` + WCAG 1.4.12 @ 1.5×) inside `ggen-graph::layout::taffy_aot` | **PARTIAL_ALIVE** — six-width seal court green on #849 (`preflight_column_six_widths`); photolithography not ALIVE until a consumer court binds the receipt | Founder still wires / retries zcode or igniter consume when receipt law drifts; no hands-free photolithography loop | `cargo test -p ggen-graph --test taffy_aot_preflight_court` | If on **2027-01-01** geometry law still requires founder-edited exceptions instead of typed `LayoutRefusal` / seal receipts, Taffy row is **falsified** |
| **zcode-cli** | `ui-layout-court.ts` consumes ggen `AotSealReceipt`; fails closed on missing/stale digests | **UNSUPPORTED** in this repo (not a workspace member; no `ui-layout-court.ts` on disk) | Founder must author/land adapter in sibling repo and re-run courts; ggen cannot self-prove consume | In **zcode-cli**: `pnpm test -- ui-layout-court` (or that repo’s exact court runner) against a ggen layout-receipt fixture | If on **2027-01-01** UI courts still need founder to paste/override layout digests, zcode row is **falsified** |
| **CASTLE** | Brand / token inventory publish plane (Gate 2–3); full DTCG color/type inventories beyond Gate-1 rhythm stubs | **UNKNOWN** — no CASTLE publish/court surface in ggen; OEM rhythm stubs only (`docs/rfc/v27.1.1/fixtures/*.brand.toml` on #849) | Founder still invents or hand-publishes token inventories without admitted CASTLE sources (explicitly **REFUSED** in ECOSYSTEM-SPEC non-goals) | CASTLE-plane publish court + brand inventory replay (out of ggen DO); until mounted, standing stays UNKNOWN | If on **2027-01-01** CASTLE publish still depends on daily founder token edits with no receipt, CASTLE row is **falsified** |
| **Smalt** | Sibling control / RAID / posh membrane plane (Elixir+Go surfaces historically cited in v26.11.4 closure docs) | **UNKNOWN** in this checkout — sibling `/Users/sac/smalt` not mounted; fleet census does not list a live Smalt product row as ALIVE | Founder still runs Smalt gate / KD / appconfig waves by hand (historical HITL); no ggen-admitted hands-free RAID receipt | Out-of-repo: Smalt gate suite / posh KD register courts in the Smalt repo (track tip SHA + exit 0); do not claim ALIVE from ggen jira citations alone | If on **2027-01-01** Smalt RAID still requires daily founder HITL to keep the membrane green, Smalt row is **falsified** |
| **revops** | Plane-5 front door (`revops.fnox.io` and related hosting) — sole DO via **BRCE + WebAuthn AS-01** | **UNKNOWN** / **OUT OF SCOPE** for ggen DO — fleet census names `seanchatmangpt/revops` / `chatmangpt-org/revops` only | Founder still actuates Plane-5 without AS-01; BRCE doctrine is PARTIAL as text, runtime AS-01 **UNKNOWN** in ggen | Out-of-repo: revops / fnox Plane-5 court that proves BRCE+AS-01 admits DO and refuses ambient founder actuation; never promote from ggen alone | If on **2027-01-01** Plane-5 still depends on daily founder HITL for ordinary DO, revops row (and crown claim) is **falsified** |

---

## 2. HITL residual summary (why daily founder is still required)

| Residual class | Where it bites | Clearance condition |
|----------------|----------------|---------------------|
| Gate-1 photolithography open | Taffy receipt not consumed by zcode-cli / igniter | Sibling `ui-layout-court` (or igniter) exit 0 on a pinned ggen receipt |
| Out-of-repo planes unmounted | CASTLE, Smalt, revops, Truex product admit | Tracked standing docs + exact sibling verifiers (see §4) |
| Authority edge incomplete | BRCE + WebAuthn AS-01 runtime | CCS/BRCE standing ledger + runtime court — doctrine text alone is not ALIVE |
| GA risk register open | R-01 … R-07 | Typed receipt or typed refusal per risk before GA |
| Swarm merge HITL | Multiple Gate-1 slices → #849 | Merge coordinator lands courts without founder re-authoring law |

Foundry finish is **not** “all crates compile.” It is **zero daily founder HITL** on the horizontal spine while preserving `SELECT ≠ CONSTRUCT ≠ DO`.

---

## 3. Next aggregate verifier (this ledger)

Replay cheapest high-information path first:

```text
# Gate-1 in-repo fences (require #849 / ecosystem-spec tip, not bare main)
cargo test -p ggen-graph --test taffy_aot_preflight_court
cargo test -p ggen-graph --test brand_toml_rhythm_court
cargo test -p ggen-graph --test shui_public_prefix_court

# This ledger’s presence / cross-link integrity (always)
test -f docs/rfc/v27.1.1/FOUNDRY-FINISH-STANDING.md
rg -n 'GA `2027-01-01` falsifier|HITL residual' docs/rfc/v27.1.1/FOUNDRY-FINISH-STANDING.md
```

Out-of-repo next verifiers remain owned by sibling ledgers; ggen must **track** them without claiming their ALIVE standing.

---

## 4. Cross-links (companions + swarm standing ledgers)

### 4.1 Landed / in-flight on PR #849 tree

| Path | Role vs this ledger |
|------|---------------------|
| [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) | Requirements / architecture binding; defines GA **2027-01-01** and Gate schedule |
| [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md) | Gate-1 requirement matrix — **not rewritten here**; Foundry finish is horizontal spine source |
| [`REPO-INVENTORY-27.1.1.md`](./REPO-INVENTORY-27.1.1.md) | Path census + component standing seeds for Taffy / zcode / CASTLE / revops |

Paths above materialize on `cursor/ecosystem-spec-27-1-1-bc54` / PR #849; this branch may land the Foundry ledger ahead of that merge. Broken relative links until merge are **expected**, not evidence of ALIVE.

### 4.2 Parallel standing ledgers (swarm; track even if not yet on tip)

Prefer these paths when present under `docs/rfc/v27.1.1/`:

| Expected path | Spine / plane coverage |
|---------------|------------------------|
| `PHOTOLITHOGRAPHY-STANDING.md` | Taffy → zcode/igniter consume |
| `ZCODE-POWL-OCEL-STANDING.md` (or `ZCODE-STANDING.md`) | zcode-cli courts / POWL·OCEL |
| `CASTLE-SPINE-STANDING.md` (or `CASTLE-STANDING.md`) | CASTLE publish |
| `SMALT-RAID-STANDING.md` | Smalt RAID / membrane |
| `REVOPS-COS-STANDING.md` | revops + cos Plane-5 |
| `GATE-0-BILLING-SOW-STANDING.md` | Gate 0 out-of-ggen residuals |
| `GATE-3-RELEASE-COURTS-STANDING.md` | Gate 3 release courts |
| `BRCE-WEBAUTHN-AS01-STANDING.md` | DO authority edge |
| `CCS-AUTHORITY-STANDING.md` | CCS authority matrix |
| `GGEN-IGNITER-STANDING.md` | igniter clean-room consume |

If a sibling ledger disagrees with the spine table in §1, **do not silently merge standings**. Record the conflict as `UNKNOWN` / contradictory evidence until one court wins.

### 4.3 Related in-repo doctrine (not spine ALIVE)

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
- Affidavit pack manufacture PARTIAL ≠ `verify_witness` WASM &lt;250KB ALIVE.
- This ledger’s existence is **PARTIAL_ALIVE** documentation standing only — not Foundry-finish crown `ALIVE`.

---

## 6. Closure rule for crown ALIVE

Promote Foundry-finish crown claim to `ALIVE` only when **all** hold:

1. Gate-1 aggregate rows that own ggen fences are `ALIVE` (or typed `REFUSED_*` with remediation closed) per `GATE-1-GAP-MATRIX.md`.
2. Every §1 spine component is either `ALIVE` or has an out-of-repo tracked standing doc with an exact next verifier and a dated receipt tip.
3. Daily founder HITL is no longer required for ordinary spine SELECT/CONSTRUCT; Plane-5 DO is BRCE+AS-01 only.
4. GA date **2027-01-01** falsifiers in §1 would not fire if evaluated that day.

Until then, overall goal standing remains:

```text
FOUNDRY_FINISH = PARTIAL_ALIVE
  ∧ ¬(Gate1 ALIVE)
  ∧ ¬(out-of-repo residuals fully tracked to receipts)
⇒ crown claim incomplete
```

---

## 7. Provenance

Materialized 2026-11-22 for the v27.1.1 Foundry-finish goal slice (branch
`cursor/v2711-foundry-finish-bc54`). Evidence drawn from ECOSYSTEM-SPEC-27.1.1 /
GATE-1-GAP-MATRIX / REPO-INVENTORY on the #849 tip, AGENTS.md standing vocabulary,
fleet census naming of revops/truex, and explicit non-mount of Smalt/CASTLE/zcode-cli
in this workspace. No Plane-5 / CASTLE / Smalt / zcode claim is asserted as `ALIVE`.
