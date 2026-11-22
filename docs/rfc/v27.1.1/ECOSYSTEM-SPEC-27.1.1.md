# ECOSYSTEM-SPEC-27.1.1 — Sovereign Ecosystem PRD/ARD

| Field | Value |
|-------|-------|
| Document ID | `ECOSYSTEM-SPEC-27.1.1` |
| Supersedes | `ECOSYSTEM-SPEC-26.11.20` |
| Status | MATERIALIZED (authoritative requirements; not an execution receipt) |
| Target GA | **2027-01-01** |
| Materialized | 2026-11-22 |
| Repo scope of this land | `seanchatmangpt/ggen` (sibling repos named where ownership falls outside) |

Standing vocabulary for claims in this document and its companion inventory:
`UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

This document is a requirements / architecture binding. Source presence, workflow
definitions, and historical prose are never promoted to `ALIVE`.

---

## 1. Mandate

### 1.1 Operating doctrine

- **TAI** 555→1 (collapse ambient human-in-the-loop surface to a single admitted
  authority edge).
- **F = 0** (zero fabricated standing; no ambient DO).
- Quality: **DfLSS Cpk ≥ 2.0**, **γ = 0**.
- Authority separation (constitutional):

  ```text
  SELECT ≠ CONSTRUCT ≠ DO
  ```

- Sole lawful **CONSTRUCT → DO** gate: **BRCE + WebAuthn AS-01**.
  Raw input, model output, generated code, proof text, hooks, and semantic
  derivations have **no ambient execution authority**.

### 1.2 Manufacture identity

```text
A = μ(O*)
R = receipt(A)
```

`O*` is admitted, aligned, grounded, and bounded observation. `μ` is lawful
manufacture. A receipt binds subject identity, authority, consequence, replay,
and standing.

---

## 2. Core release delta vs ECOSYSTEM-SPEC-26.11.20

| # | Delta | Primary owners | GA posture |
|---|-------|----------------|------------|
| D1 | Deterministic zero-HITL mobile-first UIs — **Zero-DOM WebGPU canvas Mode A**; **FFB Mode B** | UI plane / sibling UI repos; ggen emits layout + brand tokens | required for GA |
| D2 | **Taffy AOT layout pre-flight** in ggen (widths `320 / 375 / 414 / 768 / 1024 / 1440` + WCAG 1.4.12 text spacing at **1.5×**) | **ggen** | Gate 1 seal |
| D3 | **Truex** admits W3C SHACL 1.2 **`shui:`** (`http://www.w3.org/ns/shacl-ui/`) | **truex** (admission); **ggen** (public-prefix / vocab fence alignment) | Gate 1 |
| D4 | **DTCG 2025.10** brand manifests (`.toml`): `fourth-watch.brand.toml`, `cindycat.brand.toml` | brand plane + ggen schema/admission | Gate 2+ |
| D5 | **zcode-cli** `ui-layout-court.ts` adapter | **zcode-cli** | Gate 1 |
| D6 | Plane 5 front doors: `revops.fnox.io`, cos/FinFab, Fourth Watch + CindyCat | Plane 5 / hosting — **out of ggen DO** | Gate 2–3 |
| D7 | Unresolved **R-01 … R-07** must close before GA | multi-repo; tracked below | hard GA gate |

---

## 3. Gate schedule

### Gate 0 — Nov 21 – Dec 5 (mostly OUT of ggen)

| Work | Owner | ggen standing |
|------|-------|---------------|
| GCP billing | infra / finance | **OUT OF SCOPE** — do not claim `ALIVE` from this repo |
| servengine M4 | servengine | **OUT OF SCOPE** |
| cos ledger fixes | cos | **OUT OF SCOPE** |
| year-end SOW | ops / commercial | **OUT OF SCOPE** |

### Gate 1 — Dec 6 – Dec 18 (**IN SCOPE for ggen + named siblings**)

| Work | Owner | Notes |
|------|-------|-------|
| `curve25519-dalek` v4 / cgap-aoq docs | ggen (+ crypto docs surface) | lock resolves 4.1.3 + 5.0.0; pin/policy doc [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md); policy court still open for `ALIVE` |
| Admit **`shui:`** in Truex | truex | ggen aligns public-prefix fences so `shui:` is not refused as private |
| Seal Rust/**Taffy** AOT pre-flight in ggen | **ggen** | widths + WCAG 1.4.12 1.5× |
| `ui-layout-court.ts` in zcode-cli | zcode-cli | adapter consumes ggen pre-flight receipts |
| `<250KB` `verify_witness` WASM | witness / affidavit plane | size court; ggen may emit or pack, not host runtime |

### Gate 2–3 — post Gate 1

| Work | Owner |
|------|-------|
| revops plane | revops / fnox |
| brand `.toml` manifests (Fourth Watch, CindyCat) | brand + ggen schema |
| CASTLE publish | CASTLE plane |
| 7 release courts | release governance |

---

## 4. Planes (architecture)

| Plane | Role | DO authority |
|-------|------|--------------|
| 0 — Observation | Select admitted `O*` | SELECT only |
| 1 — Manufacture (ggen) | `μ` — packs, graphs, layout pre-flight, brand projection | CONSTRUCT |
| 2 — Trust (Truex / Affidavit / receipts) | admit, verify, replay | CONSTRUCT; DO only via BRCE |
| 3 — Layout / UI courts | Taffy AOT + zcode-cli courts; Zero-DOM / FFB renderers | CONSTRUCT; runtime DO blocked without AS-01 |
| 4 — Witness WASM | `verify_witness` ≤ 250KB | verify / CONSTRUCT |
| 5 — Front doors | revops.fnox.io, cos/FinFab, Fourth Watch, CindyCat | **DO via BRCE+WebAuthn AS-01 only** |

Do **not** claim Plane 5 / cos / GCP billing `ALIVE` from ggen evidence.

---

## 5. UI modes (D1)

### Mode A — Zero-DOM WebGPU canvas

- Deterministic, zero-HITL, mobile-first.
- No ambient DOM mutation authority; canvas is the sole presentational surface.
- Layout geometry must be pre-admitted by Taffy AOT (D2) before paint.

### Mode B — FFB

- Alternate presentational path; same layout courts and brand manifests.
- Must not bypass Mode A’s admission or BRCE/AS-01.

Standing in this repo at materialization: **UNKNOWN** (no Zero-DOM / WebGPU / FFB
implementation under live crates; see inventory). Deterministic **zero-HITL**
is a GA requirement, not an earned `ALIVE` standing — do not promote D1 / Mode A
to `ALIVE` from doctrine prose.

---

## 6. Taffy AOT layout pre-flight (D2)

ggen SHALL expose a deterministic layout pre-flight that:

1. Evaluates candidate UI trees at widths:
   `320, 375, 414, 768, 1024, 1440` (CSS px).
2. Re-evaluates under WCAG **1.4.12** text spacing at **1.5×** (line height,
   letter/word spacing factors as specified by WCAG 1.4.12).
3. Emits a machine-readable refusal or receipt (overflow, clip, contrast-adjacent
   geometry failures as typed refusals — exact refusal codes TBD in Gate 1 seal,
   must not be silently dropped).
4. Is consumable by **zcode-cli** `ui-layout-court.ts` without re-implementing
   geometry law.

Standing at materialization: **UNSUPPORTED** in ggen (no Taffy dependency /
pre-flight crate surface).

---

## 7. Truex + `shui:` (D3)

W3C SHACL 1.2 User Interfaces defines:

| Prefix | Namespace |
|--------|-----------|
| `shui:` | `http://www.w3.org/ns/shacl-ui/` |

Truex SHALL admit `shui:` as a **public** vocabulary (not a project-private
namespace). ggen’s public-prefix / forbid-private fences MUST NOT refuse `shui:`
as if it were `truex:`, `gall:`, `gg:`, etc.

Historical note: ggen still treats **`truex:`** as a *private* / banned IRI token
in worktree sanitizers and ORIGINAL_REQUEST doctrine. That ban is about the
**project-private** `truex:` prefix, not about Truex-the-product admitting the
**W3C** `shui:` namespace. Do not conflate the two.

Standing at materialization:

- Truex repo admission of `shui:`: **UNKNOWN** (sibling `seanchatmangpt/truex`;
  not mounted in this workspace).
- ggen public-prefix alignment: see Gate-1 foundation slice in this PR
  (`vocab::shui` + admitted-prefix court) — **PARTIAL_ALIVE** after land.

---

## 8. DTCG 2025.10 brand manifests (D4)

Brand tokens SHALL ship as DTCG **2025.10**-aligned manifests in `.toml` form:

- `fourth-watch.brand.toml`
- `cindycat.brand.toml`

Gate-1 in-repo slice (this land): **visual rhythm only** — px/rem primitives,
4px/8px grid, fluid clamp/cqi slopes, Material 48 / Apple 44 touch floors,
8px inter-target gap — via `ggen_graph::layout::brand` + fixtures under
`docs/rfc/v27.1.1/fixtures/`. Full color/type/CASTLE inventories remain Gate 2+;
inventing those token values here is still **REFUSED**.

Standing: **PARTIAL_ALIVE** (rhythm schema + OEM stubs + Chicago court);
token-inventory plane **UNSUPPORTED** until Gate 2+.

In-repo **brand.toml hot-swap** fence (`seed_hot_swap_slot` / `hot_swap_brand`
+ `brand_toml_hotswap_court`): **PARTIAL_ALIVE** only — admits Fourth Watch ↔
CindyCat transitions under rhythm law; refuses unknown OEM ids / same-brand /
rhythm failures. Does **not** authorize production runtime theme swap or CASTLE
publish `ALIVE`.

---

## 9. zcode-cli ui-layout-court (D5)

`zcode-cli` SHALL provide `ui-layout-court.ts` that:

- consumes ggen Taffy AOT receipts;
- fails closed on missing / stale / mismatched layout digests;
- does not re-authorize DO.

Standing: **UNSUPPORTED** in this repo (zcode-cli not a workspace member).

---

## 10. Witness WASM (Gate 1)

`verify_witness` WASM artifact MUST be **&lt; 250KB**. Owner may be affidavit /
Truex / witness plane; ggen may pack or document but must not claim runtime
`ALIVE` without a size court receipt.

Standing in ggen: **PARTIAL_ALIVE** for Affidavit *pack manufacture*
(`packs/affidavit-pack`) and for the in-repo size court
(`ggen_graph::witness` + `verify_witness_size_court`); production WASM
runtime remains **UNSUPPORTED** until the out-of-repo build path in
`VERIFY-WITNESS-PRODUCTION.md` yields a courted artifact.

---

## 11. Unresolved risks R-01 … R-07

These identifiers are **hard GA gates**. Exact prose for each risk was not
mounted as a sibling artifact in this checkout; they remain **OPEN / UNKNOWN**
and must be closed with typed evidence before GA 2027-01-01.

| ID | Standing | Closure rule |
|----|----------|--------------|
| R-01 | OPEN / UNKNOWN | typed receipt or typed refusal before GA |
| R-02 | OPEN / UNKNOWN | typed receipt or typed refusal before GA |
| R-03 | OPEN / UNKNOWN | typed receipt or typed refusal before GA |
| R-04 | OPEN / UNKNOWN | typed receipt or typed refusal before GA |
| R-05 | OPEN / UNKNOWN | typed receipt or typed refusal before GA |
| R-06 | OPEN / UNKNOWN | typed receipt or typed refusal before GA |
| R-07 | OPEN / UNKNOWN | typed receipt or typed refusal before GA |

Do not invent risk bodies. Materialize risk text from the admitted risk register
when it is supplied; until then, treat “closed” claims as **REFUSED_UNGROUNDED**.

---

## 12. Crypto / docs (Gate 1)

- Workspace depends on `ed25519-dalek` 3.x; Cargo.lock resolves
  `curve25519-dalek` **4.1.3** and **5.0.0** transitively.
- Gate 1 pin/policy standing:
  [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md)
  (exact versions, dependents, refuse undocumented major).
- Remaining falsifier for crown `ALIVE`: a policy court that refuses silent
  major drift on the admitted path (see pin standing §5).

Standing: **PARTIAL_ALIVE** (lock + pin doc present; policy court absent).

---

## 13. Non-goals for this ggen land

- Plane 5 hosting, cos/FinFab ledger mutation, GCP billing.
- Implementing Zero-DOM / WebGPU / FFB renderers inside ggen.
- Publishing CASTLE or brand token inventories without admitted sources.
- Claiming Truex product `ALIVE` from ontology catalogue mirrors alone.

---

## 14. Companion artifacts

| Path | Role |
|------|------|
| `docs/rfc/v27.1.1/REPO-INVENTORY-27.1.1.md` | path + standing census |
| `docs/rfc/v27.1.1/GATE-0-STANDING.md` | Gate 0 billing / cos / servengine / SOW standing + stub citations |
| `docs/rfc/v27.1.1/GATE-1-GAP-MATRIX.md` | requirement → evidence → verifier → owner |
| `docs/rfc/v27.1.1/R01-R07-STANDING.md` | R-01…R-07 resolution contract standing + next verifiers |
| `docs/rfc/v27.1.1/PHOTOLITHOGRAPHY-STANDING.md` | hop ledger: perimeter → ttl/toml → Taffy AOT → igniter clean-room → zcode POWL/OCEL (no fake ALIVE) |
| `docs/rfc/v27.1.1/BRCE-WEBAUTHN-STANDING.md` | BRCE + WebAuthn AS-01 (`:prime256v1`) exists vs BLOCKED/UNSUPPORTED + GA verifiers |
| `docs/rfc/v27.1.1/CURVE25519-DALEK-PIN-STANDING.md` | Gate-1 `curve25519-dalek` / `ed25519-dalek` lock pin + policy (PARTIAL_ALIVE) |

---

## 15. Provenance

Materialized from the Sovereign Ecosystem v27.1.1 mandate delivered to the
executing agent (Document ID `ECOSYSTEM-SPEC-27.1.1`, superseding
`ECOSYSTEM-SPEC-26.11.20`). Inventory standings were observed against
`origin/main` tip at branch creation. No Plane 5 / cos / GCP claim is asserted
as `ALIVE`.
