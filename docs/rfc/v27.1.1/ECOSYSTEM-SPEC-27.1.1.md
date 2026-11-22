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
| `curve25519-dalek` v4 / cgap-aoq docs | ggen (+ crypto docs surface) | lock resolves 4.1.3 + 5.0.0; pin/policy doc + court [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md) → **ALIVE** (`curve25519_dalek_pin_court` EXIT 0 @ `bebea5dd0`) |
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

Standing on tip (post Gate-1 land): **PARTIAL_ALIVE** — workspace `taffy`
dependency + `ggen_graph::layout::taffy_aot` six-width seal +
`taffy_aot_preflight_court` (evidence tip `0478fde59`, exit 0). Photolithography /
zcode-cli consume of `AotSealReceipt` remains open (not path-crown `ALIVE`).
Exact refusal-code vocabulary still TBD in the Gate-1 seal; failures must not
be silently dropped. Historical materialization note: surface was
**UNSUPPORTED** before the Taffy land.

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

Standing on tip:

- Truex product admission of `shui:`: **ALIVE** — sibling Chicago admit-court
  **6/6 vitest exit 0** on `seanchatmangpt/truex` @ `68d2d2331696f18949f4877b994320018b6365e5`
  ([truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED** → `main`
  `b4672131`; mount `/tmp/truex`): `shui:` → **ADMITTED**; private `truex:` →
  **REFUSED_PRIVATE**; unknown → **REFUSED_UNKNOWN**. Replay:
  `pnpm exec vitest run test/admit/admit-court.test.ts` → exit **0**. See
  [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md).
- ggen public-prefix alignment: `vocab::shui` + `shui_public_prefix_court` —
  **ALIVE** (lineage tip-cargo) — **4 passed; exit 0** @ evidence `26256ca9d`
  (sources ≡ crown `678a6c106`; ancestor `0478fde59` **3 passed**). Fence ≠
  Truex product crown alone. See [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md).

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

Standing: **ALIVE** (lineage tip-cargo) — completeness **5** + rhythm **12**
EXIT **0** @ `c91b22561` (`/tmp/ggen-brand-toml-tip`; jobs=1; official
`cursor/v2711-brand-toml-tip-dee5` / bc-7f0c72b7;
[`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md));
hot-swap + fluid tip-cargo remain **ALIVE** (**6+6** @ `19c08a3b0`). Exact-tip
reconfirm SCHEDULED. Full DTCG color/type/CASTLE token-inventory plane
**UNSUPPORTED** until Gate 2+.

In-repo **brand.toml hot-swap** fence (`seed_hot_swap_slot` / `hot_swap_brand`
+ `brand_toml_hotswap_court`): **ALIVE** (lineage) — **6 passed; exit 0** @
`26256ca9d` (sources ≡ crown `678a6c106`). Admits Fourth Watch ↔ CindyCat
transitions under rhythm law; refuses unknown OEM ids / same-brand / rhythm
failures. Does **not** authorize production runtime theme swap or CASTLE publish
`ALIVE`.

Production theme promote (`promote_production_theme`) is fail-closed
**UNSUPPORTED** → typed `REFUSED_PRODUCTION` on the Gate-1 path (lib court
`layout::brand::production_theme_tests`). Exact next verifier to promote
production theme: [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md)
(G2-13 inventory → G2-12 CASTLE bind → production apply court).

---

## 9. zcode-cli ui-layout-court (D5)

`zcode-cli` SHALL provide `ui-layout-court.ts` that:

- consumes ggen Taffy AOT receipts;
- fails closed on missing / stale / mismatched layout digests;
- does not re-authorize DO.

Standing: zcode-cli adapter **UNSUPPORTED** in this repo (not a workspace
member). ggen in-repo `ui_layout_court` digest-consume fence is separately
**ALIVE** tip-cargo (**11 passed; exit 0** @ `ca52e9c16`; sources ≡ tip
`8cee4246f`) — fence ≠ sibling `.ts` `ALIVE`.

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
- Policy court present: `cargo test -p ggen-config --test curve25519_dalek_pin_court`
  refuses silent major drift (`REFUSED_UNDOCUMENTED_MAJOR`; see pin standing §5).

Standing: **ALIVE** (lock + pin doc + policy court EXIT 0 @ tip `bebea5dd0`).
  Crown crypto / AS-01 / Plane-5 remain **UNSUPPORTED** from this pin.

---

## 13. Crown objective bindings (photo / CCS·BRCE / ops / foundry)

These four crowns are **requirements bindings** on tip
`origin/cursor/ecosystem-spec-27-1-1-bc54`. Detailed hop/standing ledgers live in
the companion artifacts named below. Presence of this section does **not**
promote any crown to `ALIVE`.

### 13.1 Photolithography path

Hop chain (aggregate standing **PARTIAL** / path crown **non-ALIVE**):

```text
perimeter → .ttl/.toml → ggen+Taffy AOT → ui-layout digest consume
  → ggen_igniter clean-room → zcode-cli POWL/OCEL (bounded)
```

| Binding | Source of truth |
|---------|-----------------|
| Hop table + falsifiers | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) |
| Hop 4 contract | [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) |
| Hop 5 contract | [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) |
| Gate-1 layout fences | §6 Taffy; §9 zcode-cli; [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md) |

Do not invent path-crown `ALIVE` from in-repo fence green alone.

### 13.2 CCS / BRCE + WebAuthn AS-01

- Constitutional law: `SELECT ≠ CONSTRUCT ≠ DO`; sole CONSTRUCT→DO broker is
  **BRCE + WebAuthn AS-01** (`:prime256v1`) — §1.1 / Plane 5.
- ~8 Crystallized Control Surfaces (CCS), CP17 hash-gate law, and plane map:
  [`CCS-AUTHORITY-STANDING.md`](./CCS-AUTHORITY-STANDING.md).
- In-repo AS-01 ungated-DO seat fence vs live broker / crypto:
  [`BRCE-WEBAUTHN-STANDING.md`](./BRCE-WEBAUTHN-STANDING.md)
  (fence **PARTIAL_ALIVE**; WebAuthn/`prime256v1` crypto **UNSUPPORTED**; live
  BRCE broker **BLOCKED**).

Fence presence ≠ Plane-5 DO `ALIVE`.

### 13.3 Ops surfaces (Gate 0 + economic ops + Plane 5)

| Surface | Standing ceiling (ggen) | Ledger |
|---------|-------------------------|--------|
| GCP billing / cos ledger / servengine M4 / year-end SOW | **BLOCKED** / **UNKNOWN** — OUT OF SCOPE | [`GATE-0-STANDING.md`](./GATE-0-STANDING.md) |
| Smalt 4-cloud RAID (Azure·AWS·GCP·Twilio) | **ALIVE** (6-column spine @ `875efbb87`/`91997da76f0f` + residual b4b treasury/tax/erp/rates exits **0,0,0,0** @ `91997da76f0f` — 8/8/8/7 passed; `bc-c700dafb`) — ≠ Foundry / zero-HITL crown | [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) §4.3/§6b |
| `revops.fnox.io` / `cos.fnox.io` / CindyCat·Fourth Watch front doors | **UNKNOWN** / **BLOCKED** — DO only via BRCE+AS-01 | [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) |
| Exact external next verifiers | OOR Gate 0/2/3 rows | [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md) |

Commercial ops priority (CindyCat-first) is Gate 2 doctrine, not ops `ALIVE`:
[`GATE-2-CINDYCAT-FIRST.md`](./GATE-2-CINDYCAT-FIRST.md).

### 13.4 Foundry finish (horizontal spine)

Crown claim: spine
`ggen → Taffy → zcode-cli → CASTLE → Smalt → revops` runs without **daily
founder HITL**.

| Binding | Source of truth |
|---------|-----------------|
| Spine table + HITL residuals + GA falsifiers | [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) |
| CASTLE publish / brand spine | [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) |
| Release courts RC-D1…RC-D7 | [`GATE-3-RELEASE-COURTS-STANDING.md`](./GATE-3-RELEASE-COURTS-STANDING.md) |

Crown standing on tip: **PARTIAL_ALIVE** (incomplete). Zero daily founder HITL
is **not** `ALIVE`. Do not invent Foundry crown from Gate-1 fence green, CASTLE
sibling courts, or Smalt 6-column spine `ALIVE` alone.

---

## 14. Non-goals for this ggen land

- Plane 5 hosting, cos/FinFab ledger mutation, GCP billing.
- Implementing Zero-DOM / WebGPU / FFB renderers inside ggen.
- Publishing CASTLE or brand token inventories without admitted sources.
- Claiming Truex product `ALIVE` from ontology catalogue mirrors alone.
- Promoting photo / CCS·BRCE / ops / Foundry crowns to `ALIVE` from §13 prose
  or companion presence alone.

---

## 15. Companion artifacts

| Path | Role |
|------|------|
| `docs/rfc/v27.1.1/REPO-INVENTORY-27.1.1.md` | path + standing census |
| `docs/rfc/v27.1.1/GATE-0-STANDING.md` | Gate 0 billing / cos / servengine / SOW standing + stub citations |
| `docs/rfc/v27.1.1/GATE-1-GAP-MATRIX.md` | requirement → evidence → verifier → owner |
| `docs/rfc/v27.1.1/GATE-2-CINDYCAT-FIRST.md` | Gate 2 CindyCat-first commercial priority |
| `docs/rfc/v27.1.1/GATE-3-RELEASE-COURTS-STANDING.md` | RC-D1…RC-D7 release-court standing ledger |
| `docs/rfc/v27.1.1/R01-R07-STANDING.md` | R-01…R-07 resolution contract standing + next verifiers |
| `docs/rfc/v27.1.1/PHOTOLITHOGRAPHY-STANDING.md` | hop ledger: perimeter → ttl/toml → Taffy AOT → igniter clean-room → zcode POWL/OCEL (no fake ALIVE) |
| `docs/rfc/v27.1.1/FOUNDRY-FINISH-STANDING.md` | horizontal spine ggen→Taffy→zcode→CASTLE→Smalt→revops (zero daily founder HITL) |
| `docs/rfc/v27.1.1/OUT-OF-REPO-NEXT-VERIFIERS.md` | Gate 0/2/3 exact next external verifiers |
| `docs/rfc/v27.1.1/BRCE-WEBAUTHN-STANDING.md` | BRCE + WebAuthn AS-01 (`:prime256v1`) exists vs BLOCKED/UNSUPPORTED + GA verifiers |
| `docs/rfc/v27.1.1/CCS-AUTHORITY-STANDING.md` | SELECT≠CONSTRUCT≠DO authority edges + A2A/BRCE fence census |
| `docs/rfc/v27.1.1/CURVE25519-DALEK-PIN-STANDING.md` | Gate-1 `curve25519-dalek` / `ed25519-dalek` lock pin + policy court (**ALIVE** @ tip `bebea5dd0`) |
| `docs/rfc/v27.1.1/VERIFY-WITNESS-PRODUCTION.md` | production `verify_witness` WASM build path (affidavit pin ALIVE ≠ ggen prod ALIVE) |
| `docs/rfc/v27.1.1/VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md` | affidavit gatekeeper size court ALIVE @ `563eb322a` (ggen wire UNSUPPORTED) |
| `docs/rfc/v27.1.1/VERIFY-WITNESS-SIZE-COURT-STANDING.md` | ggen placeholder size court tip-cargo ALIVE (≠ production) |
| `docs/rfc/v27.1.1/BRAND-PRODUCTION-THEME.md` | brand.toml production theme `REFUSED_PRODUCTION` fence + next verifier (≠ hot-swap ALIVE) |
| `docs/rfc/v27.1.1/IGNITER-CLEANROOM-STANDING.md` | ggen_igniter clean-room contract (hop 4) |
| `docs/rfc/v27.1.1/ZCODE-POWL-OCEL-STANDING.md` | zcode POWL/OCEL bounded remediation (hop 5) |
| `docs/rfc/v27.1.1/CASTLE-SPINE-STANDING.md` | CASTLE publish / brand spine (Gate 2–3) |
| `docs/rfc/v27.1.1/SMALT-RAID-STANDING.md` | Smalt 4-cloud RAID economic ops standing |
| `docs/rfc/v27.1.1/REVOPS-COS-STANDING.md` | revops.fnox.io / cos.fnox.io Plane-5 standing |
| `docs/rfc/v27.1.1/CINDYCAT-REVERSE-GLOO.md` | CindyCat Reverse Gloo deal pack |
| `docs/rfc/v27.1.1/LINKEDIN-DEAL-PLAYBOOK.md` | LinkedIn DM / content calendar companion to Gate-2 CindyCat-first |
| `docs/rfc/v27.1.1/INTEGRATE-849-RECEIPT.md` | PR #849 Gate-1 integrate receipt (historical evidence pin) |

---

## 16. Provenance

Materialized from the Sovereign Ecosystem v27.1.1 mandate delivered to the
executing agent (Document ID `ECOSYSTEM-SPEC-27.1.1`, superseding
`ECOSYSTEM-SPEC-26.11.20`). Inventory standings were observed against
`origin/main` tip at branch creation; tip standing refresh on
`cursor/v2711-ecospec-fill-bc54` (parent `origin/cursor/ecosystem-spec-27-1-1-bc54`
@ `8f0a60ea1` — tip `5480d7ada+` / OOR retip4; CASTLE sibling spine courts
**ALIVE** @ `5480d7ada`; Truex product admit **PARTIAL_ALIVE** @ `d0e3ba473`).
Crown-objective body bind (§13 photo / CCS·BRCE / ops / foundry) + companion
index completeness on `cursor/v2711-ecospec-audit-bc54` against tip
`d11758cf4182181991ec3158e073b75b474d5cbe`.
No Plane 5 / cos / GCP / Foundry / photolithography claim is asserted as `ALIVE`.
