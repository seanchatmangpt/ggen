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

Standing on tip (post Gate-1 land): **ALIVE (exact tip-cargo)** —
`taffy_aot_preflight_court` **7**/EXIT:**0** @ `2685119bd`
([`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md)).
tip-cargo `ALIVE` ≠ live-tip reconfirm ≠ photolithography path-crown `ALIVE`
(path remains **PARTIAL** — [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md)).
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
  **6/6 vitest exit 0** reconfirmed @ `68d2d2331696f18949f4877b994320018b6365e5`
  ([truex#6](https://github.com/seanchatmangpt/truex/pull/6) **MERGED** → `main`
  `b4672131`; mount `/tmp/truex`; sidebranch `cursor/v2711-truex-exact-retip-bc54`
  on tip `a3e5d3109`): `shui:` → **ADMITTED**; private `truex:` →
  **REFUSED_PRIVATE**; unknown → **REFUSED_UNKNOWN**. Replay:
  `pnpm exec vitest run test/admit/admit-court.test.ts` → exit **0**. See
  [`TRUEX-SHUI-EXACT-RETIP-STANDING.md`](./TRUEX-SHUI-EXACT-RETIP-STANDING.md) /
  [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md).
- ggen public-prefix alignment: `vocab::shui` + `shui_public_prefix_court` —
  **ALIVE (exact tip-cargo)** — **4 passed; EXIT:0** @ cargo tip `a4b3d3fb4`
  ([`SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md);
  absorb `edf2ec22a` / tip3 absorb held). tip-cargo `ALIVE` ≠ live-tip
  (`15eefb628`+) reconfirm ≠ Truex product crown alone ≠ Foundry `ALIVE`.
  See [`TRUEX-SHUI-EXACT-RETIP-STANDING.md`](./TRUEX-SHUI-EXACT-RETIP-STANDING.md) /
  [`GATE1-REMAINING-FALSIFIERS.md`](./GATE1-REMAINING-FALSIFIERS.md).

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

Standing: completeness + rhythm **ALIVE (exact tip-cargo)** — **5+12** EXIT **0**
@ `77eb46fd0` ([`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md));
fluid lineage tip-cargo **ALIVE** (**6** @ `19c08a3b0`); hot-swap exact tip →
**COMPILING** / **PARTIAL_ALIVE** (EXIT **137** @ tip `cc2a4ea66`; lineage **ALIVE**
**6** @ `19c08a3b0` / absorb `cde02d1f2` retained; sources ≡;
[`BRAND-HOTSWAP-TIP-CARGO-STANDING.md`](./BRAND-HOTSWAP-TIP-CARGO-STANDING.md)).
Full DTCG color/type/CASTLE token-inventory plane **UNSUPPORTED** until Gate 2+.

In-repo **brand.toml hot-swap** fence (`seed_hot_swap_slot` / `hot_swap_brand`
+ `brand_toml_hotswap_court`): exact tip **COMPILING** / **PARTIAL_ALIVE** —
command `cargo test -p ggen-graph --test brand_toml_hotswap_court` EXIT **137**
(cgroup OOM) on tip `cc2a4ea66`; lineage **ALIVE** — **6 passed; exit 0** @
`19c08a3b0` (sources ≡ tip). Fence receipt ceiling **PARTIAL_ALIVE** (never the
string `ALIVE`). Admits Fourth Watch ↔ CindyCat transitions under rhythm law;
refuses unknown OEM ids / same-brand / rhythm failures. Does **not** authorize
production runtime theme swap or CASTLE publish `ALIVE`.

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

These identifiers are **hard GA gates**. Body prose is **admitted** from the
sovereign risk-register mount
[`fixtures/r01-r07-sovereign-risk-register.md`](./fixtures/r01-r07-sovereign-risk-register.md)
(verbatim §4 of `zcode-cli` `docs/ecosystem-spec-27.1.1.md` @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a`).
Do not invent risk bodies. “Closed” without an admitted body is
**REFUSED_UNGROUNDED**. Body admission ≠ closure — open rows still need typed
receipt or typed refusal before GA 2027-01-01.

| ID | Body | Closure standing | Closure rule (from admitted register) |
|----|------|------------------|----------------------------------------|
| R-01 | **ADMITTED** (`cos.fnox.io` ledger + `--seed 7`) | **OPEN** | Close 6 ledger fixes + seed race; 2× LOW-only skeptic waves |
| R-02 | **ADMITTED** (FFB Web Worker SAB arena) | **OPEN** | Lock `SharedArrayBuffer` `initial === maximum` |
| R-03 | **ADMITTED** (FFB Transport WebTransport/QUIC) | **OPEN** | Align `h3` edge-proxy to `ALPN: aoq/1` + UDP-443 |
| R-04 | **ADMITTED** (FFB Auth WebCrypto channel-bind) | **OPEN** | Non-extractable WebCrypto ↔ SessionActor |
| R-05 | **ADMITTED** (Affidavit ML-DSA / PQC seal stride) | **OPEN** | Lock Epoch Lease anchor + `expected_byte_length` |
| R-06 | **ADMITTED** (`zcode-cli` bounded remediation) | **CLOSED** | Evidence: commits `0cff19a`/`7b2c181` + `bun test test/powl-ocel-chicago.test.ts` → **11**/EXIT:**0** @ zcode `f32df08` |
| R-07 | **ADMITTED** (Billing rails / hold `018FD7…`) | **OPEN** | Clear GCP billing hold; stand up Stripe/Mercury/Brex rails |

Treat invented “closed” claims as **REFUSED_UNGROUNDED**. typed receipt or typed refusal before GA remains the closure rule for every **OPEN** row.

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
| Path hop ledger (honesty) | [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md) |
| Hop table + falsifiers | [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md) |
| Hop 4 contract | [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) |
| Hop 5 contract | [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) |
| Gate-1 layout fences | §6 Taffy; §9 zcode-cli; [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md) |

Do not invent path-crown `ALIVE` from in-repo fence green alone.

### 13.2 CCS / BRCE + WebAuthn AS-01

- Constitutional law: `SELECT ≠ CONSTRUCT ≠ DO`; sole CONSTRUCT→DO broker is
  **BRCE + WebAuthn AS-01** (`:prime256v1`) — §1.1 / Plane 5.
- Clause-2 honesty binder (CCS inventory + BRCE **V8**/bodies + WebAuthn +
  SELECT≠CONSTRUCT≠DO enforcement + exact next verifiers):
  [`CCS-BRCE-WEBAUTHN-STANDING.md`](./CCS-BRCE-WEBAUTHN-STANDING.md)
  (crown **PARTIAL_ALIVE**; **V8**/bodies **BLOCKED**; live WebAuthn
  **UNSUPPORTED**; **no** Foundry / tip-cargo EXIT0 / WebAuthn `ALIVE`).
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
| O(N)→O(N²) ops honesty (brand prod / Smalt / VW size / cos / revops) | **PARTIAL** aggregate — prod theme **UNSUPPORTED**; Smalt **ALIVE (lineage)** ≠ Foundry; VW measure **ALIVE** / court **PARTIAL**; cos **BLOCKED**; revops **UNKNOWN** | [`OPS-SURFACES-ON2-STANDING.md`](./OPS-SURFACES-ON2-STANDING.md) |
| Exact external next verifiers | OOR Gate 0/2/3 rows | [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md); [`GATE023-OOR-STANDING-MATRIX.md`](./GATE023-OOR-STANDING-MATRIX.md) |

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
| `docs/rfc/v27.1.1/PERIMETER-TTL-TOML-PATH-STANDING.md` | tip-wins absorb perimeter→ttl/toml path honesty (`46c347281` onto `8629691f8`; hops 1–2 ALIVE; path crown PARTIAL; `--live` BLOCKED_HITL; no Foundry ALIVE) |
| `docs/rfc/v27.1.1/FOUNDRY-FINISH-STANDING.md` | horizontal spine ggen→Taffy→zcode→CASTLE→Smalt→revops (zero daily founder HITL) |
| `docs/rfc/v27.1.1/FOUNDRY-FINISH-SPINE-STANDING.md` | Foundry finish horizontal spine honesty (PARTIAL / not daily-HITL-free + `--live` BLOCKED_HITL; Gate1 trio ≠ Foundry ALIVE) |
| `docs/rfc/v27.1.1/FOUNDRY-HITL-ACTION-CARD.md` | Founder `--live` action card + finish honesty retip (§5: Gate1 ALIVE trio ≠ Foundry ≠ zero-HITL; spine PARTIAL; PASS only `foundry_exit=0`) |
| `docs/rfc/v27.1.1/OUT-OF-REPO-NEXT-VERIFIERS.md` | Gate 0/2/3 exact next external verifiers |
| `docs/rfc/v27.1.1/GATE023-OOR-STANDING-MATRIX.md` | Gate 0/2/3 OOR standing honesty matrix through GA 2027-01-01 (tip pin `4060f9d6b` ⊆ `f0a0e07bc`; no invented ALIVE) |
| `docs/rfc/v27.1.1/OPS-SURFACES-ON2-STANDING.md` | O(N)→O(N²) ops surfaces honesty (brand prod / Smalt / VW / cos / revops) |
| `docs/rfc/v27.1.1/ECOSYSTEM-SPEC-27.1.1-MATERIALIZE-STANDING.md` | Materialize-verify receipt @ tip `f0a0e07bc` (docs-only; binder index gaps closed; no crown ALIVE) |
| `docs/rfc/v27.1.1/BRCE-WEBAUTHN-STANDING.md` | BRCE + WebAuthn AS-01 (`:prime256v1`) exists vs BLOCKED/UNSUPPORTED + GA verifiers |
| `docs/rfc/v27.1.1/CCS-BRCE-WEBAUTHN-STANDING.md` | Clause-2 binder: ~8 CCS inventory + BRCE V8/bodies BLOCKED + WebAuthn UNSUPPORTED + SELECT≠CONSTRUCT≠DO enforcement |
| `docs/rfc/v27.1.1/CCS-AUTHORITY-STANDING.md` | SELECT≠CONSTRUCT≠DO authority edges + A2A/BRCE fence census |
| `docs/rfc/v27.1.1/CURVE25519-DALEK-PIN-STANDING.md` | Gate-1 `curve25519-dalek` / `ed25519-dalek` lock pin + policy court (**ALIVE** @ tip `bebea5dd0`) |
| `docs/rfc/v27.1.1/VERIFY-WITNESS-PRODUCTION.md` | production `verify_witness` WASM build path (affidavit pin ALIVE ≠ ggen prod ALIVE) |
| `docs/rfc/v27.1.1/VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md` | affidavit gatekeeper size court ALIVE @ `563eb322a` (ggen wire UNSUPPORTED) |
| `docs/rfc/v27.1.1/VERIFY-WITNESS-SIZE-COURT-STANDING.md` | ggen placeholder size court tip-cargo ALIVE (≠ production) |
| `docs/rfc/v27.1.1/BRAND-HOTSWAP-TIP-CARGO-STANDING.md` | brand.toml hot-swap **ALIVE (exact tip-cargo)** **6**/EXIT:**0** @ `c4e757a14` (fence ceiling **PARTIAL_ALIVE**; ≠ production / Foundry) |
| `docs/rfc/v27.1.1/BRAND-TOML-TIP-CARGO-STANDING.md` | brand.toml completeness+rhythm **ALIVE (exact tip-cargo)** **5+12**/EXIT:**0** @ `2685119bd` / tip6 `cfd93c1cb` (≠ live-tip reconfirm / Foundry) |
| `docs/rfc/v27.1.1/TAFFY-AOT-PREFLIGHT-STANDING.md` | Taffy AOT preflight **ALIVE (exact tip-cargo)** **7**/EXIT:**0** @ `2685119bd` (≠ path-crown / Foundry) |
| `docs/rfc/v27.1.1/UILAYOUT-EXACT-RETIP5-STANDING.md` | `ui_layout_court` **ALIVE (exact tip-cargo)** **11**/EXIT:**0** @ `2685119bd` (≠ sibling `.ts` / Foundry) |
| `docs/rfc/v27.1.1/SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md` | `shui_public_prefix_court` **ALIVE (exact tip-cargo)** **4**/EXIT:**0** @ `a4b3d3fb4` (≠ live-tip reconfirm / Foundry) |
| `docs/rfc/v27.1.1/GATE1-REMAINING-FALSIFIERS.md` | Gate1 in-repo falsifiers tip-cargo **ALIVE** closed (Truex/brand/Taffy/uilayout/shui); Foundry **BLOCKED_HITL**; no Foundry ALIVE |
| `docs/rfc/v27.1.1/GATE1-EXACT-TIP-TIPWINS-ABSORB-STANDING.md` | tip-wins absorb Gate1 exact-tip EXIT0 sides onto crown (tip-cargo ≠ live-tip reconfirm; Foundry **BLOCKED_HITL**) |
| `docs/rfc/v27.1.1/BRAND-PRODUCTION-THEME.md` | brand.toml production theme `REFUSED_PRODUCTION` fence + next verifier (≠ hot-swap ALIVE) |
| `docs/rfc/v27.1.1/IGNITER-CLEANROOM-STANDING.md` | ggen_igniter clean-room contract (hop 4) |
| `docs/rfc/v27.1.1/ZCODE-POWL-OCEL-STANDING.md` | zcode POWL/OCEL bounded remediation (hop 5) |
| `docs/rfc/v27.1.1/ZCODE-POWL-PATH-STANDING.md` | hop-5 path honesty — tip-cargo refusal **ALIVE** (retip4) ≠ `--live` / Foundry; path crown **PARTIAL**; `--live` **BLOCKED_HITL** |
| `docs/rfc/v27.1.1/ZCODE-POWL-RETIP4-STANDING.md` | zcode POWL/OCEL tip-cargo **ALIVE** **6+4**/EXIT:**0** (refusal ≠ live / Foundry ALIVE) |
| `docs/rfc/v27.1.1/ZCODE-POWL-REFUSAL-RETIP6-STANDING.md` | historical tip-cargo refuse-path PARTIAL (EXIT 137 @ `2685119bd`; superseded by retip4 ALIVE for tip-cargo class) |
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

---

## 17. Retip matrix — live tip honesty (`9c56420dd`)

| Field | Value |
|-------|-------|
| Retip branch | `cursor/v2711-ecospec-matrix-retip-bc54` (`bc-5b3b2915`) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`9c56420dd10bf1cf8cd231c84ceba67830fefe11`** (`9c56420dd`; ⊇ Smalt RAID FinOps `2c69230f2` + Foundry HITL Retip3 `6f920670f`) |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty matrix; Gate-1 tip6 owns thrash) |
| Law | Standings below are **held from tip companion ledgers only** — no invented `ALIVE` |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

Companion matrices: [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md); [`GATE-0-2-3-NEXT-VERIFIERS.md`](./GATE-0-2-3-NEXT-VERIFIERS.md); [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md).

### 17.1 Gate 1 — in-repo / sibling

| Surface | Standing @ tip `9c56420dd` | Evidence ledger | Exact next verifier (non-`ALIVE` / replay) |
|---------|---------------------------|-----------------|-------------------------------------------|
| Truex admit `shui:` | **ALIVE** (sibling admit-court; ≠ Foundry) | [`TRUEX-ADMIT-EXACT-RETIP3-STANDING.md`](./TRUEX-ADMIT-EXACT-RETIP3-STANDING.md) — `truex@68d2d233` **6/6** EXIT **0** (merged `b4672131`) | Replay: `pnpm exec vitest run test/admit/admit-court.test.ts` → EXIT **0** |
| `shui:` public-prefix fence | **PARTIAL_ALIVE** (exact-tip5 EXIT:**137**; lineage **ALIVE** @ `008a81d21` held) | [`SHUI-PUBLIC-PREFIX-EXACT-TIP5-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP5-ALIVE-STANDING.md) | `CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip5 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` → **4**/EXIT:**0** |
| Taffy AOT preflight | **PARTIAL** (exact-tip reconfirm PENDING; lineage **ALIVE** **7** @ `19c08a3b0` held) | [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md) | `CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` → **7**/EXIT:**0** |
| brand.toml rhythm/completeness | **PARTIAL_ALIVE** (retip5 budget miss; lineage **ALIVE** **5+12** @ `77eb46fd0` held) | [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md) | `CARGO_TARGET_DIR=/tmp/ggen-brand-exact-tip cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked -- --test-threads=1` → EXIT:**0** |
| `ui_layout` digest consume | **PARTIAL_ALIVE** (retip6 EXIT:**124**; lineage **ALIVE** **11** @ `a508e6bae` held) | [`UILAYOUT-EXACT-RETIP6-STANDING.md`](./UILAYOUT-EXACT-RETIP6-STANDING.md) | exclusive `ui_layout_court` `--target-dir /tmp/ggen-uilayout-exact-retip6` → **11**/EXIT:**0** |
| `curve25519-dalek` pin | **ALIVE** (pin court **6**/EXIT:**0** @ `bebea5dd0`) | [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md) | Replay: `cargo test -p ggen-config --test curve25519_dalek_pin_court` → **6**/EXIT:**0** |
| VW size (&lt;250KB) | measure **ALIVE** (`wc -c` **28673**); tip-cargo court **PARTIAL** (EXIT:**137**); production runtime **UNSUPPORTED** | [`VERIFY-WITNESS-SIZE-RETIP3-STANDING.md`](./VERIFY-WITNESS-SIZE-RETIP3-STANDING.md) | Tip-cargo next: `cargo test -p ggen-graph --test verify_witness_size_court` → **8**/EXIT:**0**; prod: [`VERIFY-WITNESS-CI-WASM-NEXT-VERIFIER-STANDING.md`](./VERIFY-WITNESS-CI-WASM-NEXT-VERIFIER-STANDING.md) §3 |
| zcode POWL refusal / hop-5 path | **PARTIAL** (retip6 EXIT:**137**; lineage refusal **ALIVE** held @ `cd7c2433b`; hop-5 slice **PARTIAL**; ≠ Foundry; `--live` **BLOCKED_HITL**) | [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md); [`ZCODE-POWL-REFUSAL-RETIP6-STANDING.md`](./ZCODE-POWL-REFUSAL-RETIP6-STANDING.md) | Tip-cargo: `cargo test -p ggen-sjira --test zcode_powl_ocel_court` + `… remediation_bound --lib` → **6+4**/EXIT:**0**. Live: `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **0** |
| igniter clean-room | **ALIVE** (`git diff --exit-code` → **0**; ≠ path / Foundry `--live`) | [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) | Replay: `cd /home/ubuntu/ggen_igniter && git diff --exit-code` → **0** |

### 17.2 Ops

| Surface | Standing @ tip `9c56420dd` | Evidence ledger | Exact next verifier |
|---------|---------------------------|-----------------|---------------------|
| Smalt RAID / FinOps | **ALIVE** (lineage; 4-cloud **0,0,0,0,0** + FinOps **19** + residual **0,0,0,0** @ `91997da76f0f`) — ≠ Foundry | [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) §6c/§6d | Replay FinOps + §4.1 4-cloud + residual §4.3 |
| cos / revops | cos `/readyz` **BLOCKED** (**503** `DB_UNAVAILABLE`); `/healthz` **PARTIAL**; revops **UNKNOWN** (**NXDOMAIN**) | [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md); OOR retip18 @ `47aebeb75` held through tip | `curl … https://cos.fnox.io/readyz` → **200**; `getent hosts revops.fnox.io` ∧ `curl … https://revops.fnox.io/readyz` → DNS ∧ **200** |
| brand hot-swap / production theme | hot-swap fence lineage **ALIVE** (**6** @ `19c08a3b0`); **production theme UNSUPPORTED** (`REFUSED_PRODUCTION`) | [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md) | Fence replay: `… brand_toml_hotswap_court`; production promote: admit DTCG inventory (G2-13) + CASTLE bind (G2-12) + production apply court → EXIT:**0** (until then `promote_production_theme` → `REFUSED_PRODUCTION`) |

### 17.3 CCS

| Surface | Standing @ tip `9c56420dd` | Evidence ledger | Exact next verifier |
|---------|---------------------------|-----------------|---------------------|
| BRCE **V8** + R-01…R-07 bodies | **V8 BLOCKED**; bodies **BLOCKED** (contract fence **ALIVE** **7** @ `e7569893a` ≠ bodies / broker) | [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md); [`R01-R07-BODIES-NEXT-VERIFIER-STANDING.md`](./R01-R07-BODIES-NEXT-VERIFIER-STANDING.md) | V8: Founder HITL mount BRCE broker — without AS-01 → `REFUSED_*`; with seat → receipt+replay → EXIT:**0**. Bodies: admit sovereign risk-register → per-ID receipt/`REFUSED_*` |
| WebAuthn AS-01 (live) | live **UNSUPPORTED**; V7 fixture **ALIVE** @ `fb6e7fc6e` ≠ live; V1 seat **ALIVE** @ `e7569893a` ≠ broker | [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 | Founder HITL: admit RP-ID → enroll hardware → live assertion court → EXIT:**0**; then **V8** |
| CASTLE digest-bind wire | wire **ALIVE**/`WIRED` @ castle `e0d99c6` (**6/6**); spine crown **PARTIAL_ALIVE**; Foundry **PARTIAL** | [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §13–§14 | Replay: `cargo test --test publish_digest_bind_court` → **6/6**; spine residual: bind admitted ggen digest into mint + LinkedIn AND-gate after cos `/readyz` **200** |

### 17.4 Foundry / `--live` HITL

| Surface | Standing @ tip `9c56420dd` | Evidence ledger | Exact next verifier |
|---------|---------------------------|-----------------|---------------------|
| Foundry / zero-HITL crown | **PARTIAL** — **not** Foundry `ALIVE` | [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) §9 | Close HITL residuals (cos DB + WebAuthn + `--live`) before any crown promote |
| foundry `--live` | **BLOCKED_HITL** (`foundry_exit=1` / `Model access is not configured` @ zcode `f32df08`) | [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §10 | `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |

### 17.5 Compact surface→standing

```text
Truex=ALIVE  shui=PARTIAL_ALIVE  Taffy=PARTIAL  brand.toml=PARTIAL_ALIVE
ui_layout=PARTIAL_ALIVE  curve25519=ALIVE  VW_size=ALIVE(measure)/PARTIAL(court)/UNSUPPORTED(prod)
zcode_POWL_refusal=PARTIAL(EXIT137)/lineage_ALIVE_held  hop5_slice=PARTIAL
igniter_cleanroom=ALIVE
Smalt_RAID=ALIVE  cos=BLOCKED  revops=UNKNOWN  brand_hotswap_prod=UNSUPPORTED
BRCE_V8=BLOCKED  R01_R07_bodies=BLOCKED  WebAuthn_live=UNSUPPORTED  CASTLE_wire=ALIVE/WIRED
Foundry=PARTIAL  --live=BLOCKED_HITL  refusal≠Foundry_ALIVE
tip=83c954e5f(path_pin)/9c56420dd(matrix)  cargo=NOT_RUN  invented_ALIVE=none
```

---

## 18. Retip matrix — live tip honesty (`c4e757a14` / ⊇ `552c85f29`+)

| Field | Value |
|-------|-------|
| Retip branch | `cursor/v2711-ecospec-matrix-retip2-bc54` (`bc-5fc56758`) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`c4e757a14c91c6512438a8a94bbde304b0a6e921`** (`c4e757a14`; ⊇ tip6 lands uilayout `c4e757a14` + Taffy restore `2b876c6ff` + shui `702a929cf` + brand `83ecfb80b`; floor `552c85f29` zcode path pin + Truex absorb `83c954e5f`; `e24ce7a73`+) |
| Cargo / tip-cargo | **NOT RUN** (docs-only honesty matrix; Gate-1 tip6 owns thrash) |
| Law | Standings below are **held from tip companion ledgers only** — no invented `ALIVE` |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

Companion matrices: [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md); [`GATE-0-2-3-NEXT-VERIFIERS.md`](./GATE-0-2-3-NEXT-VERIFIERS.md); [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md); [`GATE023-OOR-STANDING-MATRIX.md`](./GATE023-OOR-STANDING-MATRIX.md). Prior matrix: §17 @ `9c56420dd` (superseded for tip citation; standings held where ledgers unchanged).

### 18.1 Gate 1 — in-repo / sibling

| Surface | Standing @ tip `c4e757a14` | Evidence ledger | Exact next verifier (non-`ALIVE` / replay) |
|---------|----------------------------|-----------------|-------------------------------------------|
| Truex admit `shui:` | **ALIVE** (tip-exact; sibling admit-court **6/6** EXIT **0**; ≠ Foundry) | [`TRUEX-SHUI-EXACT-RETIP-STANDING.md`](./TRUEX-SHUI-EXACT-RETIP-STANDING.md); [`TRUEX-ADMIT-EXACT-RETIP3-STANDING.md`](./TRUEX-ADMIT-EXACT-RETIP3-STANDING.md) — `truex@68d2d233` **6/6** EXIT **0** (merged `b4672131`; reconfirm held through absorb `83c954e5f`) | Replay: `pnpm exec vitest run test/admit/admit-court.test.ts` → EXIT **0** |
| `shui:` public-prefix fence | **PARTIAL** / **PARTIAL_ALIVE** (exact-tip6 EXIT:**137**; lineage **ALIVE** **4** @ `008a81d21` held; sources ≡ tip) | [`SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md); [`TRUEX-SHUI-EXACT-RETIP-STANDING.md`](./TRUEX-SHUI-EXACT-RETIP-STANDING.md) §2 | `CARGO_TARGET_DIR=/tmp/ggen-shui-exact-tip6 cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` → **4**/EXIT:**0** |
| Taffy AOT preflight | **PARTIAL** (exact-tip retip6/tip6 EXIT:**137**; lineage **ALIVE** **7** @ `19c08a3b0` held; PRESENT blob `d8923b07e`) | [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md) | `CARGO_TARGET_DIR=/tmp/gate1-taffy cargo test -p ggen-graph --test taffy_aot_preflight_court --locked --config build.jobs=1 -- --test-threads=1` → **7**/EXIT:**0** |
| brand.toml rhythm/completeness | **PARTIAL** / **PARTIAL_ALIVE** (exact-tip6 EXIT:**137**; lineage **ALIVE** **5+12** @ `77eb46fd0` held) | [`BRAND-TOML-EXACT-TIP6-STANDING.md`](./BRAND-TOML-EXACT-TIP6-STANDING.md) | `CARGO_TARGET_DIR=/tmp/ggen-brand-exact-tip6 cargo test -p ggen-graph --test brand_toml_completeness_court --test brand_toml_rhythm_court --locked --config build.jobs=1 -- --test-threads=1` → EXIT:**0** |
| brand.toml hot-swap fence | **PARTIAL** / **PARTIAL_ALIVE** (exact-retip2 EXIT:**137**; lineage **ALIVE** **6** @ `19c08a3b0` held); production theme **UNSUPPORTED** (`REFUSED_PRODUCTION`) | [`BRAND-TOML-HOTSWAP-EXACT-RETIP2-STANDING.md`](./BRAND-TOML-HOTSWAP-EXACT-RETIP2-STANDING.md); [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md) | Fence: `CARGO_TARGET_DIR=/tmp/ggen-brand-hotswap-exact-retip2 cargo test -p ggen-graph --test brand_toml_hotswap_court --locked --config build.jobs=1 -- --test-threads=1` → **6**/EXIT:**0** |
| `ui_layout` digest consume | **PARTIAL** / **PARTIAL_ALIVE** (exact-tip6 EXIT:**137**; lineage **ALIVE** **11** @ `a508e6bae` held) | [`UILAYOUT-EXACT-TIP6-STANDING.md`](./UILAYOUT-EXACT-TIP6-STANDING.md) | exclusive `ui_layout_court` `--target-dir /tmp/ggen-uilayout-exact-tip6` → **11**/EXIT:**0** |
| `curve25519-dalek` pin | **ALIVE** (pin court **6**/EXIT:**0** @ `bebea5dd0`) | [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md) | Replay: `cargo test -p ggen-config --test curve25519_dalek_pin_court` → **6**/EXIT:**0** |
| VW size (&lt;250KB) | measure **ALIVE** (`wc -c` **28673**); tip-cargo court **PARTIAL** (EXIT:**137**); production runtime **UNSUPPORTED** | [`VERIFY-WITNESS-SIZE-RETIP3-STANDING.md`](./VERIFY-WITNESS-SIZE-RETIP3-STANDING.md) | Tip-cargo next: `cargo test -p ggen-graph --test verify_witness_size_court` → **8**/EXIT:**0**; prod: [`VERIFY-WITNESS-CI-WASM-NEXT-VERIFIER-STANDING.md`](./VERIFY-WITNESS-CI-WASM-NEXT-VERIFIER-STANDING.md) §3 |
| zcode POWL / hop-5 path | **PARTIAL** (tip-cargo EXIT:**137**; lineage refusal **ALIVE** held @ `cd7c2433b`; hop-5 slice **PARTIAL**; ≠ Foundry; `--live` **BLOCKED_HITL**) | [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md) @ tip pin `83c954e5f`/`552c85f29`; [`ZCODE-POWL-REFUSAL-RETIP6-STANDING.md`](./ZCODE-POWL-REFUSAL-RETIP6-STANDING.md) | Tip-cargo: `cargo test -p ggen-sjira --test zcode_powl_ocel_court` + `… remediation_bound --lib` → **6+4**/EXIT:**0**. Live: `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |
| igniter clean-room | **ALIVE** (`git diff --exit-code` → **0**; ≠ path / Foundry `--live`) | [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) | Replay: `cd /home/ubuntu/ggen_igniter && git diff --exit-code` → **0** |

### 18.2 Ops

| Surface | Standing @ tip `c4e757a14` | Evidence ledger | Exact next verifier |
|---------|----------------------------|-----------------|---------------------|
| Smalt RAID / FinOps | **ALIVE** (lineage; 4-cloud **0,0,0,0,0** + FinOps **19** + residual **0,0,0,0** @ `91997da76f0f`) — ≠ Foundry | [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) §6c/§6d | Replay FinOps + §4.1 4-cloud + residual §4.3 |
| cos / revops | cos `/readyz` **BLOCKED** (**503** `DB_UNAVAILABLE`); `/healthz` **PARTIAL**; revops **UNKNOWN**/**BLOCKED** (**NXDOMAIN**) | [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md); OOR retip18 @ `47aebeb75` held through tip | `curl … https://cos.fnox.io/readyz` → **200**; `getent hosts revops.fnox.io` ∧ `curl … https://revops.fnox.io/readyz` → DNS ∧ **200** |

### 18.3 CCS

| Surface | Standing @ tip `c4e757a14` / binder base `7d1c24584` (⊇ `e6d00965a`) | Evidence ledger | Exact next verifier |
|---------|----------------------------|-----------------|---------------------|
| Clause-2 CCS / BRCE + WebAuthn crown | **PARTIAL** / **PARTIAL_ALIVE** (tip-cargo fences **ALIVE**; live broker **BLOCKED** — **not** V8 / WebAuthn / Foundry `ALIVE`) | [`CCS-BRCE-WEBAUTHN-STANDING.md`](./CCS-BRCE-WEBAUTHN-STANDING.md); [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) | Close WebAuthn §3 → **V8** → **V9**/G2-5 + bodies before crown promote |
| BRCE **V8** + R-01…R-07 bodies | **V8 BLOCKED**; bodies **BLOCKED** (contract fence **ALIVE** **7** @ `e7569893a` ≠ bodies / broker) | [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md); [`R01-R07-BODIES-NEXT-VERIFIER-STANDING.md`](./R01-R07-BODIES-NEXT-VERIFIER-STANDING.md) | V8: Founder HITL mount BRCE broker — without AS-01 → `REFUSED_*`; with seat → receipt+replay → EXIT:**0**. Bodies: admit sovereign risk-register → per-ID receipt/`REFUSED_*` |
| WebAuthn AS-01 (live) | live **UNSUPPORTED**; V7 fixture **ALIVE** @ `fb6e7fc6e` ≠ live; V1 seat **ALIVE** @ `e7569893a` ≠ broker | [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 | Founder HITL: admit RP-ID → enroll hardware → live assertion court → EXIT:**0**; then **V8** |
| CASTLE digest-bind wire | wire **ALIVE**/`WIRED` @ castle `e0d99c6` (**6/6**); spine crown **PARTIAL_ALIVE**; Foundry **PARTIAL** | [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §13–§14 | Replay: `cargo test --test publish_digest_bind_court` → **6/6**; spine residual: bind admitted ggen digest into mint + LinkedIn AND-gate after cos `/readyz` **200** |

### 18.4 Foundry / `--live` HITL

| Surface | Standing @ tip `c4e757a14` | Evidence ledger | Exact next verifier |
|---------|----------------------------|-----------------|---------------------|
| Foundry / zero-HITL crown | **PARTIAL** — **not** Foundry `ALIVE` | [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) §1a/§9 | Close HITL residuals (cos DB + WebAuthn + `--live`) before any crown promote |
| foundry `--live` | **BLOCKED_HITL** (`foundry_exit=1` / `Model access is not configured` @ zcode `f32df08`) | [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §10; [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md) §3.2 | `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |

### 18.5 Compact surface→standing

```text
Truex=ALIVE(EXIT0 tip-exact)  shui=PARTIAL  Taffy=PARTIAL  brand=PARTIAL
ui_layout=PARTIAL  curve25519=ALIVE  VW_size=ALIVE(measure)/PARTIAL(court)/UNSUPPORTED(prod)
zcode_path=PARTIAL(EXIT137)/lineage_refusal_ALIVE_held  hop5_slice=PARTIAL
igniter_cleanroom=ALIVE
Smalt_RAID=ALIVE  cos=BLOCKED  revops=BLOCKED(NXDOMAIN)  brand_hotswap_prod=UNSUPPORTED
CCS=PARTIAL  BRCE_V8=BLOCKED  R01_R07_bodies=BLOCKED  WebAuthn_live=UNSUPPORTED  CASTLE_wire=ALIVE/WIRED
Foundry=PARTIAL  --live=BLOCKED_HITL  refusal≠Foundry_ALIVE
tip=c4e757a14(⊇552c85f29+)  cargo=NOT_RUN  invented_ALIVE=none
```

---

## 19. Retip matrix — live tip honesty (`ab0a52362` / ⊇ `59b9fae1c`+ / tip-cargo `2685119bd`)

| Field | Value |
|-------|-------|
| Retip branch | `cursor/v2711-ecospec-alive-index-bc54` |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`ab0a523620f19fc2d697b6ccc782e1a881baf433`** (`ab0a52362`; past uilayout land `59b9fae1c`; ⊇ brand/Taffy/uilayout exact tip-cargo ALIVE absorb @ `2685119bd`; Gate1 remaining-falsifiers retip) |
| Cargo / tip-cargo | **NOT RUN** (docs-only binder index; sealed tip-cargo EXIT:**0** held @ `2685119bd` / hot-swap @ `c4e757a14`; shui tip7 EXIT0 **in flight**) |
| Law | tip-cargo `ALIVE` ≠ live-tip reconfirm ≠ path-crown `ALIVE` ≠ Foundry `ALIVE`. Standings held from companion ledgers only — **no invented Foundry / tip-subject / production-theme `ALIVE`** |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

Companion matrices: [`GATE1-REMAINING-FALSIFIERS.md`](./GATE1-REMAINING-FALSIFIERS.md); [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md); [`GATE-0-2-3-NEXT-VERIFIERS.md`](./GATE-0-2-3-NEXT-VERIFIERS.md); [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md); [`GATE023-OOR-STANDING-MATRIX.md`](./GATE023-OOR-STANDING-MATRIX.md). Prior matrices: §17 @ `9c56420dd`, §18 @ `c4e757a14` (superseded for tip citation; historical rows retained).

### 19.1 Gate 1 — in-repo / sibling (post ALIVE trio)

| Surface | Standing @ tip `ab0a52362` (cargo seal `2685119bd` where noted) | Evidence ledger | Exact next verifier (non-`ALIVE` / replay) |
|---------|----------------------------------------------------------------|-----------------|-------------------------------------------|
| Truex admit `shui:` | **ALIVE** (sibling admit-court **6/6** EXIT **0** @ `truex@68d2d233`; ≠ Foundry / path-crown) | [`TRUEX-SHUI-EXACT-RETIP-STANDING.md`](./TRUEX-SHUI-EXACT-RETIP-STANDING.md); [`TRUEX-ADMIT-EXACT-RETIP3-STANDING.md`](./TRUEX-ADMIT-EXACT-RETIP3-STANDING.md) | Replay: `cd /tmp/truex && pnpm exec vitest run test/admit/admit-court.test.ts` → **6**/EXIT:**0** |
| brand.toml hot-swap tip-cargo | **ALIVE (exact tip-cargo)** **6**/EXIT:**0** @ `c4e757a14` (fence ceiling **PARTIAL_ALIVE**); production theme **UNSUPPORTED** | [`BRAND-HOTSWAP-TIP-CARGO-STANDING.md`](./BRAND-HOTSWAP-TIP-CARGO-STANDING.md); [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md) | Sealed — do not re-run under tip7 thrash unless sources drift. Prod stays `REFUSED_PRODUCTION` |
| brand.toml completeness + rhythm | **ALIVE (exact tip-cargo)** **5+12**/EXIT:**0** @ `2685119bd` — **≠ live-tip (`ab0a52362`/`59b9fae1c`) reconfirm** | [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md) | Sealed at `2685119bd` — replay only if sources drift |
| Taffy AOT preflight | **ALIVE (exact tip-cargo)** **7**/EXIT:**0** @ `2685119bd` — **≠ live-tip reconfirm** ≠ path-crown / Foundry | [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md) | Sealed at `2685119bd` — replay only if sources drift |
| `ui_layout` digest consume | **ALIVE (exact tip-cargo)** **11**/EXIT:**0** @ `2685119bd` — **≠ live-tip reconfirm**; sibling `.ts` **UNSUPPORTED** | [`UILAYOUT-EXACT-RETIP5-STANDING.md`](./UILAYOUT-EXACT-RETIP5-STANDING.md) | Sealed at `2685119bd` — replay only if sources drift |
| `shui:` public-prefix fence | **PARTIAL_ALIVE** (tip7 EXIT0 **in flight**; tip6 EXIT:**137**; lineage **ALIVE** **4** @ `008a81d21` held — not tip-subject substitute) | [`SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md`](./SHUI-PUBLIC-PREFIX-EXACT-TIP6-STANDING.md); [`GATE1-REMAINING-FALSIFIERS.md`](./GATE1-REMAINING-FALSIFIERS.md) | **Exact next:** `CXX=g++ SCCACHE_DISABLE=1 RUSTC_WRAPPER= CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/gate1-tip7-shui cargo test -p ggen-graph --test shui_public_prefix_court --locked --config build.jobs=1 -- --test-threads=1` → **4**/EXIT:**0** (do **not** invent EXIT:**0**) |
| `curve25519-dalek` pin | **ALIVE** (pin court **6**/EXIT:**0** @ `bebea5dd0`) | [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md) | Replay: `cargo test -p ggen-config --test curve25519_dalek_pin_court` → **6**/EXIT:**0** |
| VW size (&lt;250KB) | measure **ALIVE** (`wc -c` **28673**); tip-cargo court **PARTIAL**; production runtime **UNSUPPORTED** | [`VERIFY-WITNESS-SIZE-RETIP3-STANDING.md`](./VERIFY-WITNESS-SIZE-RETIP3-STANDING.md) | Tip-cargo: `cargo test -p ggen-graph --test verify_witness_size_court` → **8**/EXIT:**0** |
| zcode POWL / hop-5 path | **PARTIAL** (tip-cargo EXIT:**137**; lineage refusal **ALIVE** held; ≠ Foundry; `--live` **BLOCKED_HITL**) | [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md); [`ZCODE-POWL-REFUSAL-RETIP6-STANDING.md`](./ZCODE-POWL-REFUSAL-RETIP6-STANDING.md) | Tip-cargo replay if disputed; **exact next live:** founder `bun bin/zcode.js login` + `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** (**not** bare `zcode /login`) |
| igniter clean-room | **ALIVE** (`git diff --exit-code` → **0**; ≠ path / Foundry `--live`) | [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) | Replay: `cd /home/ubuntu/ggen_igniter && git diff --exit-code` → **0** |

### 19.2 Ops

| Surface | Standing @ tip `ab0a52362` | Evidence ledger | Exact next verifier |
|---------|----------------------------|-----------------|---------------------|
| Smalt RAID / FinOps | **ALIVE** (lineage; 4-cloud + FinOps + residual @ `91997da76f0f`) — ≠ Foundry | [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) §6c/§6d | Replay FinOps + §4.1 4-cloud + residual §4.3 |
| cos / revops | cos `/readyz` **BLOCKED** (**503** `DB_UNAVAILABLE`); `/healthz` **PARTIAL**; revops **UNKNOWN**/**BLOCKED** (**NXDOMAIN**) | [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) | `curl … https://cos.fnox.io/readyz` → **200**; `getent hosts revops.fnox.io` ∧ `curl … https://revops.fnox.io/readyz` → DNS ∧ **200** |
| brand production theme | **UNSUPPORTED** (`REFUSED_PRODUCTION`) — hot-swap / rhythm tip-cargo `ALIVE` ≠ production `ALIVE` | [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md) | Gate2+ / OOR path until production falsifiers EXIT:**0** |

### 19.3 CCS / BRCE / WebAuthn

| Surface | Standing @ tip `ab0a52362` | Evidence ledger | Exact next verifier |
|---------|----------------------------|-----------------|---------------------|
| Clause-2 CCS / BRCE + WebAuthn crown | **PARTIAL** / **PARTIAL_ALIVE** (tip-cargo fences **ALIVE**; live broker **BLOCKED** — **not** V8 / WebAuthn / Foundry `ALIVE`) | [`CCS-BRCE-WEBAUTHN-STANDING.md`](./CCS-BRCE-WEBAUTHN-STANDING.md); [`CCS-BRCE-STANDING.md`](./CCS-BRCE-STANDING.md) | Close WebAuthn §3 → **V8** → **V9**/G2-5 + bodies before crown promote |
| BRCE **V8** + R-01…R-07 bodies | **V8 BLOCKED**; bodies **BLOCKED** (contract fence **ALIVE** ≠ bodies / broker) | [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md); [`R01-R07-BODIES-NEXT-VERIFIER-STANDING.md`](./R01-R07-BODIES-NEXT-VERIFIER-STANDING.md) | Founder HITL mount BRCE broker — without AS-01 → `REFUSED_*`; with seat → receipt+replay → EXIT:**0** |
| WebAuthn AS-01 (live) | live **UNSUPPORTED**; V7 fixture **ALIVE** ≠ live; V1 seat **ALIVE** ≠ broker | [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 | Founder HITL: admit RP-ID → enroll hardware → live assertion court → EXIT:**0**; then **V8** |
| CASTLE digest-bind wire | wire **ALIVE**/`WIRED` @ castle `e0d99c6` (**6/6**); spine crown **PARTIAL_ALIVE**; Foundry **PARTIAL** | [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) §13–§14 | Replay: `cargo test --test publish_digest_bind_court` → **6/6** |

### 19.4 Foundry / `--live` HITL

| Surface | Standing @ tip `ab0a52362` | Evidence ledger | Exact next verifier |
|---------|----------------------------|-----------------|---------------------|
| Foundry / zero-HITL crown | **PARTIAL** — **BLOCKED_HITL** residuals open — **not** Foundry `ALIVE` | [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md) §1a/§9 | Close HITL residuals (cos DB + WebAuthn + `--live`) before any crown promote |
| foundry `--live` | **BLOCKED_HITL** (`foundry_exit=1` / `Model access is not configured` @ zcode `f32df08`) | [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §10; [`ZCODE-POWL-PATH-STANDING.md`](./ZCODE-POWL-PATH-STANDING.md) §3.2 | **Exact next:** `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |

### 19.5 Compact surface→standing

```text
Truex=ALIVE@68d2d233
hotswap=ALIVE(exact tip-cargo)@c4e757a14 prod=UNSUPPORTED
shui=PARTIAL_ALIVE tip7_EXIT0_in_flight
Taffy=ALIVE(exact tip-cargo)@2685119bd (7) ≠live-tip-reconfirm
uilayout=ALIVE(exact tip-cargo)@2685119bd (11) ≠live-tip-reconfirm
brand_completeness+rhythm=ALIVE(exact tip-cargo)@2685119bd (5+12) ≠live-tip-reconfirm
curve25519=ALIVE  VW_size=ALIVE(measure)/PARTIAL(court)/UNSUPPORTED(prod)
zcode_path=PARTIAL  hop5_slice=PARTIAL  igniter_cleanroom=ALIVE
Smalt_RAID=ALIVE  cos=BLOCKED  revops=UNKNOWN/BLOCKED
CCS=PARTIAL  BRCE_V8=BLOCKED  WebAuthn_live=UNSUPPORTED  CASTLE_wire=ALIVE/WIRED
Foundry=PARTIAL/BLOCKED_HITL  --live=BLOCKED_HITL  refusal≠Foundry_ALIVE
tip-cargo≠live-tip-reconfirm≠path-crown≠Foundry
exact_next=shui_EXIT0 ; founder zcode_/login + foundry_--live
tip=ab0a52362(⊇59b9fae1c+/2685119bd)  cargo=NOT_RUN  invented_ALIVE=none
```

---

## 20. Retip matrix — Gate1 closed tip-cargo ALIVE (`15eefb628`)

| Field | Value |
|-------|-------|
| Retip branch | `cursor/v2711-ecospec-gate1-closed-bc54` (`bc-73d56bd6`) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`15eefb628368afcbb230d082d6ca00c286ca9d42`** (`15eefb628`; ⊇ Gate1 close pin; ⊇ tip-wins Gate1 exact-tip EXIT0 absorb `712cdfafb`; ⊇ shui fence ALIVE absorb `edf2ec22a` ← `a4b3d3fb4`; ⊇ brand tip6 `31b495a6a`/`cfd93c1cb`; ⊇ Taffy/uilayout `2685119bd`) |
| Cargo / tip-cargo | **NOT RUN** (docs-only binder close; Gate1 in-repo tip-cargo falsifiers already EXIT:**0** sealed) |
| Law | tip-cargo `ALIVE` ≠ live-tip reconfirm ≠ path-crown `ALIVE` ≠ Foundry `ALIVE`. **Never invent Foundry ALIVE.** |

Standing vocabulary: `UNKNOWN` | `PARTIAL` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `UNSUPPORTED` | `REFUSED_*`.

Companion matrices: [`GATE1-REMAINING-FALSIFIERS.md`](./GATE1-REMAINING-FALSIFIERS.md); [`GATE1-EXACT-TIP-TIPWINS-ABSORB-STANDING.md`](./GATE1-EXACT-TIP-TIPWINS-ABSORB-STANDING.md); [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md); [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md); [`GATE-1-GAP-MATRIX.md`](./GATE-1-GAP-MATRIX.md); [`GATE023-OOR-STANDING-MATRIX.md`](./GATE023-OOR-STANDING-MATRIX.md). Prior matrices: §17–§19 retained (historical; tip citation superseded by this §20).

### 20.1 Gate 1 — in-repo falsifiers (tip-cargo ALIVE closed)

| Surface | Standing @ tip `15eefb628` (cargo seals noted) | Evidence ledger | Exact next verifier |
|---------|------------------------------------------------|-----------------|---------------------|
| Truex admit `shui:` | **ALIVE** (sibling admit-court **6/6** EXIT **0** @ `truex@68d2d233`; ≠ Foundry / path-crown) | [`TRUEX-SHUI-EXACT-RETIP-STANDING.md`](./TRUEX-SHUI-EXACT-RETIP-STANDING.md); [`TRUEX-ADMIT-EXACT-RETIP2-STANDING.md`](./TRUEX-ADMIT-EXACT-RETIP2-STANDING.md) | Replay only if disputed: `pnpm exec vitest run test/admit/admit-court.test.ts` → **6**/EXIT:**0** |
| brand.toml completeness + rhythm | **ALIVE (exact tip-cargo)** **5+12**/EXIT:**0** @ `cfd93c1cb` — **≠ live-tip reconfirm** | [`BRAND-TOML-EXACT-TIP6-STANDING.md`](./BRAND-TOML-EXACT-TIP6-STANDING.md) | Sealed — replay only if sources drift |
| brand.toml hot-swap tip-cargo | **ALIVE (exact tip-cargo)** **6**/EXIT:**0** @ `c4e757a14` (fence ceiling **PARTIAL_ALIVE**); production theme **UNSUPPORTED** | [`BRAND-HOTSWAP-TIP-CARGO-STANDING.md`](./BRAND-HOTSWAP-TIP-CARGO-STANDING.md); [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md) | Sealed — prod stays `REFUSED_PRODUCTION` |
| Taffy AOT preflight | **ALIVE (exact tip-cargo)** **7**/EXIT:**0** @ `2685119bd` — **≠ live-tip reconfirm** ≠ path-crown / Foundry | [`TAFFY-AOT-PREFLIGHT-STANDING.md`](./TAFFY-AOT-PREFLIGHT-STANDING.md) | Sealed — replay only if sources drift |
| `ui_layout` digest consume | **ALIVE (exact tip-cargo)** **11**/EXIT:**0** @ `2685119bd` — **≠ live-tip reconfirm**; sibling `.ts` **UNSUPPORTED** | [`UILAYOUT-EXACT-RETIP5-STANDING.md`](./UILAYOUT-EXACT-RETIP5-STANDING.md) | Sealed — replay only if sources drift |
| `shui:` public-prefix fence | **ALIVE (exact tip-cargo)** **4**/EXIT:**0** @ `a4b3d3fb4` (absorb `edf2ec22a`; tip3 absorb held) — **≠ live-tip reconfirm** | [`SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md`](./SHUI-PUBLIC-PREFIX-ALIVE-STANDING.md); [`GATE1-REMAINING-FALSIFIERS.md`](./GATE1-REMAINING-FALSIFIERS.md) | Sealed — tip6/tip7 PARTIAL superseded for tip-cargo class |
| `curve25519-dalek` pin | **ALIVE** (pin court **6**/EXIT:**0** @ `bebea5dd0`) | [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md) | Replay: `cargo test -p ggen-config --test curve25519_dalek_pin_court` → **6**/EXIT:**0** |
| VW size (&lt;250KB) | size court **ALIVE** (**8**/EXIT:**0** retip3); measure **ALIVE** (`wc -c` **28673**); production runtime **UNSUPPORTED** | [`VERIFY-WITNESS-CI-WASM-RETIP3-STANDING.md`](./VERIFY-WITNESS-CI-WASM-RETIP3-STANDING.md) | Production: CI consume EXIT:**0** path — stay **UNSUPPORTED** until then |
| zcode POWL / hop-5 path | tip-cargo refusal **ALIVE** (**6+4**/EXIT:**0** retip4); path crown **PARTIAL**; `--live` **BLOCKED_HITL** — refusal ≠ Foundry | [`ZCODE-POWL-RETIP4-STANDING.md`](./ZCODE-POWL-RETIP4-STANDING.md); [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md) | **Exact next live:** founder `bun bin/zcode.js login` + `bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |
| igniter clean-room | **ALIVE** (`git diff --exit-code` → **0**; ≠ path / Foundry `--live`) | [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md) | Replay: `cd /home/ubuntu/ggen_igniter && git diff --exit-code` → **0** |
| Photolithography path crown | **PARTIAL** (non-ALIVE) — Gate1 tip-cargo green ≠ path crown | [`PHOTOLITHOGRAPHY-PATH-STANDING.md`](./PHOTOLITHOGRAPHY-PATH-STANDING.md) | Close hop residuals + Foundry `--live` before any path-crown promote |

### 20.2 Ops

| Surface | Standing @ tip `15eefb628` | Evidence ledger | Exact next verifier |
|---------|----------------------------|-----------------|---------------------|
| Smalt RAID / FinOps | **ALIVE** (lineage) — ≠ Foundry | [`SMALT-RAID-STANDING.md`](./SMALT-RAID-STANDING.md) | Replay FinOps + 4-cloud + residual |
| cos | `/readyz` **BLOCKED** (**503** `DB_UNAVAILABLE`); `/healthz` **PARTIAL** | [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) | `curl … https://cos.fnox.io/readyz` → **200** |
| revops | **UNKNOWN** (**NXDOMAIN**) | [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) | `getent hosts revops.fnox.io` ∧ `curl … https://revops.fnox.io/readyz` → DNS ∧ **200** |
| brand production theme | **UNSUPPORTED** (`REFUSED_PRODUCTION`) | [`BRAND-PRODUCTION-THEME.md`](./BRAND-PRODUCTION-THEME.md) | Gate2+ / OOR until production falsifiers EXIT:**0** |

### 20.3 CCS / BRCE / WebAuthn

| Surface | Standing @ tip `15eefb628` | Evidence ledger | Exact next verifier |
|---------|----------------------------|-----------------|---------------------|
| Clause-2 CCS / BRCE + WebAuthn crown | **PARTIAL** / **PARTIAL_ALIVE** — **not** V8 / WebAuthn / Foundry `ALIVE` | [`CCS-BRCE-WEBAUTHN-STANDING.md`](./CCS-BRCE-WEBAUTHN-STANDING.md) | Close WebAuthn AS-01 → **V8** → bodies before crown promote |
| BRCE **V8** + R-01…R-07 bodies | **BRCE V8 BLOCKED**; bodies **BLOCKED** | [`BRCE-V8-NEXT-VERIFIER-STANDING.md`](./BRCE-V8-NEXT-VERIFIER-STANDING.md); [`R01-R07-BODIES-NEXT-VERIFIER-STANDING.md`](./R01-R07-BODIES-NEXT-VERIFIER-STANDING.md) | Founder HITL mount BRCE broker — without AS-01 → `REFUSED_*`; with seat → receipt+replay → EXIT:**0**; admit risk-register → per-ID receipt/`REFUSED_*` |
| WebAuthn AS-01 (live) | live **UNSUPPORTED**; V7 fixture / V1 seat ≠ live broker | [`WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md`](./WEBAUTHN-LIVE-NEXT-VERIFIER-STANDING.md) §3 | Founder HITL: admit RP-ID → enroll hardware → live assertion court → EXIT:**0**; then **V8** |
| CASTLE digest-bind wire | wire **ALIVE**/`WIRED`; spine crown **PARTIAL_ALIVE**; Foundry **PARTIAL** | [`CASTLE-SPINE-STANDING.md`](./CASTLE-SPINE-STANDING.md) | Replay digest-bind court; residual after cos `/readyz` **200** |

### 20.4 Foundry / `--live` HITL

| Surface | Standing @ tip `15eefb628` | Evidence ledger | Exact next verifier |
|---------|----------------------------|-----------------|---------------------|
| Foundry finish / zero-HITL crown | **PARTIAL** + **BLOCKED_HITL** residuals — **not** Foundry `ALIVE` | [`FOUNDRY-FINISH-STANDING.md`](./FOUNDRY-FINISH-STANDING.md); [`FOUNDRY-FINISH-SPINE-STANDING.md`](./FOUNDRY-FINISH-SPINE-STANDING.md) | Close HITL residuals (cos DB + WebAuthn + `--live`) before any crown promote |
| foundry `--live` | **BLOCKED_HITL** (`foundry_exit=1` / `Model access is not configured`) | [`FOUNDRY-HITL-ACTION-CARD.md`](./FOUNDRY-HITL-ACTION-CARD.md); [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) §10 | **Exact next:** `cd /agent/repos/zcode-cli && export PATH="/home/ubuntu/.bun/bin:$PATH" && bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live` → **foundry_exit=0** |

### 20.5 Exact next (ordered)

```text
1. founder: bun bin/zcode.js login && bun scripts/powl-foundry-loop.ts --live → foundry_exit=0
2. cos /readyz → 200 ; revops DNS ∧ /readyz → 200
3. BRCE V8 bodies (broker + R-01…R-07 typed receipts/refusals)
4. WebAuthn AS-01 live assertion court → EXIT:0 (then V8)
```

### 20.6 Compact surface→standing

```text
Gate1_inrepo_tip_cargo=ALIVE_CLOSED
Truex=ALIVE@68d2d233
brand=ALIVE(exact tip-cargo)@cfd93c1cb (5+12) hotswap=ALIVE@c4e757a14 prod=UNSUPPORTED
Taffy=ALIVE(exact tip-cargo)@2685119bd (7)
uilayout=ALIVE(exact tip-cargo)@2685119bd (11)
shui_fence=ALIVE(exact tip-cargo)@a4b3d3fb4 (4)
curve25519=ALIVE  VW_size=ALIVE(size court)/UNSUPPORTED(prod)
zcode_refusal=ALIVE(retip4)  photolith_path=PARTIAL  igniter=ALIVE
Smalt_RAID=ALIVE  cos=BLOCKED  revops=UNKNOWN
CCS=PARTIAL  BRCE_V8=BLOCKED  WebAuthn_live=UNSUPPORTED  CASTLE_wire=ALIVE/WIRED
Foundry_finish=PARTIAL  --live=BLOCKED_HITL  refusal≠Foundry_ALIVE
tip-cargo≠live-tip-reconfirm≠path-crown≠Foundry
exact_next=founder foundry_--live foundry_exit=0 ; cos/revops readyz 200 ; BRCE_V8 bodies ; WebAuthn_AS-01
tip=15eefb628  cargo=NOT_RUN  invented_ALIVE=none  ManagePullRequest=not_used
```

---

## 21. Retip21 — Construct preempt + OOR honesty (`6ae116c31`)

| Field | Value |
|-------|-------|
| Retip branch | `cursor/v2711-oor-construct-retip-bc54` |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ **`6ae116c31636cddf8d7674c6012fe34bdcabfd07`** |
| Companion | [`OOR-RETIP21-CONSTRUCT-PREEMPT-STANDING.md`](./OOR-RETIP21-CONSTRUCT-PREEMPT-STANDING.md); [`GATE023-OOR-STANDING-MATRIX.md`](./GATE023-OOR-STANDING-MATRIX.md) retip21; [`V2711-COMPLETION-AUDIT.md`](./V2711-COMPLETION-AUDIT.md) |
| Cargo | **NOT RUN** this retip (docs-only). Local Construct **55/55** @ `e568bef1d` ≠ CI Construct ALIVE. Local wasm-pack ALIVE @ `6ae116c31` ≠ CI Manufacture ALIVE. |
| Law | Gate1 tip-cargo **CLOSED/ALIVE** ≠ path-crown ALIVE ≠ Foundry ALIVE. **Never invent Foundry ALIVE.** |

### 21.1 Compact

```text
Gate1_tip_cargo=CLOSED/ALIVE
Construct_local=55/55@e568bef1d  Construct_CI=UNKNOWN(settling@6ae116c31)
wasm_opt_template=6ae116c31  Manufacture_CI=UNKNOWN
photolith_path=PARTIAL  Foundry=PARTIAL  --live=BLOCKED_HITL
cos=BLOCKED(503)  revops=UNKNOWN(NXDOMAIN)
BRCE_V8=BLOCKED  WebAuthn_live=UNSUPPORTED  CASTLE_wire=ALIVE/WIRED  Smalt=ALIVE
goal=NOT_COMPLETE  tip=6ae116c31  invented_ALIVE=none
exact_next=founder foundry_--live foundry_exit=0 ; cos/revops readyz 200 ; WebAuthn→V8 ; R01-R07 bodies
```
