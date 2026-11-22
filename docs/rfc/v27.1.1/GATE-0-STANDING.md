# Gate 0 standing matrix — billing / cos / servengine / SOW

| Field | Value |
|-------|-------|
| Document ID | `GATE-0-STANDING-27.1.1` |
| Companion | `ECOSYSTEM-SPEC-27.1.1.md` §3 Gate 0; `GATE-1-GAP-MATRIX.md` (Gate 0 out-of-scope rows); `REPO-INVENTORY-27.1.1.md` |
| Window | **2026-11-21 – 2026-12-05** (Gate 0) |
| Target GA | **2027-01-01** |
| Repo scope | `seanchatmangpt/ggen` — Gate 0 work is **mostly OUT of ggen DO** |
| Materialized | 2026-11-22 |
| Base observed | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `97c61345233907a2d096ac188c28332ac4666c9f` (retip8 OOR honesty; prior materialization tip `22d108f54`); inventory paths also present on `origin/main` |

Standing vocabulary (AGENTS.md / ECOSYSTEM-SPEC):
`UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Law for this matrix:** source presence, archived prototypes, marketplace prose, and
fleet-census name rows are **never** promoted to `ALIVE`. Out-of-repo subjects that
this checkout cannot execute are `BLOCKED` or `UNKNOWN` with an exact next verifier
through GA.

---

## 1. Crown matrix (Gate 0)

| Subject | Owner | ggen DO | In-repo evidence (stubs only) | Standing | Next exact verifier (≤ 2027-01-01 GA) |
|---------|-------|---------|-------------------------------|----------|----------------------------------------|
| **GCP billing** | infra / finance | **OUT OF SCOPE** | Contract prose + archived governor (below) — **not** a live billing plane | **BLOCKED** | External: GCP Billing / Marketplace console receipt for the admitted billing account covering Gate 0 window; weekly reconciliation exit 0 exported to customer GCS audit path named in contract. ggen must not claim this row `ALIVE`. |
| **cos ledger fixes** | cos (Plane 5 / FinFab) | **OUT OF SCOPE** | Fleet name `seanchatmangpt/cosg` only; no cos ledger runtime in ggen | **BLOCKED** | In **cos** (or FinFab) repo: ledger replay/verify court that closes the admitted Gate 0 fix set; typed receipt or typed refusal. Sibling not mounted → cannot run here. |
| **servengine M4** | servengine | **OUT OF SCOPE** | Archive path hardcodes foreign `servengine-frontend` dir (below) | **BLOCKED** | In **servengine** repo: M4 court / milestone gate exit 0 (repo’s exact runner). Path `/Users/sac/srve/servengine-frontend` is not this workspace. |
| **year-end SOW** | ops / commercial | **OUT OF SCOPE** | No commercial SOW register in ggen | **UNKNOWN** | Commercial SOW register / signed year-end SOW artifact under ops custody; verify presence + parties + period covering through GA. Do not invent SOW body in this repo. |

Do **not** promote any Gate 0 crown row to `ALIVE` from this repository.

---

## 2. In-repo stubs (observed — not execution proof)

These paths exist in-tree. They are **stubs / prose / archive / ontology mirrors**.
Standing for the Gate 0 *subjects* remains `BLOCKED` / `UNKNOWN` as above.

### 2.1 Billing (GCP / marketplace contract)

| Path | What it is | Standing as Gate 0 billing |
|------|------------|----------------------------|
| `docs/marketplace/billing-contract.md` | Marketplace billing chain contract (usage events → invoice → GCS/Firestore audit). **Definition of Done unchecked** (lines 533–554: schemas, reconciliation scripts, fraud path, etc. all `[ ]`). | **PARTIAL_ALIVE** as *prose*; **UNSUPPORTED** as live GCP billing DO |
| `docs/marketplace/entitlement-contract.md` | Entitlement FSM links billing plan / suspend-on-billing-failure | Companion prose only — does not run billing |
| `docs/marketplace/sku-catalog-structure.md` | SKU / cost-management catalog; links billing contract | Companion prose only |
| `examples/archive/gcp-erlang-autonomics/BILLING_GOVERNOR_IMPLEMENTATION.md` | Archived billing governor design | **UNSUPPORTED** (archive; not workspace member) |
| `examples/archive/gcp-erlang-autonomics/src/marketplace/billing_governor.rs` | Archived Rust governor | **UNSUPPORTED** |
| `examples/archive/gcp-erlang-autonomics/erlang_src/billing_governor.erl` (+ `billing_sup.erl`) | Archived Erlang FSM | **UNSUPPORTED** |
| `marketplace/packages/medical-billing/ontology/billing.ttl` | Domain ontology for *medical* billing packs | **REFUSED** as evidence for GCP Gate 0 billing (wrong subject) |
| `packs/autofde-semantic-registry-pack/ontology/10-cost-finops.ttl` | FinOps ontology pack metadata (`reg:recordStanding reg:ALIVE` is **pack-record** standing, not Plane-5 / GCP billing) | **REFUSED** as Gate 0 billing proof |

### 2.2 cos / FinFab / Plane 5

| Path | What it is | Standing as Gate 0 cos ledger |
|------|------------|-------------------------------|
| `docs/gh-terraform/FLEET-CENSUS.md` → `seanchatmangpt/cosg` | Fleet census row (private, unmanaged) | **UNKNOWN** (name only; sibling not mounted) |
| `ECOSYSTEM-SPEC-27.1.1.md` §3 / §4 / §13 | Mandates cos ledger fixes OUT OF SCOPE; Plane 5 cos/FinFab DO only via BRCE+WebAuthn AS-01 | Doctrine **PARTIAL_ALIVE** as text; cos ledger runtime **BLOCKED** |
| Live crates under `crates/` | No cos ledger / FinFab DO surface found | **UNSUPPORTED** in ggen |

### 2.3 servengine M4

| Path | What it is | Standing as Gate 0 servengine M4 |
|------|------------|----------------------------------|
| `examples/archive/factory-paas/verify_mermaid.mjs` | Mermaid parse helper; comment + default `FRONTEND_DIR` → `/Users/sac/srve/servengine-frontend` | **UNSUPPORTED** (archive; foreign absolute path; not a M4 court) |
| Fleet census / workspace members | No `servengine` repo mounted; not a Cargo workspace member | **BLOCKED** / **UNKNOWN** |

### 2.4 Year-end SOW

| Path | What it is | Standing as Gate 0 SOW |
|------|------------|------------------------|
| ggen tree (live docs + packs) | No year-end / commercial SOW register | **UNKNOWN** |
| `examples/archive/gcp-erlang-autonomics/docs/PRICING_STRATEGY.md` (SOW mention in sales prose) | Historical pricing talk-track | **REFUSED** as commercial SOW evidence |

---

## 3. Per-subject detail

### 3.1 GCP billing — `BLOCKED`

- **Mandate:** ECOSYSTEM-SPEC Gate 0 “GCP billing” — OUT OF SCOPE for ggen; do not claim `ALIVE` from this repo.
- **Observed stubs:** billing-contract + entitlement/sku companions; archived autonomics billing governor.
- **Falsifier that would still fail here:** any claim that `docs/marketplace/billing-contract.md` DoD checkboxes or archive Erlang/Rust governors constitute a GCP Marketplace Usage API + invoice + reconciliation receipt for the Gate 0 window.
- **Next verifier (external):**
  1. Identify admitted GCP billing account + Marketplace product.
  2. Produce period receipt (Gate 0 window → roll forward through GA as required by finance).
  3. Run weekly reconciliation against exported GCS audit path; exit 0 with signed summary.
  4. File typed receipt outside ggen (ops/infra custody). Attach digest only if a ggen *observation* pack is later admitted — observation ≠ DO.

### 3.2 cos ledger fixes — `BLOCKED`

- **Mandate:** ECOSYSTEM-SPEC Gate 0 “cos ledger fixes”; Plane 5 cos/FinFab is out of ggen DO.
- **Observed stubs:** `cosg` fleet census name only.
- **Next verifier (external):** in cos/FinFab/`cosg` — ledger replay/verify for the admitted Gate 0 fix set; typed receipt or `REFUSED_*`. Until that sibling is mounted and the court runs, standing stays `BLOCKED`.

### 3.3 servengine M4 — `BLOCKED`

- **Mandate:** ECOSYSTEM-SPEC Gate 0 “servengine M4”.
- **Observed stubs:** archive `verify_mermaid.mjs` dependency path only.
- **Next verifier (external):** servengine M4 milestone court in that repository (exact command per servengine CI). Resolving `FRONTEND_DIR` to a laptop absolute path is **not** a verifier.

### 3.4 Year-end SOW — `UNKNOWN`

- **Mandate:** ECOSYSTEM-SPEC Gate 0 “year-end SOW”.
- **Observed stubs:** none that are a commercial register.
- **Next verifier (external):** ops/commercial SOW register shows signed year-end SOW (parties, scope, period through GA 2027-01-01). Absence in ggen is expected; inventing SOW text here is `REFUSED_UNGROUNDED`.

---

## 4. GA ladder (Gate 0 → 2027-01-01)

| Checkpoint | Date | Required standing move |
|------------|------|------------------------|
| Gate 0 close | 2026-12-05 | Each crown row has external typed receipt **or** typed `REFUSED_*` / carry-forward with owner — still not claimable as `ALIVE` from ggen |
| Gate 1 | 2026-12-06 – 12-18 | Gate 0 rows remain out of ggen DO; do not block Gate 1 layout/`shui:` courts on billing/cos/servengine/SOW |
| Gate 2–3 | post Gate 1 | Plane 5 front doors may consume cos/FinFab only via BRCE+AS-01; still external to ggen manufacture |
| GA | **2027-01-01** | Unresolved Gate 0 subjects without typed closure remain hard GA risk (with R-01…R-07); do not silent-close |

---

## 5. Explicit non-claims

- No GCP billing console, Marketplace Usage API, or invoice pipeline was executed in this materialization.
- No cos ledger mutation or FinFab DO was attempted.
- No servengine M4 court was run (sibling not mounted).
- No year-end SOW was authored or signed in this repo.
- `packs/autofde-semantic-registry-pack` FinOps `reg:ALIVE` and medical-billing ontology are **not** Gate 0 billing.

---

## 6. Replay / next verifier for *this* document

```text
test -f docs/rfc/v27.1.1/GATE-0-STANDING.md
rg -n 'BLOCKED|UNKNOWN|OUT OF SCOPE' docs/rfc/v27.1.1/GATE-0-STANDING.md
rg -n 'billing-contract|billing_governor|servengine-frontend|cosg' docs/rfc/v27.1.1/GATE-0-STANDING.md
```

External crown verifiers remain owner-side (infra / cos / servengine / ops) through GA.

---

## 7. Provenance

Materialized as the Gate 0 standing companion to ECOSYSTEM-SPEC-27.1.1. Expands the
four-row Gate 0 table in `GATE-1-GAP-MATRIX.md` with stub citations and GA-bound
next verifiers. No Plane 5 / cos / GCP / servengine / SOW claim is asserted as
`ALIVE`.
