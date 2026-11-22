# REVOPS + COS standing — Gate 0 / Gate 2 (ECOSYSTEM-SPEC-27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `REVOPS-COS-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1.md`, `GATE-1-GAP-MATRIX.md`, `REPO-INVENTORY-27.1.1.md` |
| Observed | 2026-11-22 |
| Observation host | Cursor cloud agent on `seanchatmangpt/ggen` @ `origin/cursor/ecosystem-spec-27-1-1-bc54` tip `a3b1c94103984e2d362c068deb61cd700615e158` (retip7 cos/ops probe) |
| Scope | Plane 5 front doors + Gate 0 cos/billing adjacency — **out of ggen DO** |

Standing labels used here (per request; map to AGENTS.md as noted):

| This doc | AGENTS.md equivalent |
|----------|----------------------|
| `ALIVE` | `ALIVE` |
| `PARTIAL` | `PARTIAL_ALIVE` |
| `BLOCKED` | `BLOCKED` |
| `UNKNOWN` | `UNKNOWN` |

**Hard rule:** HTTP reachability is never promoted to `ALIVE` without an exact admitted verifier
(readyz/ledger/court exit 0). A 200 on a liveness probe is `PARTIAL` at most. A 404/503/DNS
failure is never rewritten as success.

---

## 1. Standing table (Gate 0 / Gate 2 subjects)

| Subject | Standing | Evidence observed 2026-11-22 | Owner / plane |
|---------|----------|------------------------------|---------------|
| `revops.fnox.io` front door | **UNKNOWN** | `getent` empty; `curl https://revops.fnox.io/` → exit 6 *Could not resolve host* (`http_code=000`). No TLS handshake. | Plane 5 / revops·fnox |
| `cos.fnox.io` edge presence | **PARTIAL** | DNS → `34.8.148.142`; TLS terminates (`server: Google Frontend`). Root `GET /` → **HTTP 404** typed HTML `data-code="NOT_FOUND"` (not a product page). | Plane 5 / cos·fnox |
| `cos.fnox.io` liveness `/healthz` | **PARTIAL** | `GET https://cos.fnox.io/healthz` → **HTTP 200** body `{"status":"ok"}`. Process/edge up; does **not** prove ledger or product readiness. | cos |
| `cos.fnox.io` readiness `/readyz` | **BLOCKED** | `GET https://cos.fnox.io/readyz` → **HTTP 503** body `{"status":"DB_UNAVAILABLE"}`. Explicit readiness refusal. | cos (DB dependency) |
| cos / FinFab ledger surface | **UNKNOWN** | `GET /ledger`, `GET /finfab`, `GET /api/health` → **HTTP 404** (same NOT_FOUND shell as `/`). No ledger replay or FinFab mutation exercised from this host. | cos / FinFab |
| GCP billing (Gate 0) | **UNKNOWN** | No live billing console receipt, no GCP Billing API call from this observation. Spec marks **OUT OF SCOPE** for ggen DO. | infra / finance |
| Billing hooks inside `ggen` | **UNKNOWN** | No `BillingHook` / `billing_hook` symbol under live crates. Marketplace `docs/marketplace/billing-contract.md` is contract prose only. `examples/archive/gcp-erlang-autonomics/**` billing governor is archived, not a live workspace member. Pack name `gcp-billing-budgets-pack` appears only as a Fortune5 catalog string. | ggen (docs/archive only) |
| Sibling `seanchatmangpt/revops` | **PARTIAL** | Private GitHub repo exists (Vue/NuxtHub; default `main`; tip observed via API tree). **Not mounted** in this workspace; no deploy↔`revops.fnox.io` link verified. Fleet census: unmanaged. | revops |
| Sibling `seanchatmangpt/fnox` (`cos/`, `ledger/`) | **PARTIAL** | Private repo exists (Elixir; tip `8044ede7…` 2026-11-22). Tree contains `cos/` and `ledger/` directories; recent commits mention cos `readyz` terraform tests. **Not mounted**; source presence ≠ runtime ALIVE. | fnox / cos |
| `seanchatmangpt/cosg` | **UNKNOWN** | Fleet census + GitHub “Chief Of Staff GPT” — **not** established as `cos.fnox.io` / FinFab owner. Do not conflate. | unknown / distinct |
| ggen DO over Plane 5 | **BLOCKED** | `ECOSYSTEM-SPEC-27.1.1` Plane 5: DO only via BRCE+WebAuthn AS-01. This repo has no AS-01 runtime claim for revops/cos. | constitutional |

Crown claim for this slice: **Plane 5 revops+cos product standing is not `ALIVE` from ggen evidence.**

---

## 2. Search method (ggen tree)

Commands / scopes used on this tip:

```text
rg -n -i 'revops\.fnox|cos\.fnox|fnox\.io|FinFab' --glob '!**/target/**' --glob '!ontology_catalogue/**'
rg -n -i 'billing.?hook|BillingHook|gcp.?billing' --glob '!**/target/**' --glob '!docs/archive/**' --glob '!ontology_catalogue/**'
rg -n 'revops|cosg' docs/gh-terraform/FLEET-CENSUS.md
```

Material hits (names only — not ALIVE):

| Path | What it is |
|------|------------|
| `docs/rfc/v27.1.1/ECOSYSTEM-SPEC-27.1.1.md` (when present on sibling PR tips) | D6 / Gate 0 / Plane 5 naming |
| `docs/gh-terraform/FLEET-CENSUS.md` | `seanchatmangpt/revops`, `chatmangpt-org/revops`, `seanchatmangpt/cosg` rows |
| `ontology_catalogue/open-ontologies/ontology/revops-manufacturing.ttl` | Catalogue ontology (description), not runtime |
| `docs/marketplace/billing-contract.md` | Marketplace billing chain prose |
| `examples/archive/gcp-erlang-autonomics/**` | Archived GCP billing governor examples |
| `packs/fortune5-deployment-blocks-pack/**` | Catalog string `gcp-billing-budgets-pack` |

No live crate under `crates/` implements revops.fnox / cos.fnox hosting or a billing hook actuator.

---

## 3. External HTTP probes (exact; not success theater)

Observed from the agent host on 2026-11-22. Failures are recorded as failures.

| Probe | Result | Standing implication |
|-------|--------|----------------------|
| `curl -sS -o /dev/null -w '%{http_code}' https://revops.fnox.io/` | exit 6, `http_code=000` | DNS absent → **UNKNOWN** |
| `curl -sS -D - https://cos.fnox.io/` | HTTP **404**, Google Frontend, body `NOT_FOUND` | Edge up, product root absent → **PARTIAL** edge / not ALIVE product |
| `curl -sS https://cos.fnox.io/healthz` | HTTP **200** `{"status":"ok"}` | Liveness **PARTIAL** |
| `curl -sS https://cos.fnox.io/readyz` | HTTP **503** `{"status":"DB_UNAVAILABLE"}` | Readiness **BLOCKED** |
| `curl -sS https://cos.fnox.io/ledger` | HTTP **404** | Ledger HTTP surface **UNKNOWN** |
| `curl -sS https://fnox.io/` | HTTP **401** | Apex auth wall; not claimed as revops/cos ALIVE |

Do **not** treat `/healthz` 200 as ledger ALIVE, FinFab ALIVE, or Gate 0 closure.

---

## 4. Exact next external verifiers

These are the next falsifiers outside ggen. Run them in the owning plane; paste exit codes / bodies into a successor receipt. Until then, standings above stay as written.

### Gate 0 — cos / billing

1. **cos readiness (DB)** — on an authorized operator host with DB credentials admitted:
   ```text
   curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz
   ```
   Pass criterion for readiness claim: HTTP **200** with an admitted ready body (not `DB_UNAVAILABLE`). Until then: keep **BLOCKED**.

2. **cos ledger replay/verify** — in `seanchatmangpt/fnox` (or owning cos checkout):
   ```text
   # exact recipe name may differ; prefer the repo’s documented ledger court, e.g.:
   just cos-ledger-verify
   # or the mix/test target the cos CLAUDE.md / Justfile names for ledger replay
   ```
   Pass criterion: exit 0 + receipt binding ledger tip. Source tree alone stays **PARTIAL**/not ALIVE.

3. **GCP billing console / infra receipt** — infra owner:
   ```text
   # exact: GCP Billing account invoice export OR marketplace billing reconciliation job
   # with a dated receipt artifact (GCS/Firestore path per docs/marketplace/billing-contract.md)
   ```
   Pass criterion: dated external receipt. ggen archive examples do not count.

### Gate 2 — revops plane / front doors

4. **revops DNS + TLS + ready**:
   ```text
   getent hosts revops.fnox.io
   curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz
   ```
   Pass criterion: DNS resolves **and** HTTP 200 on the admitted ready path. Current DNS failure keeps **UNKNOWN**.

5. **revops deploy ↔ repo binding** — in `seanchatmangpt/revops` or `seanchatmangpt/fnox` infra:
   ```text
   # terraform/plan or hosting console showing hostname revops.fnox.io → admitted revision SHA
   ```
   Pass criterion: hostname maps to an exact git SHA with a deploy receipt.

6. **Plane 5 DO path** — only after BRCE+WebAuthn AS-01 admits the subject:
   ```text
   # AS-01 session + BRCE intent for the named front-door change; refuse ambient agent DO
   ```
   Until AS-01 runtime is receipted for these hosts: ggen DO remains **BLOCKED**.

---

## 5. What this document does **not** claim

- No fabricated HTTP 200 for `revops.fnox.io` or for `cos.fnox.io/` product root.
- No `ALIVE` for cos ledger, FinFab mutation, GCP billing, or revops product UX.
- No ggen authority to actuate Plane 5 hosts.
- Catalogue TTL / fleet census / marketplace markdown / archived examples are names only.

---

## 6. Replay of this observation

```text
git -C /path/to/ggen rev-parse HEAD   # expect base used when this file was written
curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/
curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/
curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/healthz
curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz
```

If any probe’s standing changes, amend this file with a new dated observation block; do not silently rewrite history.

---

## 7. Tip reconfirm (2026-11-22)

Observed on `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `7bf148d82e125589859d8830d29a3ce899d28f3f`.

| Probe | Result | Standing |
|-------|--------|----------|
| `getent hosts revops.fnox.io` | empty | **UNKNOWN** |
| `curl https://revops.fnox.io/` | exit 6 / `http=000` | **UNKNOWN** |
| `curl https://cos.fnox.io/` | HTTP **404** `NOT_FOUND` | **PARTIAL** (edge) |
| `curl https://cos.fnox.io/healthz` | HTTP **200** `{"status":"ok"}` | **PARTIAL** |
| `curl https://cos.fnox.io/readyz` | HTTP **503** `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |
| `curl https://cos.fnox.io/ledger` | HTTP **404** | **UNKNOWN** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1 **held**. Next verifiers remain §4.

---

## 8. Tip reconfirm (2026-11-22) — crown `5f7c755a6`

Observed on `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `5f7c755a61634bf1c0370f8552d43bf7803ccc17` (post–CCS ops honesty demotion FF; prior pins `c9e224d33` / `7bf148d82` remain historical in §7). Probe standings **held**; no capability promotion; no false `ALIVE`. Honesty demotions on tip (CCS/ops BRCE/brand/`verify_witness` → **PARTIAL_ALIVE**) do not promote Plane-5 revops+cos.

| Probe | Result | Standing |
|-------|--------|----------|
| `getent hosts revops.fnox.io` | empty | **UNKNOWN** |
| `curl https://revops.fnox.io/` | exit 6 / `http=000` | **UNKNOWN** |
| `curl https://cos.fnox.io/` | HTTP **404** `NOT_FOUND` | **PARTIAL** (edge) |
| `curl https://cos.fnox.io/healthz` | HTTP **200** `{"status":"ok"}` | **PARTIAL** |
| `curl https://cos.fnox.io/readyz` | HTTP **503** `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |
| `curl https://cos.fnox.io/ledger` | HTTP **404** | **UNKNOWN** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1 **held**. Next verifiers remain §4.

---

## 9. Tip reconfirm (2026-11-22) — retip7 `a3b1c9410`

Observed on branch `cursor/v2711-oor-retip7-cos-bc54` from tip
`a3b1c94103984e2d362c068deb61cd700615e158` (ecosystem crown lineage through
retip6 `ca52e9c16`). **No cargo.** tip-cargo locks **untouched**. Probe-only
Gate0/ops standing. No capability promotion; no false `ALIVE`.

| Probe | Result | Standing |
|-------|--------|----------|
| `getent hosts revops.fnox.io` | empty; exit **2** | **UNKNOWN** |
| `host revops.fnox.io` | **NXDOMAIN** (exit 1) | **UNKNOWN** |
| `curl https://revops.fnox.io/` | exit **6** / `http=000` | **UNKNOWN** |
| `getent hosts cos.fnox.io` | `34.8.148.142` | edge DNS **PARTIAL** |
| `curl https://cos.fnox.io/healthz` | HTTP **200** body `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl https://cos.fnox.io/readyz` | HTTP **503** body `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1
**held** (`revops` **UNKNOWN**; cos `/healthz` **PARTIAL**; cos `/readyz`
**BLOCKED**). Next exact verifiers remain §4 items 1–2 (cos DB readiness →
**200** not `DB_UNAVAILABLE`; fnox ledger replay) and §4 item 4 (revops DNS ∧
`/readyz` **200**).
