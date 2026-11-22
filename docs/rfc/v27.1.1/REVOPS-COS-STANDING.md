# REVOPS + COS standing — Gate 0 / Gate 2 (ECOSYSTEM-SPEC-27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `REVOPS-COS-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1.md`, `GATE-1-GAP-MATRIX.md`, `REPO-INVENTORY-27.1.1.md` |
| Observed | 2026-11-22 |
| Observation host | Cursor cloud agent on `seanchatmangpt/ggen` @ `origin/cursor/ecosystem-spec-27-1-1-bc54` tip `df9597c18` (sidebranch `cursor/v2711-oor-retip14-bc54`; tip-wins absorb via `cursor/v2711-zcode-alive-oor-absorb-2230`; ⊇ HITL `--live` re-probe + CASTLE digest-bind + OOR retip13 `e2ca6a69a` + BRCE V8) / probe base `df9597c1817313f8d5173f425af28aaf485d114c` (retip14 reconfirm 2026-11-22 08:29:02 PST; prior retip13/`f65434315` + `098f53ea8` + retip12/`7ddfd8b6e` + retip11/`6a3a623ba` + retip10/`a40338aea` + retip9/`a508e6bae` + retip8/`5e131a75e` + honesty-retip8/`97c613452` + retip7 `a3b1c9410`) |
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

---

## 10. Tip reconfirm (2026-11-22) — retip8 `f1492ed55`

Observed on branch `cursor/v2711-oor-honesty-retip8-bc54` from tip
`97c61345233907a2d096ac188c28332ac4666c9f`. **No cargo.** tip-cargo locks
**untouched**. Probe-only Gate0/ops standing reconfirm. No capability
promotion; no false `ALIVE`.

| Probe | Result | Standing |
|-------|--------|----------|
| `getent hosts revops.fnox.io` | empty; exit **2** | **UNKNOWN** |
| `host revops.fnox.io` | **NXDOMAIN** (exit 1) | **UNKNOWN** |
| `curl https://revops.fnox.io/readyz` | exit **6** / `http=000` | **UNKNOWN** |
| `curl https://cos.fnox.io/healthz` | HTTP **200** body `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl https://cos.fnox.io/readyz` | HTTP **503** body `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1
**held**. Next exact verifiers unchanged from §4 / §9.

---

## 11. Tip reconfirm (2026-11-22 06:09:47 PST) — retip8 `5e131a75e`

Observed on branch `cursor/v2711-oor-retip8-bc54` from tip
`5e131a75e6051eb54340f14f8856ce5a8825769f`. **No cargo.** Probe unchanged vs
§10: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200**
`{"status":"ok"}`; `dig +short revops.fnox.io A`/`AAAA` empty; `host`
**NXDOMAIN**; `curl` exit **6** `http=000`. Labels **held** (cos readiness
**BLOCKED**; liveness **PARTIAL**; revops **UNKNOWN**). No false `ALIVE`.

---

## 12. Tip reconfirm (2026-11-22 07:12:40 PST) — retip9 `a508e6bae`

Observed on branch `cursor/v2711-oor-retip9-0e29` from tip
`a508e6bae9e61d7894ad98df8c507fdb1fb28eb2`. **No cargo.** Probe unchanged vs
§11: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200**
`{"status":"ok"}`; `dig +short revops.fnox.io A`/`AAAA` empty; `host`
**NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000`. Labels
**held** (cos readiness **BLOCKED**; liveness **PARTIAL**; revops
**UNKNOWN**). No ManagePullRequest. No false `ALIVE`.

| Probe | Result | Standing |
|-------|--------|----------|
| `dig +short revops.fnox.io A` / `AAAA` | empty | **UNKNOWN** |
| `host revops.fnox.io` | **NXDOMAIN** (exit 1) | **UNKNOWN** |
| `getent hosts revops.fnox.io` | empty; exit **2** | **UNKNOWN** |
| `curl https://revops.fnox.io/readyz` | exit **6** / `http=000` | **UNKNOWN** |
| `dig +short cos.fnox.io A` | `34.8.148.142` | edge DNS **PARTIAL** |
| `curl https://cos.fnox.io/healthz` | HTTP **200** body `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl https://cos.fnox.io/readyz` | HTTP **503** body `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1
**held**. Next exact verifiers unchanged from §4 / §9–§11.

---

## 13. Tip reconfirm (2026-11-22 07:37:36 PST) — retip10 `a40338aea`

Observed on branch `cursor/v2711-oor-retip10-ecdf` from tip
`a40338aea16bccd72cbc8baffe26b004155b34d4`. **No cargo.** Probe unchanged vs
§12: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200**
`{"status":"ok"}`; `dig +short revops.fnox.io A`/`AAAA` empty; `host`
**NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000`. Labels
**held** (cos readiness **BLOCKED**; liveness **PARTIAL**; revops
**UNKNOWN**). No ManagePullRequest. No false `ALIVE`.

| Probe | Result | Standing |
|-------|--------|----------|
| `dig +short revops.fnox.io A` / `AAAA` | empty | **UNKNOWN** |
| `host revops.fnox.io` | **NXDOMAIN** (exit 1) | **UNKNOWN** |
| `getent hosts revops.fnox.io` | empty; exit **2** | **UNKNOWN** |
| `curl https://revops.fnox.io/readyz` | exit **6** / `http=000` | **UNKNOWN** |
| `dig +short cos.fnox.io A` | `34.8.148.142` | edge DNS **PARTIAL** |
| `curl https://cos.fnox.io/healthz` | HTTP **200** body `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl https://cos.fnox.io/readyz` | HTTP **503** body `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1
**held**. Next exact verifiers unchanged from §4 / §9–§12.

---

## 14. Tip reconfirm (2026-11-22 07:42:56 PST) — retip11 `6a3a623ba`

Observed on branch `cursor/v2711-oor-retip11-fec0` from tip
`6a3a623bacd33efdc1ec7efccf6a35141662ca53` (post–ui_layout exact tip-cargo
absorb `6a3a623ba` on crown after retip10 `bdd84e105`). **No cargo.** Probe
unchanged vs §13: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`;
`/healthz` **200** `{"status":"ok"}`; `dig +short revops.fnox.io A`/`AAAA`
empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6**
`http=000`. Labels **held** (cos readiness **BLOCKED**; liveness
**PARTIAL**; revops **UNKNOWN**). No ManagePullRequest. No false `ALIVE`.

| Probe | Result | Standing |
|-------|--------|----------|
| `dig +short revops.fnox.io A` / `AAAA` | empty | **UNKNOWN** |
| `host revops.fnox.io` | **NXDOMAIN** (exit 1) | **UNKNOWN** |
| `getent hosts revops.fnox.io` | empty; exit **2** | **UNKNOWN** |
| `curl https://revops.fnox.io/readyz` | exit **6** / `http=000` | **UNKNOWN** |
| `dig +short cos.fnox.io A` | `34.8.148.142` | edge DNS **PARTIAL** |
| `curl https://cos.fnox.io/healthz` | HTTP **200** body `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl https://cos.fnox.io/readyz` | HTTP **503** body `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1
**held**. Next exact verifiers unchanged from §4 / §9–§13.

---

## 15. Tip reconfirm (2026-11-22 07:58:41 PST) — retip12 `7ddfd8b6e`

Observed on branch `cursor/v2711-oor-retip12-5958` from tip
`7ddfd8b6efd10ed21cd514f7bb17a5a63056a79c` (crown PR #849 after affidavit
size-court absorb; probe base was `1dbf1cc2e`). **No cargo.** Probe
unchanged vs §14: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`;
`/healthz` **200** `{"status":"ok"}`; `dig +short revops.fnox.io A`/`AAAA`
empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6**
`http=000`. Labels **held** (cos readiness **BLOCKED**; liveness
**PARTIAL**; revops **UNKNOWN**). No ManagePullRequest. No false `ALIVE`.

| Probe | Result | Standing |
|-------|--------|----------|
| `dig +short revops.fnox.io A` / `AAAA` | empty | **UNKNOWN** |
| `host revops.fnox.io` | **NXDOMAIN** (exit 1) | **UNKNOWN** |
| `getent hosts revops.fnox.io` | empty; exit **2** | **UNKNOWN** |
| `curl https://revops.fnox.io/readyz` | exit **6** / `http=000` | **UNKNOWN** |
| `dig +short cos.fnox.io A` | `34.8.148.142` | edge DNS **PARTIAL** |
| `curl https://cos.fnox.io/healthz` | HTTP **200** body `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl https://cos.fnox.io/readyz` | HTTP **503** body `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1
**held**. Next exact verifiers unchanged from §4 / §9–§14.

---

## 16. Tip reconfirm (2026-11-22 08:13:20 PST) — retip13 `f65434315`

Observed on branch `cursor/v2711-oor-retip13-bc54` @ `e2ca6a69a`; tip-wins
absorb onto crown `cc2b344bc` (⊇ BRCE V8 / `8d761d8a1`) retaining probe base
`f654343155c902a003a852f61df345e986d98b5e` (parent
`098f53ea8589c7ab05195fb6563c374bbc7765a4`; prior retip12 pin `7ddfd8b6e`).
**No cargo.** Probe unchanged vs §15: cos `/readyz` **503**
`{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`;
`dig +short revops.fnox.io A`/`AAAA` empty; `host` **NXDOMAIN**; `getent`
exit **2**; `curl` exit **6** `http=000`. Labels **held** (cos readiness
**BLOCKED**; liveness **PARTIAL**; revops **UNKNOWN**). No ManagePullRequest.
No false `ALIVE`.

Exact probe commands (this host, America/Los_Angeles):

```text
# 2026-11-22 08:13:20 PST (held from 08:11:26 PST on 098f53ea8)
curl -sS -o /tmp/cos-readyz13.json -w '%{http_code}' https://cos.fnox.io/readyz
# → 503 ; body {"status":"DB_UNAVAILABLE"}
curl -sS -o /tmp/cos-healthz13.json -w '%{http_code}' https://cos.fnox.io/healthz
# → 200 ; body {"status":"ok"}
curl -sS -o /tmp/revops-readyz13.json -w '%{http_code}' https://revops.fnox.io/readyz
# → exit 6 ; http_code=000 ; Could not resolve host: revops.fnox.io (NXDOMAIN)
```

| Probe | Result | Standing |
|-------|--------|----------|
| `dig +short revops.fnox.io A` / `AAAA` | empty | **UNKNOWN** |
| `host revops.fnox.io` | **NXDOMAIN** (exit 1) | **UNKNOWN** |
| `getent hosts revops.fnox.io` | empty; exit **2** | **UNKNOWN** |
| `curl https://revops.fnox.io/readyz` | exit **6** / `http=000` | **UNKNOWN** |
| `dig +short cos.fnox.io A` | `34.8.148.142` | edge DNS **PARTIAL** |
| `curl https://cos.fnox.io/healthz` | HTTP **200** body `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl https://cos.fnox.io/readyz` | HTTP **503** body `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1
**held**. Next exact verifiers unchanged from §4 / §9–§15.

---

## 17. Tip reconfirm (2026-11-22 08:29:02 PST) — retip14 `df9597c18`

Observed on branch `cursor/v2711-oor-retip14-bc54` @ `72662eeff` from tip
`df9597c1817313f8d5173f425af28aaf485d114c` (parent
`7ea96a70df61ad782078775e20ff692de50ec067`; prior retip13 pin `f65434315` /
`e2ca6a69a`). **No cargo.** Sidebranch only — crown **not** pushed. Probe
unchanged vs §16: cos `/readyz` **503** `{"status":"DB_UNAVAILABLE"}`;
`/healthz` **200** `{"status":"ok"}`; `dig +short revops.fnox.io A`/`AAAA`
empty; `host` **NXDOMAIN**; `getent` exit **2**; `curl` exit **6** `http=000`.
Labels **held** (cos readiness **BLOCKED**; liveness **PARTIAL**; revops
**UNKNOWN**). No ManagePullRequest. No false `ALIVE`.

Exact probe commands (this host, America/Los_Angeles):

```text
# 2026-11-22 08:29:02 PST
curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz
# → 503 ; body {"status":"DB_UNAVAILABLE"}
curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/healthz
# → 200 ; body {"status":"ok"}
curl -sS -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz
# → exit 6 ; http_code=000 ; Could not resolve host: revops.fnox.io (NXDOMAIN)
```

| Probe | Result | Standing |
|-------|--------|----------|
| `dig +short revops.fnox.io A` / `AAAA` | empty | **UNKNOWN** |
| `host revops.fnox.io` | **NXDOMAIN** (exit 1) | **UNKNOWN** |
| `getent hosts revops.fnox.io` | empty; exit **2** | **UNKNOWN** |
| `curl https://revops.fnox.io/readyz` | exit **6** / `http=000` | **UNKNOWN** |
| `dig +short cos.fnox.io A` | `34.8.148.142` | edge DNS **PARTIAL** |
| `curl https://cos.fnox.io/healthz` | HTTP **200** body `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl https://cos.fnox.io/readyz` | HTTP **503** body `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1
**held**. Next exact verifiers unchanged from §4 / §9–§16.

---

## 18. Tip reconfirm (2026-11-22 08:40:12 PST) — retip15 `01eccfffb`

Observed on branch `cursor/v2711-oor-retip15-bc54` @ `5c969ac05` (side tip
`a6f90ea9b`) from tip `01eccfffb56c1c59be67a220eadb5bd98783c806` (parent
`1f10949a78523348951f55583d94bdac7bf63c66`; prior retip14 pin `df9597c18` /
`72662eeff`). **No cargo.** Tip-wins absorb onto crown tip `bebea5dd0` via
`cursor/v2711-oor15-curve-absorb-ded8`. Probe unchanged vs §17: cos `/readyz`
**503** `{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`;
`dig +short revops.fnox.io A`/`AAAA` empty; `host` **NXDOMAIN**; `getent` exit
**2**; `curl` exit **6** `http=000`. Labels **held** (cos readiness **BLOCKED**;
liveness **PARTIAL**; revops **UNKNOWN**). No ManagePullRequest. No false
`ALIVE`. ALIVE gate unmet (requires cos `/readyz` **200** ∧ revops `/readyz`
**200**).

Exact probe commands (this host, America/Los_Angeles):

```text
# 2026-11-22 08:40:12 PST
curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz
# → 503 ; body {"status":"DB_UNAVAILABLE"}
curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/healthz
# → 200 ; body {"status":"ok"}
getent hosts revops.fnox.io
# → empty; exit 2
curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://revops.fnox.io/readyz
# → exit 6 ; http_code=000 ; Could not resolve host: revops.fnox.io (NXDOMAIN)
```

| Probe | Result | Standing |
|-------|--------|----------|
| `dig +short revops.fnox.io A` / `AAAA` | empty | **UNKNOWN** |
| `host revops.fnox.io` | **NXDOMAIN** (exit 1) | **UNKNOWN** |
| `getent hosts revops.fnox.io` | empty; exit **2** | **UNKNOWN** |
| `curl https://revops.fnox.io/readyz` | exit **6** / `http=000` | **UNKNOWN** |
| `dig +short cos.fnox.io A` | `34.8.148.142` | edge DNS **PARTIAL** |
| `curl https://cos.fnox.io/healthz` | HTTP **200** body `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl https://cos.fnox.io/readyz` | HTTP **503** body `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1
**held**. Next exact verifiers unchanged from §4 / §9–§17.

---

## 19. Tip reconfirm (2026-11-22 08:59:55 PST) — retip16 `acd3b8c89`

Observed on branch `cursor/v2711-oor-retip16-bc54` @ `0032fc0b8` (side tip `423e7a831`) from tip
`acd3b8c89e57d3e4ced125f5a6f9440eb74b86d3` (parent
`b49ca04eaea9fd54301570491c9749a15ebc08ee`; prior retip15 pin `01eccfffb` /
`a6f90ea9b`). **No cargo.** Sidebranch only — crown **not** absorbed / **not**
pushed to #849. Probe unchanged vs §18: cos `/readyz` **503**
`{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`;
`dig +short revops.fnox.io A`/`AAAA` empty; `host` **NXDOMAIN**; `getent` exit
**2**; `curl` exit **6** `http=000`. Labels **held** (cos readiness **BLOCKED**;
liveness **PARTIAL**; revops **UNKNOWN**). No ManagePullRequest. No false
`ALIVE`. ALIVE gate unmet (requires cos `/readyz` **200** ∧ revops `/readyz`
**200**). Earlier same-host probe 08:58:06 PST identical.

Exact probe commands (this host, America/Los_Angeles):

```text
# 2026-11-22 08:59:55 PST (held from 08:58:06 PST identical)
curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\nerr=%{errormsg}\ntime_total=%{time_total}\nremote_ip=%{remote_ip}\n' https://cos.fnox.io/readyz
# → 503 ; body {"status":"DB_UNAVAILABLE"} ; remote_ip=34.8.148.142
curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\nerr=%{errormsg}\ntime_total=%{time_total}\nremote_ip=%{remote_ip}\n' https://cos.fnox.io/healthz
# → 200 ; body {"status":"ok"} ; remote_ip=34.8.148.142
getent hosts revops.fnox.io
# → empty; exit 2
curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\nerr=%{errormsg}\ntime_total=%{time_total}\n' https://revops.fnox.io/readyz
# → exit 6 ; http_code=000 ; Could not resolve host: revops.fnox.io (NXDOMAIN)
```

| Probe | Result | Standing |
|-------|--------|----------|
| `dig +short revops.fnox.io A` / `AAAA` | empty | **UNKNOWN** |
| `host revops.fnox.io` | **NXDOMAIN** (exit 1) | **UNKNOWN** |
| `getent hosts revops.fnox.io` | empty; exit **2** | **UNKNOWN** |
| `curl https://revops.fnox.io/readyz` | exit **6** / `http=000` | **UNKNOWN** |
| `dig +short cos.fnox.io A` | `34.8.148.142` | edge DNS **PARTIAL** |
| `curl https://cos.fnox.io/healthz` | HTTP **200** body `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl https://cos.fnox.io/readyz` | HTTP **503** body `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1
**held**. Next exact verifiers unchanged from §4 / §9–§18.

## 20. Tip reconfirm (2026-11-22 09:20:38 PST) — retip17 `571571720`

Observed on branch `cursor/v2711-oor-retip17-bc54` from tip
`571571720620cd7462b9a740e3bf0b0fdeebc17a` (prior retip16 pin `acd3b8c89` /
`423e7a831`). **No cargo.** Sidebranch only — crown **not** absorbed / **not**
pushed to #849. Probe unchanged vs §19: cos `/readyz` **503**
`{"status":"DB_UNAVAILABLE"}`; `/healthz` **200** `{"status":"ok"}`;
`dig +short revops.fnox.io A`/`AAAA` empty; `host` **NXDOMAIN**; `getent` exit
**2**; `curl` exit **6** `http=000`. Labels **held** (cos readiness **BLOCKED**;
liveness **PARTIAL**; revops **UNKNOWN**). No ManagePullRequest. No false
`ALIVE`. ALIVE gate unmet (requires cos `/readyz` **200** ∧ revops `/readyz`
**200**).

Exact probe commands (this host, America/Los_Angeles):

```text
# 2026-11-22 09:20:38 PST
curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\nerr=%{errormsg}\ntime_total=%{time_total}\nremote_ip=%{remote_ip}\n' https://cos.fnox.io/readyz
# → 503 ; body {"status":"DB_UNAVAILABLE"} ; remote_ip=34.8.148.142 ; time_total≈0.21s
curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\nerr=%{errormsg}\ntime_total=%{time_total}\nremote_ip=%{remote_ip}\n' https://cos.fnox.io/healthz
# → 200 ; body {"status":"ok"} ; remote_ip=34.8.148.142 ; time_total≈0.13s
getent hosts revops.fnox.io
# → empty; exit 2
curl -sS -w '\nhttp_code=%{http_code}\nexit_curl=%{exitcode}\nerr=%{errormsg}\ntime_total=%{time_total}\n' https://revops.fnox.io/readyz
# → exit 6 ; http_code=000 ; Could not resolve host: revops.fnox.io (NXDOMAIN)
```

| Probe | Result | Standing |
|-------|--------|----------|
| `dig +short revops.fnox.io A` / `AAAA` | empty | **UNKNOWN** |
| `host revops.fnox.io` | **NXDOMAIN** (exit 1) | **UNKNOWN** |
| `getent hosts revops.fnox.io` | empty; exit **2** | **UNKNOWN** |
| `curl https://revops.fnox.io/readyz` | exit **6** / `http=000` | **UNKNOWN** |
| `dig +short cos.fnox.io A` | `34.8.148.142` | edge DNS **PARTIAL** |
| `curl https://cos.fnox.io/healthz` | HTTP **200** body `{"status":"ok"}` | **PARTIAL** (liveness ≠ readiness) |
| `curl https://cos.fnox.io/readyz` | HTTP **503** body `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |

Crown: Plane-5 revops+cos product standing is **not** `ALIVE`. Labels in §1
**held**. Next exact verifiers unchanged from §4 / §9–§19.
