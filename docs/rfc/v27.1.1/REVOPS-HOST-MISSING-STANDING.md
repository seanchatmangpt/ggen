# revops.fnox.io — host MISSING (not a DNS-restore subject) (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `REVOPS-HOST-MISSING-STANDING` |
| Spec binding | OOR **G2-1** / Plane-5 front door honesty; Gate023 retip30 |
| Observed DNS | `dig +short revops.fnox.io A` → **empty**; `getent` NXDOMAIN; curl exit **6** |
| Observed siblings | `cos.fnox.io` → `34.8.148.142`; `arcus.fnox.io` / `smalt.fnox.io` / `fnox.io` resolve |
| fnox SoT | `seanchatmangpt/fnox` — `cos/docs/gtm/JOBS-LOCK-NOV21-RUNBOOK.md` |
| Sibling `seanchatmangpt/revops` | Private Nuxt repo **PRESENT** (cloned `/agent/repos/revops`) — **no** deploy↔`revops.fnox.io` link; product marketing site ≠ Plane-5 host |
| Standing | **UNSUPPORTED** (separate host) — **refuse** inventing DNS ALIVE |

**Law (fnox GTM):** `revops.fnox.io` as intake front door is **MISSING** — `infra/gcp/revops` **MISSING**. Do **not** publish or CTA there. Commercial “RevOps airlock” names a *function*; the *host of record* is **`cos.fnox.io` only** (Featured `/drop/:lease_id` gated while cos `/readyz` ≠ **200**).

Prior ggen OOR rows that treated NXDOMAIN as “Founder: create DNS until 200” were **mis-framed**. Correct next verifier for the RevOps *function* is cos readiness (G2-2), not inventing a second host.

---

## Exact next verifier (corrected)

```bash
# A) Host honesty (expect continue empty / NXDOMAIN — do NOT invent A records)
dig +short revops.fnox.io A
# expect: empty

# B) Function path (real Plane-5 intake)
curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://cos.fnox.io/readyz
# PASS for readiness: http=200 + {"status":"ready"}
# (today: 503 DB_UNAVAILABLE — see COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md)
```

**Refuse:** inventing `revops.fnox.io` ALIVE; promoting Nuxt `revops` repo presence to Plane-5 ALIVE; inventing Foundry / zero-HITL ALIVE from this pin.

---

## Compact return line

```text
revops_host=UNSUPPORTED_MISSING
dns=NXDOMAIN_intentional
function_host=cos.fnox.io
next=cos_readyz_200_not_revops_DNS
foundry=NOT_PROMOTED
```
