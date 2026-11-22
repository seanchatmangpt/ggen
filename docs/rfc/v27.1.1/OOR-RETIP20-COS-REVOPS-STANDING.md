# OOR Retip20 — cos / revops standing

```text
utc_probe=2026-11-22 11:19:24 PST
crown_base=6138ca663
cos_url=https://cos.fnox.io/readyz
cos_http=503
cos_body={"status":"DB_UNAVAILABLE"}
revops_dns=Host revops.fnox.io not found: 3(NXDOMAIN)
NXDOMAIN
revops_http=000000
cos_standing=BLOCKED
revops_standing=UNKNOWN
Foundry_ALIVE=not_claimed
```

## Standing (held)

| Surface | Standing | Evidence |
|---------|----------|----------|
| cos `/readyz` | **BLOCKED** | HTTP `503` body `{"status":"DB_UNAVAILABLE"}` |
| revops | **UNKNOWN** | DNS/HTTP `000000` — NXDOMAIN or unreachable |

Exact next: cos `/readyz` → **200**; revops DNS ∧ `/readyz` → **200**. Never invent ALIVE.
