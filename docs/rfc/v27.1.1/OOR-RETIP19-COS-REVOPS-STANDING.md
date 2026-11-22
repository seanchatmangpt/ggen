# OOR retip19 — cos + revops probe honesty (v27.1.1)

```text
tip_base=dd61fefb29fa7ea6e1cf789f002ccd9879393988
side=cursor/v2711-oor-retip19-bc54
agent=bc-f83f4cbf
utc=2026-11-22T19:02:01Z
pst=2026-11-22 11:02:01 PST
cargo=NOT_RUN
foundry=NOT_CLAIMED (never invent Foundry ALIVE)
standing=HELD (cos BLOCKED / revops UNKNOWN)
```

| Field | Value |
|-------|-------|
| Document ID | `OOR-RETIP19-COS-REVOPS-STANDING` |
| Companion | [`GATE023-OOR-STANDING-MATRIX.md`](./GATE023-OOR-STANDING-MATRIX.md); [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md) §22 |
| Crown | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `dd61fefb29fa7ea6e1cf789f002ccd9879393988` (`dd61fefb2`) |
| Sidebranch | `cursor/v2711-oor-retip19-bc54` |
| Agent | `bc-f83f4cbf` |
| Observed | 2026-11-22 11:02:01 PST / `2026-11-22T19:02:01Z` |
| Cargo | **NOT RUN** (docs-only honesty retip) |

**Hard rule (held):** Do not invent success. HTTP reachability ≠ readiness ≠ product `ALIVE`. A `/readyz` **503** is **BLOCKED**. DNS absence is **UNKNOWN**. Never invent Foundry `ALIVE`. No ManagePullRequest. No force-push.

---

## Probe results

| Probe | Result | Standing |
|-------|--------|----------|
| `curl https://cos.fnox.io/readyz` | **503** `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |
| `curl https://cos.fnox.io/healthz` | **200** `{"status":"ok"}` | **PARTIAL** ≠ ALIVE |
| `host` / `getent` `revops.fnox.io` | **NXDOMAIN** / exit **2** | **UNKNOWN** |
| `curl https://revops.fnox.io/` / `/readyz` | exit **6** / `http=000` | **UNKNOWN** |
| `curl https://cos.ggen.dev/readyz` | exit **6** / `http=000` (**NXDOMAIN**) | alias **UNKNOWN** |
| `host revops.ggen.dev` / `curl https://revops.ggen.dev/` | **NXDOMAIN** / exit **6** | alias **UNKNOWN** |

Standings **held** vs retip18 / Gate023 matrix. No promotion. Plane-5 product **not** `ALIVE`.

---

## Exact next verifiers

```bash
curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://cos.fnox.io/readyz
# pass: HTTP 200 (not DB_UNAVAILABLE)
getent hosts revops.fnox.io
curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://revops.fnox.io/readyz
# pass: DNS non-empty ∧ HTTP 200
```

Until those pass: keep cos **BLOCKED**, revops **UNKNOWN**.
