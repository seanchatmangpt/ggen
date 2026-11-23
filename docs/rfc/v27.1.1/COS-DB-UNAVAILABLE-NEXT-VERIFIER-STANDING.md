# cos.fnox.io — DB_UNAVAILABLE next verifier (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING` |
| Spec binding | OOR **G0-2** / **G2-2**; Gate023 retip28+ |
| Observed probe | `curl https://cos.fnox.io/readyz` → **503** `{"status":"DB_UNAVAILABLE"}` (2026-11-23T00:25Z; healthz **200** held) |
| Source of truth | `seanchatmangpt/fnox` @ clone `/agent/repos/fnox` — `cos/lib/fnox_cos_web/controllers/health_controller.ex` |
| Standing | **BLOCKED** (readiness) — **≠** Foundry ALIVE; **≠** zero-HITL ALIVE |
| Agent capability this env | **UNSUPPORTED** for restore (no `gcloud` auth; no Cloud SQL credentials) |

**Law:** `/healthz` **200** proves process liveness only. `/readyz` **503** `DB_UNAVAILABLE` means `FnoxCos.Repo` failed `SELECT 1` (or exited during the check) — **not** `MIGRATIONS_PENDING`, **not** `DB_PRIVILEGE`. Collapsing those tokens is a falsifier (`health_controller_test.exs`).

---

## Source pin (fnox)

From `cos/lib/fnox_cos_web/controllers/health_controller.ex`:

- `readyz` → 200 only when `readiness() == :ok`
- `db_up/0` runs `Ecto.Adapters.SQL.query(Repo, "SELECT 1", [])`
- `{:error, _}` / `DBConnection.ConnectionError` / `:exit` → **`DB_UNAVAILABLE`**
- Distinct tokens: `ready` | `DB_UNAVAILABLE` | `DB_PRIVILEGE` | `MIGRATIONS_PENDING`

Cloud Run TF SoT: `infra/gcp/cos/main.tf` wires readiness probe `path = "/readyz"` and migrate job `cos-migrate` / `fnox-cos-migrate` with IAP JWKS mount.

---

## Exact Founder HITL next verifier

```bash
# 1) Confirm token (must stay DB_UNAVAILABLE until DB answers)
curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://cos.fnox.io/readyz
# expect until fixed: http=503 + {"status":"DB_UNAVAILABLE"}

# 2) Restore Cloud SQL reachability for cos Cloud Run (Founder / GCP)
#    - Cloud SQL instance UP (not stopped)
#    - Secret Manager DATABASE_URL bound to cos service
#    - VPC / Cloud SQL Auth Proxy / unix socket path intact
#    - Role fnox_cos_app can SELECT 1 (NOCREATE is OK; SET ROLE after_connect)

# 3) If SELECT 1 works but migrations pending → token becomes MIGRATIONS_PENDING
#    then run migrate job (names from TF):
#      gcloud run jobs execute cos-migrate …   # or fnox-cos-migrate
#    until /readyz → 200 {"status":"ready"}

# 4) PASS
curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 https://cos.fnox.io/readyz
# expect: http=200 + {"status":"ready"}

# 5) Then (agent-capable once mounted): just cos-ledger-verify → EXIT 0
```

**Refuse:** inventing cos ALIVE from `/healthz` 200; inventing Foundry / zero-HITL ALIVE from this pin.

---

## Compact return line

```text
cos_readyz=503_DB_UNAVAILABLE
cause=Repo_SELECT_1_fail
healthz=200_liveness_only
next=Founder_GCP_restore_DATABASE_URL_CloudSQL_then_readyz_200
agent=UNSUPPORTED_no_gcloud_auth
foundry=NOT_PROMOTED
```
