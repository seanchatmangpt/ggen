# cos.fnox.io — DB_UNAVAILABLE next verifier (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING` |
| Spec binding | OOR **G0-2** / **G2-2**; Gate023 retip31 |
| Observed probe | `curl https://cos.fnox.io/readyz` → **503** `{"status":"DB_UNAVAILABLE"}` (healthz **200** held) |
| fnox SoT | `seanchatmangpt/fnox` — `cos/docs/gtm/OPERATOR-AND-GATE-readyz.md` + `L10-readyz-DB_UNAVAILABLE-receipt.md` |
| Live evidence (fnox L10, 2026-11-22) | Cloud Logging: **`SELECT 1` QUERY OK** then CREATE `schema_migrations` fails under `SET ROLE fnox_cos_app` → controller token **false-`DB_UNAVAILABLE`** |
| Cloud SQL | `ai-cmd-ctr:us-west1:cos-pg` **RUNNABLE**; DB `cos` exists (L10) |
| Billing hold | `018FD7-382078-0D52EF` on project `ai-cmd-ctr` — **operator only** (agents must not clear) |
| Standing | **BLOCKED** (operator AND-gate) — **≠** Foundry ALIVE; **≠** zero-HITL ALIVE |
| Agent capability | **UNSUPPORTED** for live GCP / billing clear |

**Law:** `/healthz` **200** = liveness only. Live `/readyz` **503** `DB_UNAVAILABLE` is **not** always “DB down.” L10 shows Repo can answer `SELECT 1` while readiness still false-tokens when `Ecto.Migrator.migrations/1` tries CREATE under `fnox_cos_app`. On-disk fix = `skip_table_creation: true` in `HealthController` — **must be on the live revision image**. Distinct tokens: `ready` | `DB_UNAVAILABLE` | `DB_PRIVILEGE` | `MIGRATIONS_PENDING`.

---

## Exact Founder HITL next verifier (copy-paste SoT)

Mnemonic: **`billing → JWKS → migrate → redeploy → readyz 200`**  
Executable page (fnox): `cos/docs/gtm/OPERATOR-AND-GATE-readyz.md`  
Do **not** reorder. Do not migrate before JWKS. Agents do **not** clear billing.

```bash
export PROJECT=ai-cmd-ctr
export REGION=us-west1
export SERVICE=cos
export MIGRATE_JOB=cos-migrate
export IAP_SECRET=cos-iap-jwks
export HOST=https://cos.fnox.io
# export IMAGE='…/fnox-cos@sha256:…'  # digest with skip_table_creation + /readyz probe

# 1) Billing — clear hold 018FD7-382078-0D52EF (operator console only)
gcloud services list --enabled --project="$PROJECT" \
  --filter='config.name:(run.googleapis.com OR sqladmin.googleapis.com OR secretmanager.googleapis.com)' \
  --format='value(config.name)'

# 2) JWKS — mount IAP JWKS on migrate job (+ service if drifted)
gcloud secrets versions access latest --secret="$IAP_SECRET" --project="$PROJECT" >/dev/null
gcloud run jobs update "$MIGRATE_JOB" --project="$PROJECT" --region="$REGION" \
  --update-secrets=/etc/fnox/iap/iap_jwks.json="${IAP_SECRET}:latest" \
  --update-env-vars=IAP_JWKS_PATH=/etc/fnox/iap/iap_jwks.json

# 3) Migrate
gcloud run jobs execute "$MIGRATE_JOB" --project="$PROJECT" --region="$REGION" --wait

# 4) Redeploy revision that includes skip_table_creation: true
#    Prefer terraform apply -var="image=${IMAGE}" in infra/gcp/cos
#    or: gcloud run services update "$SERVICE" --image="$IMAGE" …

# 5) PASS
curl -sS -w '\nhttp=%{http_code}\n' --connect-timeout 10 "$HOST/readyz"
# expect: http=200 + {"status":"ready"}
```

**Refuse:** inventing cos ALIVE from `/healthz` 200 or from “SELECT 1 works”; inventing Foundry / zero-HITL ALIVE from this pin; agent clearing billing hold.

---

## Compact return line

```text
cos_readyz=503_false_DB_UNAVAILABLE
select1=OK_per_L10
root=CREATE_schema_migrations_under_fnox_cos_app + billing_hold_018FD7
next=billing→JWKS→migrate→redeploy→readyz_200
agent=UNSUPPORTED_no_gcloud_billing
foundry=NOT_PROMOTED
```
