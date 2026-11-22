# OOR cos + revops live probe — push-first honesty (v27.1.1)

```text
tip_base=1935ed718f1407232cbe97cf1e08f79d9ce11dfa
side=cursor/v2711-oor-cos-revops-probe-bc54
agent=bc-2e00bab2-1b8c-577f-97d2-48cf9ccfa06d
utc=2026-11-22T18:26:51Z
pst=2026-11-22 10:26:51 PST
cargo=NOT_RUN
foundry=NOT_CLAIMED (never invent Foundry ALIVE)
```

| Field | Value |
|-------|-------|
| Document ID | `OOR-COS-REVOPS-PROBE-STANDING` |
| Companion | [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md); [`OUT-OF-REPO-NEXT-VERIFIERS.md`](./OUT-OF-REPO-NEXT-VERIFIERS.md); [`ECOSYSTEM-SPEC-27.1.1.md`](./ECOSYSTEM-SPEC-27.1.1.md) |
| Crown | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `1935ed718f1407232cbe97cf1e08f79d9ce11dfa` (`1935ed718`; ⊇ `7d1c24584`; last known prior pin `3d13c19e96b844cfb4d4cb9cdaa0f811c32833ed`) |
| Sidebranch | `cursor/v2711-oor-cos-revops-probe-bc54` |
| Agent | `bc-2e00bab2-1b8c-577f-97d2-48cf9ccfa06d` |
| Observed (UTC) | `2026-11-22T18:26:51Z` (reconfirm `2026-11-22T18:27:06Z`) |
| Observed (America/Los_Angeles) | `2026-11-22 10:26:51 PST` (reconfirm `2026-11-22 10:27:06 PST`) |
| Cargo | **NOT RUN** (docs-only probe honesty) |
| Scope | Plane 5 front-door probes only — **out of ggen DO**; no Foundry claim |

Standing vocabulary (AGENTS.md): `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `UNSUPPORTED` | `REFUSED_*`.

**Hard rule (held):** Do not invent success. HTTP reachability ≠ readiness ≠ product `ALIVE`. A `/readyz` **503** is **BLOCKED**. DNS absence is **UNKNOWN**. Never invent Foundry `ALIVE`. No ManagePullRequest. No force-push.

---

## 1. Probe results (live; this host)

Exact commands from the crown task (run for real):

```bash
# 2026-11-22 10:26:51 PST / 2026-11-22T18:26:51Z
curl -sS -m 15 -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz || true
getent hosts revops.fnox.io || true
curl -sS -m 15 -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz || true
```

| Probe | UTC | Exit | HTTP | Body / stderr snippet | Standing |
|-------|-----|------|------|------------------------|----------|
| `curl … https://cos.fnox.io/readyz` | `2026-11-22T18:26:51Z` | **0** | **503** | `{"status":"DB_UNAVAILABLE"}` | **BLOCKED** |
| `getent hosts revops.fnox.io` | `2026-11-22T18:27:06Z` | **2** | — | empty (no A/AAAA) | **UNKNOWN** |
| `curl … https://revops.fnox.io/readyz` | `2026-11-22T18:26:51Z` | **6** | **000** | `Could not resolve host: revops.fnox.io` | **UNKNOWN** |

Reconfirm same host (`2026-11-22T18:27:06Z` / `10:27:06 PST`): cos `/readyz` again **503** `{"status":"DB_UNAVAILABLE"}` (curl exit **0**); `getent hosts cos.fnox.io` → `34.8.148.142` (exit **0**, edge DNS only); revops `getent` exit **2** / curl exit **6** `http=000` held.

Crown claim for this slice: **Plane 5 cos readiness is BLOCKED; revops front door is UNKNOWN; neither subject is ALIVE.**

---

## 2. Standing summary

| Subject | Standing | Why |
|---------|----------|-----|
| `cos.fnox.io` `/readyz` | **BLOCKED** | Typed readiness refusal: HTTP **503** `DB_UNAVAILABLE` |
| `revops.fnox.io` DNS | **UNKNOWN** | `getent` empty, exit **2** |
| `revops.fnox.io` `/readyz` | **UNKNOWN** | curl exit **6**, `http=000`, host unresolved |
| Plane-5 product ALIVE (cos ∧ revops ready) | **not ALIVE** | ALIVE gate unmet (requires cos `/readyz` **200** ∧ revops `/readyz` **200**) |
| Foundry / zero-HITL | **not claimed** | Out of scope for this probe; **never invent Foundry ALIVE** |

Held vs prior OOR retip18 (`47aebeb75` / `REVOPS-COS-STANDING.md` §21): same shape — cos readiness **BLOCKED**, revops **UNKNOWN**. No standing promotion.

---

## 3. Exact next verifier

Run on an authorized operator host (owning plane). Paste exit codes + bodies into a successor receipt before any standing promotion.

```bash
# America/Los_Angeles label + UTC
TZ=America/Los_Angeles date '+%Y-%m-%d %H:%M:%S %Z'
date -u '+%Y-%m-%dT%H:%M:%SZ'

# Pass criterion for readiness ALIVE claim:
#   cos:  HTTP 200 + admitted ready body (not DB_UNAVAILABLE)
#   revops: getent non-empty AND HTTP 200 on /readyz
curl -sS -m 15 -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz || true
getent hosts revops.fnox.io || true
curl -sS -m 15 -w '\nhttp=%{http_code}\n' https://revops.fnox.io/readyz || true
```

Until those pass criteria hold: keep cos **BLOCKED**, revops **UNKNOWN**. Do not rewrite failures as success. Do not claim Foundry `ALIVE` from this ledger.

Replay (ggen docs-only):

```text
test -f docs/rfc/v27.1.1/OOR-COS-REVOPS-PROBE-STANDING.md
rg -n 'BLOCKED|UNKNOWN|ALIVE|DB_UNAVAILABLE|Could not resolve host' docs/rfc/v27.1.1/OOR-COS-REVOPS-PROBE-STANDING.md
```

---

## 4. Provenance

| Item | Value |
|------|-------|
| Materialization | verified local checkout + live HTTP/DNS from Cursor cloud agent |
| Sidebranch | `cursor/v2711-oor-cos-revops-probe-bc54` |
| Base SHA | `1935ed718f1407232cbe97cf1e08f79d9ce11dfa` |
| Prior retip lineage | retip18/`47aebeb75` → … → retip7 (see [`REVOPS-COS-STANDING.md`](./REVOPS-COS-STANDING.md)) |
| Absorbed | tip-wins onto crown via `cursor/v2711-oor-cos-revops-absorb-bc54`; side `d52dfee4b` / content `c16c21296`; no force |
| ManagePullRequest | **not used** |
| Force-push | **not used** |
