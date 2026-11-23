# R-01 … R-07 resolution standing — ECOSYSTEM-SPEC-27.1.1 §11

| Field | Value |
|-------|-------|
| Document ID | `R01-R07-STANDING-27.1.1` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1.md` §11 (D7 hard GA gate) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` ⊇ body-admit land |
| Admitted register | [`fixtures/r01-r07-sovereign-risk-register.md`](./fixtures/r01-r07-sovereign-risk-register.md) (verbatim zcode-cli ecospec §4 @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a`) |
| Contract tip-cargo | **ALIVE** — `r01_r07_resolution_court` (ungrounded fence + admit mount) |
| Bodies | **ADMITTED** (all seven); closure **CLOSED** for **R-02** + **R-06**; **OPEN** for R-01/R-03…R-05/R-07 (R-05 aod surface PARTIAL ≠ CLOSED) |
| Target GA | **2027-01-01** |
| Repo scope | `seanchatmangpt/ggen` — bodies multi-repo; contract + mount in-repo |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

**Law:** Do **not** invent risk bodies. Source presence of this standing file is not
closure. “Closed” without an admitted risk-register body is `REFUSED_UNGROUNDED`.
Body **ADMITTED** ≠ body **CLOSED**.

**Exact next verifier (open bodies):**
Bodies admitted from fixtures/r01-r07-sovereign-risk-register.md
(zcode-cli docs/ecosystem-spec-27.1.1.md §4). Per open ID: execute that
row's Mandatory Resolution Contract → typed receipt or typed REFUSED_*
before GA 2027-01-01

---

## 1. Resolution contract (in-repo — falsifiable)

| Subject | Evidence | Standing | Next exact verifier |
|---------|----------|----------|---------------------|
| §11 enumeration R-01…R-07 | `ECOSYSTEM-SPEC-27.1.1.md` §11 | **PARTIAL_ALIVE** (spec text) | Keep IDs stable; refuse invented R-08+ via court |
| Closure rule: typed receipt **or** typed refusal before GA | Spec §11 | **PARTIAL_ALIVE** (doctrine) | Per-ID receipt/refusal after body admit |
| Ungrounded “closed” → `REFUSED_UNGROUNDED` | `ggen_graph::ecosystem_risk::resolve_closure` + Chicago court | **ALIVE** (fence) | `CARGO_BUILD_JOBS=1 cargo test -p ggen-graph --test r01_r07_resolution_court --locked -- --test-threads=1` |
| Risk-register prose | Admitted mount fixture | **ADMITTED** | See §2 |

```text
cargo test -p ggen-graph --test r01_r07_resolution_court
```

---

## 2. Crown matrix (R-01 … R-07 bodies)

| ID | Body standing | Closure | Next exact verifier (through GA 2027-01-01) | Owner / HITL |
|----|---------------|---------|-----------------------------------------------|--------------|
| R-01 | **ADMITTED** | **OPEN** | cos 6 ledger fixes + eliminate `--seed 7` race + 2× LOW-only skeptic waves ([`COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md`](./COS-DB-UNAVAILABLE-NEXT-VERIFIER-STANDING.md) AND-gate first) | Founder HITL / fnox cos |
| R-02 | **ADMITTED** | **CLOSED** | repop `24a5041e1` / [repop#1](https://github.com/seanchatmangpt/repop/pull/1): court **10**/EXIT:**0** + soak **1**/EXIT:**0** (250k×128 B through `allocFixedWorkerArena`; peak ≤ 32 MiB; grow refused). Honesty: synthetic row pressure (no product FFB ingest on disk). ≠ Foundry ALIVE | sealed (repop) |
| R-03 | **ADMITTED** | **OPEN** | repop tip `92ea8af68`: WebTransport/`h3`/`aoq/1`/UDP-443 **ABSENT** (deferred E9 prose only). Nearby: cgap `aq/1` ≠ `aoq/1`; smalt docs ≠ wire. Exact next: pin ALPN + Chicago UDP-443 court + drop WebTransport/`h3` dependency ([R-03 WebTransport probe](bc-b77c4702-1449-546a-b6ca-3ba0e99edcc3)) | UI-plane / FFB / cgap owner |
| R-04 | **ADMITTED** | **OPEN** (PARTIAL) | SessionActor **PRESENT** (`ash_a2a` `:gen_statem`); WebCrypto `extractable:false` / IndexedDB / channel-bind **ABSENT** in repop/FFB. Exact next: chromium court bind→verify→zero-cookie reconnect ([R-04 WebCrypto probe](bc-28d585dc-9cf5-56d2-a1f2-92c1d5724d22)) | UI-plane / FFB / ash_a2a owner |
| R-05 | **ADMITTED** | **OPEN** (PARTIAL) | Main `563eb32`: BLAKE3 `pqc_seal[248]` stand-in only. Stack tip `99412a7` ([affidavit#154](https://github.com/seanchatmangpt/affidavit/pull/154)): `aod_refusal_court` **16**/EXIT:**0** (includes `e50c_pqc_seal` + `e509` stride + `e50d` lease) — **≠ main CLOSED**. Exact next: merge `#154` stack → `main`, then reconfirm `cargo test --test aod_refusal_court` → **0** on main | Affidavit / Founder HITL merge |
| R-06 | **ADMITTED** | **CLOSED** | Evidence: zcode commits `0cff19a`/`7b2c181` **PRESENT**; `bun test test/powl-ocel-chicago.test.ts` → **11**/EXIT:**0** @ `f32df08`; `DEFAULT_MAX_REMEDIATION_ATTEMPTS=3` | sealed (zcode-cli) |
| R-07 | **ADMITTED** | **OPEN** | Clear GCP billing hold `018FD7-382078-0D52EF`; stand up Stripe Billing + Mercury/Brex | Founder HITL / billing |

Do **not** promote open rows to **CLOSED** without typed receipt. Silent “closed”
without admitted body → `REFUSED_UNGROUNDED`.

---

## 3. CLOSED / PARTIAL receipts (evidence)

### R-02 (repop) — **CLOSED**

Branch `cursor/v2711-r02-fixed-memory-bc54` tip `24a5041e1` ([repop#1](https://github.com/seanchatmangpt/repop/pull/1)):
fixed `initial === maximum` + 250k-row-equivalent soak under 32 MiB.

| Probe | Result |
|-------|--------|
| `pnpm --filter @repop/worker exec vitest run src/ring/fixedSharedMemory.test.ts` | **10**/EXIT:**0** |
| `pnpm --filter @repop/worker exec vitest run src/ring/fixedSharedMemory.soak.test.ts` | **1**/EXIT:**0** (independent reconfirm this lane) |
| Load honesty | Synthetic 250_000×128 B through real arena; product FFB ingest **ABSENT** on disk — still closes R-02 arena/OOM risk |
| ≠ Foundry / zero-HITL / path crown ALIVE | held |

### R-03 (repop / cgap / smalt) — **OPEN**

Independent probe ([R-03 WebTransport probe](bc-b77c4702-1449-546a-b6ca-3ba0e99edcc3)): risk surface
(WebTransport `h3` edge-proxy → native `ALPN: aoq/1` + UDP-443) **ABSENT** in
repop — deferred E9 prose only; no court. Do **not** invent CLOSED.

| Probe | Result |
|-------|--------|
| repop tip | `92ea8af68` — zero `ALPN`/`aoq/1`/`UDP-443`/`edge-proxy` hits; uplink = Null/PostMessage/Ring only |
| cgap (nearby) | `aq/1` + ephemeral UDP — **≠** `aoq/1` / UDP-443 / WebTransport |
| smalt (nearby) | docs claim `aoq/1`; wraps cgap — docs ≠ wire |
| Exact next | Pin ALPN id → Chicago UDP-443 court (mutate-ALPN refuse) → assert no WebTransport/`h3` edge path |

### R-04 (FFB Auth / SessionActor) — PARTIAL; closure **OPEN**

Independent probe ([R-04 WebCrypto probe](bc-28d585dc-9cf5-56d2-a1f2-92c1d5724d22)): BEAM SessionActor
exists; non-extractable WebCrypto channel-bind and the bind itself are **ABSENT**.
Do **not** invent CLOSED from SESS courts alone.

| Probe | Result |
|-------|--------|
| `AshA2A.Session.Statem` | **PRESENT** — `:gen_statem` court (SESS) ≠ R-04 bind |
| WebCrypto / IndexedDB / `extractable:false` / channel-bind | **ABSENT** (repop/zcode/smalt/fnox SIGN≠WebCrypto) |
| Exact next | Chromium: ECDSA P-256 non-extractable → IDB → refuse export → SessionActor verifies Wi-Fi↔cellular reconnect (zero cookie/token) EXIT **0** |

### R-05 (affidavit) — PARTIAL; closure **OPEN** on main

Independent probe ([R-05 affidavit stride probe](bc-765cfa69-642a-51b1-9007-b361b7502805)): main tip
`563eb322a` = BLAKE3 `pqc_seal[248]` stand-in only. Stack tip `99412a7`
([affidavit#154](https://github.com/seanchatmangpt/affidavit/pull/154), base not yet
`main`): refusal + PQC seal courts green — still **≠** main CLOSED.

| Probe | Result |
|-------|--------|
| Main tip | `563eb32` — BLAKE3 stand-in; aod lib **15**/EXIT:**0** |
| Stack tip `#154` | `99412a7` — `aod_refusal_court` **16**/EXIT:**0**; `aod::pqc_seal` **6**/EXIT:**0** |
| Exact next | Merge `#154` stack → `main`; reconfirm both courts on main → **0** |

### R-06 (zcode-cli) — **CLOSED**

| Probe | Result |
|-------|--------|
| `git cat-file -t 0cff19a` / `7b2c181` | `commit` / `commit` |
| `bun test test/powl-ocel-chicago.test.ts` @ `f32df08` | **11 pass / EXIT 0** |
| Cap | `DEFAULT_MAX_REMEDIATION_ATTEMPTS = 3` |

**≠** Foundry `--live` ALIVE; **≠** zero-HITL ALIVE; **≠** path crown ALIVE.

---

## 4. Compact return

```text
bodies=ADMITTED@fixtures/r01-r07-sovereign-risk-register.md
provenance=zcode-cli@f32df08 §4
R-05=ADMITTED_OPEN (main PARTIAL; #154 tip 99412a7 refusal16+pqc6 EXIT0 ≠ main CLOSED)
R-06=CLOSED (0cff19a+7b2c181; chicago 11/EXIT0)
R-01+R-07=ADMITTED_OPEN
R-02=CLOSED (repop@24a5041e1 court10+soak1 EXIT0; synthetic≠product-FFB; ≠Foundry)
R-03=ADMITTED_OPEN (repop ABSENT; cgap aq/1≠aoq/1; no court)
R-04=ADMITTED_OPEN_PARTIAL (SessionActor PRESENT; WebCrypto bind ABSENT)
contract_fence=ALIVE
ungrounded_closed=REFUSED_UNGROUNDED
Foundry=not_ALIVE
```
