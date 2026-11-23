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
| R-03 | **ADMITTED** | **OPEN** (PARTIAL) | Native Quinn ALPN **`aoq/1`** pinned + court **5**/EXIT:**0** @ cgap `c525c7a4f` / [cgap#2](https://github.com/seanchatmangpt/cgap/pull/2). UDP-443 **BLOCKED** (PermissionDenied). WebTransport/`h3` edge still deferred. Exact next: privileged UDP-443 court ([R-03 ALPN aoq/1 court](bc-f4417b0a-1e35-559a-9ba5-2261573c1883)) | cgap / UI-plane |
| R-04 | **ADMITTED** | **OPEN** (PARTIAL) | Local court **ALIVE** @ ash_a2a `ff08fc06` / [bc-cd52b5d7](bc-cd52b5d7-1e20-5b70-93f2-faf3bb6deaec): WebCrypto channel-bind **2**/EXIT:**0** (+SESS **9**/EXIT:**0**). Draft PR **403** — exact next: Founder open draft PR + CI green ([`R04-WEBCYPTO-BIND-COURT-RELAUNCH-STANDING.md`](./R04-WEBCYPTO-BIND-COURT-RELAUNCH-STANDING.md)) | Founder HITL / ash_a2a owner |
| R-05 | **ADMITTED** | **OPEN** (PARTIAL) | Main `563eb32`: BLAKE3 `pqc_seal[248]` stand-in only. Stack tip `99412a7` ([affidavit#154](https://github.com/seanchatmangpt/affidavit/pull/154), base `cursor/pqc-context-string-seal-ffa4` @ `4de0dbb` — **behind main by 7**, no open parent PR): `aod_refusal_court` **16**/EXIT:**0** — **≠ main CLOSED**. `#154` vs main: **diverged** (ahead 37 / behind 4; MERGEABLE checks green). Exact next: Founder HITL retarget/rebase `#154` → `main`, merge, then `cargo test --test aod_refusal_court` → **0** on main | Affidavit / Founder HITL merge |
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

### R-03 (cgap native Quinn) — PARTIAL; closure **OPEN**

[R-03 ALPN aoq/1 court](bc-f4417b0a-1e35-559a-9ba5-2261573c1883) + independent reconfirm: ALPN risk-body
string locked on native Quinn; UDP-443 typed BLOCKED; WebTransport/`h3` not invented.
Do **not** invent CLOSED.

| Probe | Result |
|-------|--------|
| cgap tip | `c525c7a4f` / [cgap#2](https://github.com/seanchatmangpt/cgap/pull/2) — `AOQ_ALPN = b"aoq/1"` |
| `cargo test -p cgap-beam --features quic --test aoq_alpn_native_court` | **5**/EXIT:**0** (this lane) |
| UDP-443 | **BLOCKED** `PermissionDenied` (unprivileged) |
| WebTransport/`h3` edge | still deferred E9 — **OPEN** |
| Exact next | Privileged/`CAP_NET_BIND_SERVICE` UDP-443 listen+connect court → LOCKED |

### R-04 (FFB Auth / SessionActor) — PARTIAL; local court **ALIVE**; closure **OPEN**

Relaunch ([bc-cd52b5d7](bc-cd52b5d7-1e20-5b70-93f2-faf3bb6deaec)) after
[bc-9589ff95](bc-9589ff95-17ad-5fb8-b804-89725457e51e) `resource_exhausted` empty:
Chrome CDP + `ChannelBind`/`bind_channel`/`verify_reconnect` court **EXIT0** on
`cursor/r04-webcrypto-channel-bind-bc54` @ `ff08fc06`. Draft PR **403** — do **not**
invent body CLOSED without PR+CI.

| Probe | Result |
|-------|--------|
| Prior court agent | [bc-9589ff95](bc-9589ff95-17ad-5fb8-b804-89725457e51e) → **ERROR** `resource_exhausted` (empty) |
| Relaunch court | `r04_webcrypto_channel_bind_court_test.exs --only serial_solo` → **2**/EXIT:**0**; +SESS → **9**/EXIT:**0** |
| WebCrypto channel-bind | **ALIVE** (local ash_a2a court) — ≠ Foundry / zero-HITL |
| Draft PR | **ABSENT** (`gh pr create` **403**) |
| Exact next | Founder opens draft PR from `ff08fc06`; CI green; replay court |

### R-05 (affidavit) — PARTIAL; closure **OPEN** on main

Independent probe ([R-05 affidavit stride probe](bc-765cfa69-642a-51b1-9007-b361b7502805)): main tip
`563eb322a` = BLAKE3 `pqc_seal[248]` stand-in only. Stack tip `99412a7`
([affidavit#154](https://github.com/seanchatmangpt/affidavit/pull/154), base
`cursor/pqc-context-string-seal-ffa4` @ `4de0dbb` — **behind main by 7**, no open
parent PR; tip vs main **diverged** ahead 37 / behind 4; CI MERGEABLE green):
refusal + PQC seal courts green — still **≠** main CLOSED.

| Probe | Result |
|-------|--------|
| Main tip | `563eb32` — BLAKE3 stand-in; aod lib **15**/EXIT:**0** |
| Stack tip `#154` | `99412a7` — `aod_refusal_court` **16**/EXIT:**0**; `aod::pqc_seal` **6**/EXIT:**0** |
| Exact next | Founder HITL: retarget/rebase `#154` → `main`, merge; reconfirm both courts on main → **0** |

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
R-03=ADMITTED_OPEN_PARTIAL (cgap#2 aoq/1 court5 EXIT0; UDP-443 BLOCKED; WT/h3 OPEN)
R-04=ADMITTED_OPEN_PARTIAL (local court ALIVE ash_a2a ff08fc06 bc-cd52b5d7 2/EXIT0; draft PR 403 HITL; ≠CLOSED)
contract_fence=ALIVE
ungrounded_closed=REFUSED_UNGROUNDED
Foundry=not_ALIVE
```
