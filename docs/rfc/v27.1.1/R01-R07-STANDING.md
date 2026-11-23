# R-01 … R-07 resolution standing — ECOSYSTEM-SPEC-27.1.1 §11

| Field | Value |
|-------|-------|
| Document ID | `R01-R07-STANDING-27.1.1` |
| Spec binding | `ECOSYSTEM-SPEC-27.1.1.md` §11 (D7 hard GA gate) |
| Observed tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` ⊇ body-admit land |
| Admitted register | [`fixtures/r01-r07-sovereign-risk-register.md`](./fixtures/r01-r07-sovereign-risk-register.md) (verbatim zcode-cli ecospec §4 @ `f32df08cdc4b3b8e114a8d1de8cf81acb80e1f3a`) |
| Contract tip-cargo | **ALIVE** — `r01_r07_resolution_court` (ungrounded fence + admit mount) |
| Bodies | **ADMITTED** (all seven); closure **CLOSED** for **R-06** only; **OPEN** for R-01…R-05, R-07 (R-05 aod surface PARTIAL ≠ CLOSED) |
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
| R-02 | **ADMITTED** | **OPEN** | Lock FFB `SharedArrayBuffer` `initial === maximum` before 250k-row mobile OOM | UI-plane / FFB owner |
| R-03 | **ADMITTED** | **OPEN** | Align WebTransport `h3` edge-proxy → native `ALPN: aoq/1` + UDP-443 | UI-plane / FFB owner |
| R-04 | **ADMITTED** | **OPEN** | Non-extractable WebCrypto channel-bind ↔ `:gen_statem` SessionActor | UI-plane / FFB owner |
| R-05 | **ADMITTED** | **OPEN** (PARTIAL) | Main `563eb32`: BLAKE3 `pqc_seal[248]` stand-in only. Stack tip `99412a7` ([affidavit#154](https://github.com/seanchatmangpt/affidavit/pull/154)): `aod_refusal_court` **16**/EXIT:**0** (includes `e50c_pqc_seal` + `e509` stride + `e50d` lease) — **≠ main CLOSED**. Exact next: merge `#154` stack → `main`, then reconfirm `cargo test --test aod_refusal_court` → **0** on main | Affidavit / Founder HITL merge |
| R-06 | **ADMITTED** | **CLOSED** | Evidence: zcode commits `0cff19a`/`7b2c181` **PRESENT**; `bun test test/powl-ocel-chicago.test.ts` → **11**/EXIT:**0** @ `f32df08`; `DEFAULT_MAX_REMEDIATION_ATTEMPTS=3` | sealed (zcode-cli) |
| R-07 | **ADMITTED** | **OPEN** | Clear GCP billing hold `018FD7-382078-0D52EF`; stand up Stripe Billing + Mercury/Brex | Founder HITL / billing |

Do **not** promote open rows to **CLOSED** without typed receipt. Silent “closed”
without admitted body → `REFUSED_UNGROUNDED`.

---

## 3. CLOSED / PARTIAL receipts (evidence)

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
R-01..R-04+R-07=ADMITTED_OPEN
R-02=OPEN (repop: no initial===maximum court)
contract_fence=ALIVE
ungrounded_closed=REFUSED_UNGROUNDED
Foundry=not_ALIVE
```
