# CASTLE-SPINE-STANDING — horizontal spine member (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `CASTLE-SPINE-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` (Gate 2–3 CASTLE publish); `REPO-INVENTORY-27.1.1` |
| Subject | `seanchatmangpt/castle` on spine `ggen → Taffy → zcode-cli → CASTLE → Smalt → revops` |
| Materialized | 2026-11-22 |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `6a3a623bacd33efdc1ec7efccf6a35141662ca53` (ui_layout exact tip-cargo ALIVE land; see §11 wire probe — supersedes §10 pin `1f335ea82`) |
| Tip pin | 2026-11-22 — `cursor/v2711-castle-wire-probe-bc54` from crown `6a3a623ba` (foundry clause-4 wire/digest-bind probe; **no** push to crown; sibling `main` reconfirmed `2774039ea`; digest-bind admit surface **ABSENT**; crown wire **UNKNOWN**; LinkedIn **BLOCKED**; no cargo; no ManagePullRequest) |
| Observed castle tip | `main` @ `2774039ea81bad508c93785a59dd7a99198ae256` (https://github.com/seanchatmangpt/castle/commit/2774039ea81bad508c93785a59dd7a99198ae256; live courts `bc-1bffb072`; remote **REACHABLE** this probe via `git ls-remote` + shallow clone @ `/tmp/castle-wire-probe/castle`; **not** mounted in the ggen workspace tree) |
| Repo scope of this land | `seanchatmangpt/ggen` (standing ledger only — no CASTLE runtime / no tip-cargo this retip) |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

Source presence, README prose, and remote tip SHA are **never** promoted to `ALIVE`. Sibling court exits recorded by CASTLE live `bc-1bffb072` are the authority for the rows below marked **ALIVE**.

---

## 1. Role (horizontal spine)

```text
ggen (μ / layout+brand CONSTRUCT)
  → Taffy AOT pre-flight (geometry law)
  → zcode-cli ui-layout-court (consumes AotSealReceipt)
  → CASTLE (CONSEQUENTIAL_ADMISSIBILITY + public publish mint)
  → Smalt (BEAM edge substrate)
  → revops / Plane-5 front doors (DO only via BRCE+WebAuthn AS-01)
```

CASTLE’s irreducible capability (sibling sJira `repos.ttl`): **`CONSEQUENTIAL_ADMISSIBILITY`**.

On this spine CASTLE is the **consequence / publish plane**:

| Surface | Authority ceiling | Meaning |
|---------|-------------------|---------|
| Board strategic command | `CONSTRUCT` | Mandate packets; no optimizer→DO edge |
| `castle publish` (Typst/JSON mint) | `CONSTRUCT` | Public mint behind `--public-anonymize` + explicit `--brand` |
| RedactionCourt | refuse-only | Private residue → `REFUSED_REDACTION`; never strip-and-ship |
| LinkedIn / Jobs outbound send | stop-worked | Unlock only after AND-gate (cos `/readyz` 200 ∧ 2× durable LOW) |
| Consequential DO | BRCE only | Opaque `ConstructAdmission` → GymAct; SELECT≠CONSTRUCT≠DO |

CASTLE consumes **brand manifests** (e.g. `brands/fourth-watch.brand.toml`) that ggen Gate-1 rhythm stubs foreshadow; full color/type/CASTLE token inventories remain Gate 2+ and must not be invented in ggen.

Intake host of record for CASTLE publish honesty: **`cos.fnox.io`**. Do not invent `revops.fnox.io` as the CASTLE publish host (revops remains Plane-5 / sibling; distinct edge).

---

## 2. Standing (observed 2026-11-22)

| Claim | Standing | Evidence |
|-------|----------|----------|
| CASTLE named on ECOSYSTEM Gate 2–3 (“CASTLE publish”) | doctrine present (companion / this ledger) | ECOSYSTEM-SPEC Gate schedule |
| In-ggen publish / court surface | **UNSUPPORTED** | No `castle publish` crate path; inventory: “no publish/court surface in-repo” |
| In-ggen CASTLE / brand-token inventory | **UNKNOWN** → Gate 2+ | Rhythm fixtures only; inventing CASTLE tokens → **REFUSED** |
| Sibling `seanchatmangpt/castle` mounted in this workspace | **BLOCKED** (remote **REACHABLE**) | ggen workspace unmounted; §11 probe: `ls-remote`+shallow clone @ `/tmp` → `2774039ea`; courts historically on CASTLE live agent `bc-1bffb072` |
| Sibling Typst/JSON mint (`spine-probe`) | **ALIVE** | `bc-1bffb072` @ `2774039ea`: `castle publish post --subject spine-probe --public-anonymize --brand brands/fourth-watch.brand.toml` → exit **0**, `redaction_court=ALIVE` |
| Sibling RedactionCourt / brand CLI courts | **ALIVE** | `bc-1bffb072` @ `2774039ea`: `cargo test --test publish_redaction_court` → **13/13** exit **0**; `cargo test --test cli_publish_brand` → **6/6** exit **0** |
| LinkedIn / blast send on Jobs AND-gate | **BLOCKED** / stop-worked (per castle publish README) | `/readyz` ∧ 2× LOW not observed |
| Spine wire: ggen brand/layout receipt → CASTLE `--brand` admit → Smalt → revops DO | **UNKNOWN** | No cross-repo digest-bind receipt |
| **CASTLE on horizontal spine (crown)** | **UNKNOWN** | Sibling courts **ALIVE** ≠ spine crown; no ggen layout/brand digest bind into mint |

Do **not** collapse sibling court **ALIVE** into Foundry-finish / zero-HITL / spine-crown `ALIVE`. Do **not** claim Plane-5 / revops / cos DO `ALIVE` from this repository.

---

## 3. Verifiers

### 3.1 Sibling spine courts (ALIVE — replay)

Exact commands that exited **0,0,0** on CASTLE tip `2774039ea81bad508c93785a59dd7a99198ae256` (`bc-1bffb072`):

```bash
cd /path/to/castle   # tip 2774039ea
cargo test --test publish_redaction_court
# → 13 passed; 0 failed; exit 0
cargo test --test cli_publish_brand
# → 6 passed; 0 failed; exit 0
./target/debug/castle publish post \
  --subject spine-probe \
  --public-anonymize \
  --brand brands/fourth-watch.brand.toml
# → exit 0; redaction_court=ALIVE on mint receipt
# omit --public-anonymize → REFUSED_NO_PUBLIC_ANONYMIZE / REFUSED_NO_AUTHORITY (exit 2)
# omit --brand → REFUSED_BRAND (exit 2)
```

Promotion applied: sibling mint + RedactionCourt/brand CLI courts → **ALIVE** for subject `spine-probe` only. Spine crown stays non-`ALIVE` until a receipt binds a ggen layout/brand digest into that mint. Zero-HITL / Foundry crown stay non-`ALIVE`.

### 3.2 Next falsifier (spine crown / digest bind)

Bind an admitted ggen layout/brand digest into a CASTLE mint receipt, then replay §3.1 with that bind asserted on the receipt. Until then crown remains **UNKNOWN**.

### 3.3 In-ggen (non-substitutes)

| Check | Command / probe | What it proves |
|-------|-----------------|----------------|
| No ambient CASTLE publish in ggen | `rg -n 'castle publish|RedactionCourt' crates packs --glob '!ontology_catalogue/**'` | Absence → **UNSUPPORTED** remains honest |
| Brand rhythm only (Gate 1) | `cargo test -p ggen-graph --test brand_toml_rhythm_court` (when present on tip) | Rhythm schema — **not** CASTLE inventory |

---

## 4. GA falsifier (2027-01-01)

GA claims that **CASTLE is `ALIVE` on the horizontal spine** are **REFUSED** / falsified when any of the following hold:

1. **No RedactionCourt admit** — public mint without `--public-anonymize` + explicit `--brand`, or any strip-and-ship redact path.
2. **Ungrounded inventory** — ggen (or pack) ships CASTLE color/type/token inventories without admitted sources (ECOSYSTEM non-goal).
3. **Send without AND-gate** — LinkedIn / Jobs outbound claimed `ALIVE` without cos `/readyz` **200** **and** 2× durable LOW receipts on disk.
4. **Host fabrication** — CASTLE publish host asserted as `revops.fnox.io` (honest host remains `cos.fnox.io`).
5. **Ambient DO** — CASTLE CONSTRUCT / planner / model output treated as Plane-5 DO without BRCE + WebAuthn AS-01.
6. **Prose promotion** — spine `ALIVE` asserted from this standing ledger, fleet census, or castle README alone without §3 verifier exit 0 on the admitted subjects.
7. **Sibling≠crown** — claiming horizontal-spine crown `ALIVE` from §3.1 sibling court exits alone (no ggen digest bind).

---

## 5. Search corroboration (this land)

| Source | Finding |
|--------|---------|
| `rg CASTLE` in ggen live crates/docs | Gate-2+ inventory notes; how-to marketplace `castle-pack` pin; no publish court |
| `seanchatmangpt/castle` README + `src/publish/README.md` | Role = consequential admissibility + gated public mint |
| castle `docs/sjira/v26.9.28/repos.ttl` | Owns `CONSEQUENTIAL_ADMISSIBILITY`; zcode-cli WRAP as `EXTERNAL_WORKER_PROVIDER` |
| ECOSYSTEM-SPEC-27.1.1 (companion PR) | CASTLE publish = Gate 2–3; inventing inventories REFUSED |
| CASTLE live `bc-1bffb072` @ `2774039ea` | `publish_redaction_court` 13/13; `cli_publish_brand` 6/6; spine-probe mint `redaction_court=ALIVE`; exits 0,0,0 |
| Smalt / revops / zcode-cli | Sibling repos named; spine edges beyond CASTLE courts remain **UNKNOWN** / **BLOCKED** |

---

## 6. Standing receipt (this land)

| Claim | Standing |
|-------|----------|
| Role of CASTLE on horizontal spine | Documented (§1) |
| Crown spine integration | **UNKNOWN** (no ggen digest bind; §11: bind admit surface **ABSENT** on castle publish @ `2774039ea`) |
| In-ggen CASTLE publish surface | **UNSUPPORTED** |
| Sibling mint / redaction / brand CLI courts | **ALIVE** @ `2774039ea` (`bc-1bffb072`; §3.1) |
| LinkedIn blast send | **BLOCKED** (AND-gate) |
| Zero-HITL / Foundry crown | **not ALIVE** — Foundry stays **PARTIAL_ALIVE** |
| GA readiness for CASTLE spine | **OPEN** — falsifiers §4 |

---

## 7. Tip reconfirm (2026-11-22)

| Field | Value |
|-------|-------|
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `4b22529df2939412c41107fe5e9eea66ba6a4dad` (superseded by §9) |
| Sibling `seanchatmangpt/castle` tip | `2774039ea81bad508c93785a59dd7a99198ae256` — §3.1 courts **ALIVE** via `bc-1bffb072` (not mounted in this workspace) |
| In-ggen `rg 'castle publish\|RedactionCourt' crates packs` | empty → publish surface **UNSUPPORTED** remains honest |
| Brand rhythm (non-substitute) | `brand_toml_rhythm_court` evidence `0478fde59` **4 passed; exit 0** — rhythm only, not CASTLE inventory |
| Crown spine standing | **UNKNOWN** — **unchanged** (sibling ALIVE ≠ crown) |
| False Foundry / zero-HITL `ALIVE` | **none** |

---

## 8. Provenance

GOAL SLICE: CASTLE in horizontal spine (`ggen+Taffy+zcode-cli+CASTLE+Smalt+revops`) standing.
Materialized as a ggen RFC ledger. Sibling spine courts promoted **ALIVE** from CASTLE live `bc-1bffb072` tip `2774039ea` (exits 0,0,0; publish_redaction_court 13/13; cli_publish_brand 6/6; spine-probe `redaction_court=ALIVE`). Tip pin on `cursor/v2711-castle-alive-bc54` against ggen tip `d0e3ba473`. Spine crown / zero-HITL / Foundry finish **not** promoted to `ALIVE`.

Honesty retip 2026-11-22 (`cursor/v2711-foundry-honesty-bc54`) → ggen tip `4b22529df2939412c41107fe5e9eea66ba6a4dad`; sibling courts **ALIVE** @ `2774039ea` unchanged; spine crown **UNKNOWN**/row **PARTIAL**; no Foundry crown promotion.

---

## 9. Honesty retip2 vs tip `a3b1c9410+` (2026-11-22) — absorbed @ `60b15c778`

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-castle-retip2-bc54` @ `371971c3a` absorbed onto tip (no cargo; tip-cargo spared; no ManagePullRequest) |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `60b15c7788ca7fb0c73ac19e7762759b61852603` (ancestors `c832cabdc` / `3ea41ffd0` / `a3b1c94103984e2d362c068deb61cd700615e158`) |
| Stale pin repaired | Prior Observed tip `4b22529df` lagged tip `a3b1c9410+`; absorb retips Observed to `60b15c778` — standings held; pin only |
| Sibling castle tip | still `2774039ea81bad508c93785a59dd7a99198ae256` on `main` ([commit](https://github.com/seanchatmangpt/castle/commit/2774039ea81bad508c93785a59dd7a99198ae256)) |
| Sibling courts | **ALIVE** — last recorded verifiers (`bc-1bffb072` @ `2774039ea`): `cargo test --test publish_redaction_court` → **13/13** exit **0**; `cargo test --test cli_publish_brand` → **6/6** exit **0**; `./target/debug/castle publish post --subject spine-probe --public-anonymize --brand brands/fourth-watch.brand.toml` → exit **0** / `redaction_court=ALIVE` |
| CASTLE on horizontal spine (crown wire) | **UNKNOWN** / digest-bind **PARTIAL** residual — sibling **ALIVE** ≠ ggen digest-bound spine crown |
| Zero-HITL / Foundry crown | **PARTIAL** — CASTLE sibling **ALIVE** does **not** imply crown zero-HITL **ALIVE** |
| Exact next verifier | Crown residual: bind admitted ggen layout/brand digest into CASTLE mint receipt, then in castle @ `2774039ea` replay §3.1 (`publish_redaction_court` → **0**; `cli_publish_brand` → **0**; spine-probe mint → **0** / `redaction_court=ALIVE`) with digest asserted on the receipt. LinkedIn AND-gate remains **BLOCKED** on cos `/readyz` **200** ∧ 2× durable LOW |

---

## 10. Foundry-spine honesty tip-pin vs tip `1f335ea82` (2026-11-22)

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-castle-spine-honesty-f95e` (no cargo; ui_layout exact-tip cargo under `/tmp/ggen-uilayout-exact-tip` spared; no ManagePullRequest) |
| Tip before (doc pin) | Observed ggen tip lagged at `60b15c7788ca7fb0c73ac19e7762759b61852603` (§9 absorb) |
| Tip after (live crown) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `1f335ea82cdd771215a9ede8b13393d847ed6b29` (ancestors ⊇ `fa2c782de` Smalt RAID tip-pin / `e73bf686c` / `77eb46fd0` / `60b15c778`) |
| Stale pin repaired | Header Observed advanced `60b15c778` → `1f335ea82` — **standings held; pin only** |
| Sibling castle tip | still `main` @ `2774039ea81bad508c93785a59dd7a99198ae256` (HEAD reconfirmed identical) |
| Sibling courts | **ALIVE** — last recorded verifiers (`bc-1bffb072` @ `2774039ea`): `cargo test --test publish_redaction_court` → **13/13** exit **0**; `cargo test --test cli_publish_brand` → **6/6** exit **0**; `./target/debug/castle publish post --subject spine-probe --public-anonymize --brand brands/fourth-watch.brand.toml` → exit **0** / `redaction_court=ALIVE` |
| In-ggen publish surface | **UNSUPPORTED** — `git grep 'castle publish\|RedactionCourt' crates packs` empty on tip `1f335ea82` |
| CASTLE on horizontal spine (crown wire) | **UNKNOWN** / digest-bind residual — sibling **ALIVE** ≠ ggen digest-bound spine crown; **not** Foundry / zero-HITL `ALIVE` |
| Foundry / zero-HITL crown | **PARTIAL** — CASTLE sibling **ALIVE** does **not** promote Foundry-finish crown |
| LinkedIn / Jobs AND-gate | **BLOCKED** — cos `/readyz` **200** ∧ 2× durable LOW not observed this retip |
| Exact next verifier | Crown residual (not fully **ALIVE**): bind admitted ggen layout/brand digest into CASTLE mint receipt, then in castle @ `2774039ea` replay §3.1 (`publish_redaction_court` → **0**; `cli_publish_brand` → **0**; spine-probe mint → **0** / `redaction_court=ALIVE`) with digest asserted on the receipt. LinkedIn AND-gate remains **BLOCKED** on `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → HTTP **200** ∧ 2× durable LOW |

---

## 11. Crown-wire / digest-bind probe vs tip `6a3a623ba` (2026-11-22)

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-castle-wire-probe-bc54` @ crown base `6a3a623bacd33efdc1ec7efccf6a35141662ca53` (**side branch only** — does **not** race OOR/crown tip writer; no ManagePullRequest) |
| Probe intent | Foundry clause 4 — inspect CASTLE crown-wire / §3.1 digest-bind path without cargo thrash |
| Observed ggen tip (read-only baseline) | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `6a3a623bacd33efdc1ec7efccf6a35141662ca53` (`docs(rfc): tip-wins ui_layout exact tip-cargo ALIVE @ a508e6bae`) |
| Sibling `seanchatmangpt/castle` | **REACHABLE** — `git ls-remote` HEAD = `2774039ea81bad508c93785a59dd7a99198ae256`; shallow clone HEAD identical; GitHub `main` commit API identical |
| Workspace mount | **BLOCKED** for ggen checkout — sibling not a workspace member; probe materialization was `/tmp/castle-wire-probe/castle` only |
| In-ggen publish / RedactionCourt | **UNSUPPORTED** — `git grep 'castle publish\|RedactionCourt' -- crates packs` empty on tip `6a3a623ba` |
| In-ggen layout digest (non-substitute) | **PRESENT** / tip-cargo **ALIVE** — `AotSealReceipt` + `ui_layout` consume fence @ `a508e6bae`; rhythm brand fixtures under `docs/rfc/v27.1.1/fixtures/*.brand.toml` — **not** a CASTLE mint bind |
| Castle publish wire (sibling @ `2774039ea`) | **ALIVE** for `--brand` path + `--public-anonymize` + artifact blake3 `digest` + `redaction_court` (historical §3.1 / `bc-1bffb072`) |
| ggen layout/brand **digest-bind** admit into mint | **ABSENT** / **UNSUPPORTED** — no CLI flag, receipt field, or brand.toml key that asserts a ggen `AotSealReceipt` / layout / brand digest on the mint; brand path admit ≠ digest bind; castle twin omits `extensions.ggen.*` |
| Crown can bind digest today? | **No** — bind surface missing ⇒ spine crown wire remains **UNKNOWN** (do **not** promote sibling mint **ALIVE** to crown) |
| Sibling courts re-run this probe | **not run** (honesty; no cargo thrash) — last recorded §3.1 exits **0,0,0** @ `2774039ea` / `bc-1bffb072` retained |
| LinkedIn / Jobs AND-gate | **BLOCKED** — this probe: `curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz` → `{"status":"DB_UNAVAILABLE"}` **http=503**; 2× durable LOW not observed |
| Spine wire / crown CASTLE | **UNKNOWN** (unchanged) |
| Foundry / zero-HITL | **PARTIAL** — this probe does **not** promote |

### 11.1 Standing delta (honesty)

| Claim | Before (§10) | After (§11) |
|-------|--------------|-------------|
| Observed ggen tip pin | `1f335ea82` | `6a3a623ba` |
| Sibling castle SHA | `2774039ea` | `2774039ea` (reconfirmed REACHABLE) |
| Sibling mint/redaction/brand courts | **ALIVE** | **ALIVE** (historical; not re-cargoed) |
| Crown wire / digest bind | **UNKNOWN** | **UNKNOWN** + bind surface typed **ABSENT**/**UNSUPPORTED** on castle publish @ `2774039ea` |
| LinkedIn send | **BLOCKED** | **BLOCKED** (cos **503** reconfirmed) |
| In-ggen publish | **UNSUPPORTED** | **UNSUPPORTED** (reconfirmed empty grep) |

### 11.2 Exact next verifier (crown absorb)

Do **not** treat this side-branch ledger as crown absorb. Parent stacks when ready. Exact next for crown residual:

```text
# 1) Castle (or ggen→castle bridge) must grow a digest-bind admit:
#    mint receipt asserts admitted ggen layout/brand digest (AotSealReceipt.digest
#    or brand rhythm digest) — today ABSENT at castle@2774039ea publish wire.
# 2) Then replay sibling courts at castle tip 2774039ea81bad508c93785a59dd7a99198ae256:
cd /path/to/castle   # tip 2774039ea
cargo test --test publish_redaction_court   # expect 13/13 exit 0
cargo test --test cli_publish_brand         # expect 6/6 exit 0
./target/debug/castle publish post \
  --subject spine-probe \
  --public-anonymize \
  --brand brands/fourth-watch.brand.toml
# expect exit 0; redaction_court=ALIVE; PLUS ggen digest asserted on receipt
# 3) LinkedIn remains BLOCKED until:
curl -sS -w '\nhttp=%{http_code}\n' https://cos.fnox.io/readyz   # expect HTTP 200
# ∧ 2× durable LOW receipts on disk
```

Until step 1+2 exit with digest asserted, **CASTLE on horizontal spine (crown)** stays **UNKNOWN**.
