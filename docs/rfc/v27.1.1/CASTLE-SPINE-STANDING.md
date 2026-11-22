# CASTLE-SPINE-STANDING — horizontal spine member (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `CASTLE-SPINE-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` (Gate 2–3 CASTLE publish); `REPO-INVENTORY-27.1.1` |
| Subject | `seanchatmangpt/castle` on spine `ggen → Taffy → zcode-cli → CASTLE → Smalt → revops` |
| Materialized | 2026-11-22 |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `3ea41ffd0524f63d342c2f23879038b889e28884` (lineage through `a3b1c9410` / `1a32ea1b1`) |
| Tip pin | 2026-11-22 — `cursor/v2711-castle-retip2-bc54` honesty reconfirm vs tip `a3b1c9410+` (prior: `cursor/v2711-castle-alive-bc54` / foundry-honesty `4b22529df`; sibling courts **ALIVE** @ `2774039ea` / `bc-1bffb072` unchanged) |
| Observed castle tip | `main` @ `2774039ea81bad508c93785a59dd7a99198ae256` (https://github.com/seanchatmangpt/castle/commit/2774039ea81bad508c93785a59dd7a99198ae256; live courts `bc-1bffb072`; not mounted in this ggen workspace) |
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
| Sibling `seanchatmangpt/castle` mounted in this workspace | **BLOCKED** | Checkout unmounted here; courts ran on CASTLE live agent |
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
| Crown spine integration | **UNKNOWN** (no ggen digest bind) |
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

## 9. Honesty retip2 vs tip `a3b1c9410+` (2026-11-22)

| Field | Value |
|-------|-------|
| Branch | `cursor/v2711-castle-retip2-bc54` (no tip FF; no cargo; no ManagePullRequest) |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `3ea41ffd0524f63d342c2f23879038b889e28884` (ancestor `a3b1c94103984e2d362c068deb61cd700615e158`) |
| Stale pin repaired | Prior Observed tip `4b22529df` lagged tip `a3b1c9410+` — standings held; pin only |
| Sibling castle tip | still `2774039ea81bad508c93785a59dd7a99198ae256` on `main` ([commit](https://github.com/seanchatmangpt/castle/commit/2774039ea81bad508c93785a59dd7a99198ae256)) |
| Sibling courts | **ALIVE** — last recorded verifiers (`bc-1bffb072` @ `2774039ea`): `cargo test --test publish_redaction_court` → **13/13** exit **0**; `cargo test --test cli_publish_brand` → **6/6** exit **0**; `./target/debug/castle publish post --subject spine-probe --public-anonymize --brand brands/fourth-watch.brand.toml` → exit **0** / `redaction_court=ALIVE` |
| CASTLE on horizontal spine (crown wire) | **UNKNOWN** / digest-bind **PARTIAL** residual — sibling **ALIVE** ≠ ggen digest-bound spine crown |
| Zero-HITL / Foundry crown | **PARTIAL** — CASTLE sibling **ALIVE** does **not** imply crown zero-HITL **ALIVE** |
| Exact next verifier | Crown residual: bind admitted ggen layout/brand digest into CASTLE mint receipt, then in castle @ `2774039ea` replay §3.1 (`publish_redaction_court` → **0**; `cli_publish_brand` → **0**; spine-probe mint → **0** / `redaction_court=ALIVE`) with digest asserted on the receipt. LinkedIn AND-gate remains **BLOCKED** on cos `/readyz` **200** ∧ 2× durable LOW |
