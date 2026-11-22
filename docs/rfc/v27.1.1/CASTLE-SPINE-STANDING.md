# CASTLE-SPINE-STANDING — horizontal spine member (v27.1.1)

| Field | Value |
|-------|-------|
| Document ID | `CASTLE-SPINE-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` (Gate 2–3 CASTLE publish); `REPO-INVENTORY-27.1.1` |
| Subject | `seanchatmangpt/castle` on spine `ggen → Taffy → zcode-cli → CASTLE → Smalt → revops` |
| Materialized | 2026-11-22 |
| Observed ggen tip | `origin/main` @ `d173cad5792e837bd4fec730d120c582b9bc5d4c` |
| Observed castle tip | `main` @ `2774039ea81bad508c93785a59dd7a99198ae256` (API; sibling not mounted) |
| Repo scope of this land | `seanchatmangpt/ggen` (standing ledger only — no CASTLE runtime) |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

Source presence, README prose, and remote tip SHA are **never** promoted to `ALIVE`.

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
| Sibling `seanchatmangpt/castle` mounted in this workspace | **BLOCKED** | Checkout unmounted |
| Sibling Typst/JSON mint (castle README) | **UNKNOWN** from ggen (not executed here) | Remote docs claim mint ALIVE; no local court exit |
| Sibling RedactionCourt / brand CLI courts | **UNKNOWN** from ggen | Next verifier §3 |
| LinkedIn / blast send on Jobs AND-gate | **BLOCKED** / stop-worked (per castle publish README) | `/readyz` ∧ 2× LOW not observed |
| Spine wire: ggen brand/layout receipt → CASTLE `--brand` admit → Smalt → revops DO | **UNKNOWN** | No cross-repo court executed |
| **CASTLE on horizontal spine (crown)** | **UNKNOWN** | Sibling unmounted; no spine integration receipt |

Do **not** collapse sibling README “Typst mint ALIVE” into ggen `ALIVE`. Do **not** claim Plane-5 / revops / cos DO `ALIVE` from this repository.

---

## 3. Next verifier

### 3.1 Spine-facing (preferred next falsifier)

In a mounted `seanchatmangpt/castle` checkout, admit a public post against an explicit OEM brand twin (Fourth Watch or CindyCat), fail-closed without flags:

```bash
cd /path/to/castle
cargo test --test publish_redaction_court
cargo test --test cli_publish_brand
./target/debug/castle publish post \
  --subject spine-probe \
  --public-anonymize \
  --brand brands/fourth-watch.brand.toml
# omit --public-anonymize → REFUSED_NO_PUBLIC_ANONYMIZE / REFUSED_NO_AUTHORITY (exit 2)
# omit --brand → REFUSED_BRAND (exit 2)
```

Promotion rule: exit 0 with RedactionCourt collaborator on the mint receipt → may lift **sibling mint** to `PARTIAL_ALIVE` / `ALIVE` for that subject only. Spine crown stays non-`ALIVE` until a receipt binds a ggen layout/brand digest into that mint.

### 3.2 In-ggen (non-substitutes)

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

---

## 5. Search corroboration (this land)

| Source | Finding |
|--------|---------|
| `rg CASTLE` in ggen live crates/docs | Gate-2+ inventory notes; how-to marketplace `castle-pack` pin; no publish court |
| `seanchatmangpt/castle` README + `src/publish/README.md` | Role = consequential admissibility + gated public mint |
| castle `docs/sjira/v26.9.28/repos.ttl` | Owns `CONSEQUENTIAL_ADMISSIBILITY`; zcode-cli WRAP as `EXTERNAL_WORKER_PROVIDER` |
| ECOSYSTEM-SPEC-27.1.1 (companion PR) | CASTLE publish = Gate 2–3; inventing inventories REFUSED |
| Smalt / revops / zcode-cli | Sibling repos named; not mounted; spine edges **UNKNOWN** |

---

## 6. Standing receipt (this land)

| Claim | Standing |
|-------|----------|
| Role of CASTLE on horizontal spine | Documented (§1) |
| Crown spine integration | **UNKNOWN** |
| In-ggen CASTLE publish surface | **UNSUPPORTED** |
| Sibling mint / redaction courts | **UNKNOWN** (unmounted) — next verifier §3.1 |
| LinkedIn blast send | **BLOCKED** (AND-gate) |
| GA readiness for CASTLE spine | **OPEN** — falsifiers §4 |

---

## 7. Provenance

GOAL SLICE: CASTLE in horizontal spine (`ggen+Taffy+zcode-cli+CASTLE+Smalt+revops`) standing.
Materialized as a ggen RFC ledger only. No CASTLE binary, Smalt runtime, or revops front-door was executed in this workspace.
