# SMALT-RAID-STANDING — v27.1.1 economic ops / 4-cloud RAID

| Field | Value |
|-------|-------|
| Document ID | `SMALT-RAID-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` (economic-ops / Gate-0–Plane-5 adjacent) |
| Target GA | **2027-01-01** |
| Materialized | 2026-11-22 |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `6a3a623bacd33efdc1ec7efccf6a35141662ca53` (GA residual sidebranch `cursor/v2711-smalt-ga-residual-bc54`; ⊇ `1cedf8465` tip-wins / `1f335ea82` / `e73bf686c` / `27436608c`; smalt evidence pin held). Live crown may advance via OOR retip11 (`96444e302+`) — this sidebranch does **not** race tip. |
| Observed smalt tip | FinOps `91997da76f0f356dc2be914bdb0a5b8227518719` on smalt `cursor/v2711-smalt-finops-test-bc54` (parent `875efbb87bde8422236cf9db7224412962dbc5a7` = smalt `main` @ probe; 4-cloud courts `bc-ad9ebffa` + retry `bc-1e876087`) |
| Crown standing (from ggen) | **PARTIAL_ALIVE** (full RAID / remaining GA economic face / b4b still open) — **6-column spine ALIVE**: doctor/aws/gcp/azure/twilio exits **0,0,0,0,0** @ `875efbb87`; `fin_ops` **ALIVE** @ `91997da76f0f` (`mix test test/fin_ops_calculator_test.exs` → exit **0**, **19 passed**). Never invent full RAID / Foundry crown `ALIVE`. |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

This document is a standing ledger, not an execution receipt. Source presence,
cross-repo prose, marketplace pack names, and workflow definitions do **not**
promote the crown claim to `ALIVE`. Sibling court exits recorded by Smalt live
`bc-ad9ebffa` (4-cloud @ `875efbb87`) plus FinOps restore tip `91997da76f0f`
(exit 0 / 19 passed) are the authority for the rows marked exit **0** / **ALIVE**
below.

---

## 1. Requirement

For Sovereign Ecosystem **v27.1.1** GA, the **Smalt 4-Cloud RAID** economic-ops
surface SHALL be court-proven before any economic-ops / multi-cloud substrate
claim may be promoted to `ALIVE`.

Admitted surface identity (out-of-repo, observed by Smalt live `bc-ad9ebffa` on
tip `875efbb87`, included by FinOps tip `91997da76f0f`, not by local mount in
this ggen workspace):

| Column | Role in the RAID | Smalt evidence paths (sibling) | Standing @ `875efbb87` (⊆ `91997da76f0f`) |
|--------|------------------|--------------------------------|------------------------------------------|
| **Azure** | Primary control/data plane column | `docs/azure-parity/**`, `lib/smalt/arm_gateway/**`, `lib/smalt/data_plane/**`; court: `mix test test/data_plane test/internal` | exit **0** |
| **AWS** | Second hyperscaler column | `docs/aws-parity/**`, `lib/smalt/aws/**`; court: `mix test test/aws` | exit **0** |
| **GCP** | Third hyperscaler column | `docs/gcp-parity/**`, `lib/smalt/gcp/**`; court: `mix test test/gcp` | exit **0** |
| **Twilio / fnox edge** | Fourth (comms / edge) column | `docs/twilio-parity/**`, `test/twilio/**`; court: `mix test test/twilio` | exit **0** |

Economic-ops modules on the same sibling (not 4-cloud parity itself, but the
economic face the RAID is meant to host):

- FinOps avoided-spend calculator: `lib/smalt/fin_ops/calculator.ex` (**PRESENT**)
- FinOps court: `test/fin_ops_calculator_test.exs` — **PRESENT** → **ALIVE** @ `91997da76f0f` (exit **0**, **19 passed**)
- b4b planes (treasury / tax / ERP / rates KD registers): `docs/b4b/**`
- ERP ledger face: `lib/smalt/erp/**`

ggen’s role under ECOSYSTEM-SPEC-27.1.1: **manufacture / pack / doctrine only**.
Plane-5 / economic-ops **DO** remains outside ggen (`SELECT ≠ CONSTRUCT ≠ DO`).

---

## 2. In-repo evidence (ggen) — still no RAID surface

Search method on tip (ggen falsifier; must stay empty outside standing/rfc docs):

```text
rg -i -n '\bRAID\b|4-cloud|economic.?ops' \
  --glob '!ontology_catalogue/**' --glob '!ontologies/**' --glob '!target/**' \
  --glob '!docs/rfc/v27.1.1/SMALT-RAID-STANDING.md' .
```

| Probe | Result | Standing |
|-------|--------|----------|
| Literal `RAID` / `4-cloud` / `economic ops` in live crates + docs | **zero** material hits outside standing/rfc docs | in-repo surface **UNKNOWN** |
| Workspace member or path dep on smalt | absent from root `Cargo.toml` `[workspace] members` | **UNSUPPORTED** in ggen |
| Local mount of `seanchatmangpt/smalt` in this ggen workspace | **not mounted** | courts executed on Smalt live agent — not here |
| Cross-repo smalt citations in ggen jira receipts / marketplace packs | present as **foreign** citations | not execution evidence — do not promote |
| ECOSYSTEM Gate 0 / Plane 5 economic-ops DO | declared **OUT OF SCOPE** for ggen DO | **BLOCKED** from claiming `ALIVE` here |

**Tracked crown from this repository: `PARTIAL_ALIVE`** (sibling 4-cloud exits
**0** + FinOps court **ALIVE**; remaining GA economic face / b4b still open).
Do **not** invent full RAID / Foundry crown `ALIVE`.

---

## 3. Out-of-repo owner + live tip

| Role | Owner |
|------|-------|
| Primary implementation + courts | **`seanchatmangpt/smalt`** |
| Hyperscaler adapter membrane | `smalt/posh` (Go) — when armed |
| Economic-ops FinOps / b4b modules | `smalt` (`lib/smalt/fin_ops`, `docs/b4b`, `lib/smalt/erp`) |
| ggen | standing ledger + optional pack manufacture only — **no ambient DO** |

| Field | Value |
|-------|-------|
| Smalt tip SHA | `91997da76f0f356dc2be914bdb0a5b8227518719` (parent `875efbb87bde8422236cf9db7224412962dbc5a7`) |
| Live agent (4-cloud) | `bc-ad9ebffa` @ `875efbb87` |
| Doctor | `just doctor` → exit **0** @ `875efbb87` |
| AWS | `mix test test/aws` → exit **0** @ `875efbb87` |
| GCP | `mix test test/gcp` → exit **0** @ `875efbb87` |
| Azure | `mix test test/data_plane test/internal` → exit **0** @ `875efbb87` |
| Twilio | `mix test test/twilio` → exit **0** @ `875efbb87` |
| FinOps | `mix test test/fin_ops_calculator_test.exs` → exit **0**, **19 passed** @ `91997da76f0f` → **ALIVE** |

---

## 4. Exact verifier (replay) + next

### 4.1 Replay (PARTIAL_ALIVE — 4-cloud columns @ `875efbb87`)

Run **in a mounted `seanchatmangpt/smalt` checkout** at tip `91997da76f0f`
(or ancestor `875efbb87` for 4-cloud-only reconfirm):

```text
cd /path/to/smalt   # tip 91997da76f0f356dc2be914bdb0a5b8227518719
just doctor
# → exit 0
mix test test/aws
# → exit 0
mix test test/gcp
# → exit 0
mix test test/data_plane test/internal
# → exit 0  (Azure column)
mix test test/twilio
# → exit 0
```

### 4.2 FinOps court (ALIVE @ `91997da76f0f`)

```text
cd /path/to/smalt   # tip 91997da76f0f356dc2be914bdb0a5b8227518719
test -f test/fin_ops_calculator_test.exs
# → exit 0 (PRESENT)
mix test test/fin_ops_calculator_test.exs
# → exit 0 (19 passed) ⇒ standing ALIVE
```

### 4.3 Next exact verifier (remaining GA economic face / b4b)

Register + module surface is **PRESENT** @ `91997da76f0f` (clone + GitHub
blob probe 2026-11-22 on `cursor/v2711-smalt-ga-residual-bc54`). KD docs
themselves still say **UNVERIFIED** / SOURCE-CHECK **PARTIAL** — do **not**
promote from presence. Mix/Elixir **ABSENT** in the ggen cloud env → live
b4b courts **UNSUPPORTED** here (exact next is the sibling mount below).

Admitted residual courts (GA economic face named by `docs/b4b/**`):

| Plane | KD register | Module path | Module court | Register standing @ pin |
|-------|-------------|-------------|--------------|-------------------------|
| P1 Treasury | `docs/b4b/KD-P1-TREASURY.md` | `lib/smalt/treasury/**` **PRESENT** | `mix test test/treasury/treasury_test.exs` (+ vectors) | KD **UNVERIFIED** |
| P3 Tax | `docs/b4b/KD-P3-TAX.md` | `lib/smalt/taxsanctions/**` **PRESENT** | `mix test test/taxsanctions/taxsanctions_test.exs` (+ vectors) | KD **UNVERIFIED** |
| P5 ERP | `docs/b4b/KD-P5-ERP.md` | `lib/smalt/erp/**` **PRESENT** | `mix test test/erp/erp_test.exs` (+ vectors) | KD **UNVERIFIED** |
| P10 Rates | `docs/b4b/KD-P10-RATES.md` | `lib/smalt/rates/**` **PRESENT** | `mix test test/rates/rates_test.exs` (+ vectors) | KD **UNVERIFIED** |

```text
cd /path/to/smalt   # tip 91997da76f0f356dc2be914bdb0a5b8227518719 (or newer HEAD that contains it)
# Remaining GA economic face / b4b — each must exit 0 (or typed REFUSED_*):
mix test test/treasury/treasury_test.exs
mix test test/taxsanctions/taxsanctions_test.exs
mix test test/erp/erp_test.exs
mix test test/rates/rates_test.exs
# Optional vector overlays (fixture-shape; loud [SKIP] only when module ABSENT):
mix test test/treasury_vectors test/taxsanctions_vectors test/erp_vectors test/rates_vectors
# Cite HEAD SHA + exit 0 / pass counts in this ledger before full RAID ALIVE
```

Promotion rule:

1. Paste the real `mix test` summary lines (exit 0, counts) into the smalt
   parity receipts with a pinned smalt HEAD SHA.
2. Cite that receipt path + SHA from this ledger.
3. Four-cloud exits **0** + FinOps **ALIVE** without remaining GA economic face =
   **PARTIAL_ALIVE** only (FinOps closed; RAID crown not sealed).
4. Full RAID / economic-ops `ALIVE` requires FinOps court exit **0** **and**
   the four b4b plane courts above green (or typed `REFUSED_*`) — never from
   ggen alone, never from module/`docs/b4b` presence alone.

Until remaining GA economic face is green (or typed `REFUSED_*`), any ggen
claim of full RAID / economic-ops `ALIVE` is `REFUSED_UNGROUNDED`. FinOps
itself is **ALIVE** @ `91997da76f0f` and must not be demoted without a new
failing court.

---

## 5. GA falsifier (2027-01-01)

The GA claim fails if **any** of the following is true:

1. **Missing 4-column court** — Azure, AWS, GCP, or Twilio/fnox column lacks a
   HEAD-pinned green court receipt in `seanchatmangpt/smalt`.
2. **Economic face unproven** — FinOps calculator court regresses from **ALIVE**,
   or any admitted b4b plane required by the GA economic-ops register has no
   green court (or remains loud `[SKIP]` / KD-OPEN).
3. **Standing fabrication** — ggen (or any sibling) promotes Smalt 4-Cloud RAID
   / economic ops to `ALIVE` from docs-only, pack-name, or fleet-census
   evidence without the smalt verifier above.
4. **Authority collapse** — ggen actuates economic-ops DO without BRCE+AS-01
   (violates ECOSYSTEM-SPEC-27.1.1 Plane-5 / Gate-0 out-of-scope law).

Falsifier command (ggen-side, must stay red until smalt receipts land):

```text
rg -i -n '\bRAID\b|4-cloud|economic.?ops' \
  --glob '!ontology_catalogue/**' --glob '!ontologies/**' \
  --glob '!docs/rfc/v27.1.1/SMALT-RAID-STANDING.md' .
# expected: zero material hits; a code/runtime hit without a cited smalt
# receipt is itself a GA defect
```

---

## 6. Tip reconfirm (2026-11-22 — Clause-3 ops / 6-column spine)

| Field | Value |
|-------|-------|
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `1cedf846563fe7d90e56a61a4e6cf4cc08111bba` (⊇ `1f335ea82`/`27436608c`/`e73bf686c`; tip-wins reapply — `fa2c782de` was NOT on crown) |
| Observed smalt tip | `91997da76f0f` (FinOps) / parent `875efbb87` (4-cloud) via `bc-ad9ebffa` + `bc-1e876087` |
| Sibling mount in this ggen workspace | **not mounted**; GitHub blob reconfirm `test/fin_ops_calculator_test.exs` **PRESENT** @ `91997da76f0f` (parent `875efbb87`); mix/elixir **ABSENT** this env |
| 4-cloud exits (authority) | doctor=**0** aws=**0** gcp=**0** azure=**0** twilio=**0** @ `875efbb87` (`bc-ad9ebffa`) — retained; no new mix courts this pass |
| FinOps (authority) | **ALIVE** @ `91997da76f0f` — `mix test test/fin_ops_calculator_test.exs` → exit **0**, **19 passed** — retained; blob **PRESENT** |
| This-env light probe | mix courts **UNSUPPORTED** here (no cargo / no smalt mount; ui_layout owns oxrocksdb) — does **not** demote sibling authority |
| 6-column spine standing | **ALIVE** (doctor/aws/gcp/azure/twilio/fin_ops) |
| Ledger crown | **PARTIAL_ALIVE** — full RAID / remaining GA economic face / b4b / Foundry crown **not** `ALIVE` |
| Next verifier | §4.3 remaining GA economic face / b4b courts |

---

## 6a. GA residual probe (2026-11-22 — b4b register / economic face)

Sidebranch `cursor/v2711-smalt-ga-residual-bc54` branched from crown tip
`6a3a623bacd33efdc1ec7efccf6a35141662ca53` (**no** crown push). Smalt sibling
reachable via GitHub + shallow clone (not a ggen workspace mount).

| Field | Value |
|-------|-------|
| Observed ggen tip (this pass) | `6a3a623bacd33efdc1ec7efccf6a35141662ca53` |
| Observed smalt tip (pinned) | `91997da76f0f356dc2be914bdb0a5b8227518719` (clone HEAD match; FinOps restore commit; parent `875efbb87` = smalt `main`) |
| Smalt `main` @ probe | `875efbb87bde8422236cf9db7224412962dbc5a7` — FinOps pin **not** on `main`; lives on `cursor/v2711-smalt-finops-test-bc54` |
| FinOps court file | `test/fin_ops_calculator_test.exs` **PRESENT** @ pin (prior **ALIVE** authority retained; not re-run) |
| b4b register tree | `docs/b4b/{KD-P1-TREASURY,KD-P3-TAX,KD-P5-ERP,KD-P10-RATES,SOURCE-CHECK-P1-P3,SOURCE-CHECK-P5-P10}.md` **PRESENT** |
| b4b modules | `lib/smalt/{treasury,taxsanctions,erp,rates,fin_ops}/**` **PRESENT** @ pin (KD prose that still says “module absent” is **stale relative to tree**) |
| b4b module courts (source) | `test/treasury/treasury_test.exs` (8), `test/taxsanctions/taxsanctions_test.exs` (8), `test/erp/erp_test.exs` (6+), `test/rates/rates_test.exs` (7) — **PRESENT**; exit codes **UNKNOWN** (not executed) |
| This-env mix/elixir | **ABSENT** → b4b court execution **UNSUPPORTED** here |
| SOURCE-CHECK / KD standing (author docs) | SOURCE-CHECK **PARTIAL**; KD-P1/P3/P5/P10 **UNVERIFIED** — presence ≠ green court |
| 6-column spine | **ALIVE** retained (4-cloud @ `875efbb87` + FinOps @ `91997da76f0f`) |
| Ledger crown | **PARTIAL_ALIVE** — full RAID / GA economic face / b4b **not** promoted; Foundry crown **not** invented |
| Next verifier (crown absorb) | §4.3 four-plane mix courts → exit **0** at pinned HEAD; then cite receipts here |

---

## 7. Provenance

Materialized as the v27.1.1 goal-slice standing ledger for **Smalt 4-Cloud RAID
/ economic ops**. Honesty tip pin 2026-11-22 on `5e00619a4`
(`cursor/v2711-ccs-ops-honesty-bc54`) held **UNKNOWN**+**BLOCKED**. Live
4-cloud land 2026-11-22 on `cursor/v2711-smalt-partial-bc54` against ggen tip
`8a68da481` / smalt tip `875efbb87` (`bc-ad9ebffa`: doctor/aws/gcp/azure/twilio
exits **0,0,0,0,0**). Retry honesty refresh (`bc-1e876087`): RAID
columns labeled **ALIVE**; FinOps briefly **PARTIAL_ALIVE** via burn-path
`docs/jira/v26.10.18/burn/test/fin_ops_calculator_test.exs` exit **0**.

FinOps restore land (`cursor/v2711-smalt-finops-alive-bc54`) → canonical
`test/fin_ops_calculator_test.exs` **ALIVE** @ `91997da76f0f` (exit **0**,
**19 passed**); FOUNDRY/OOR spine rows **ALIVE**. Header honesty lag retained
stale `fin_ops` **PARTIAL**/`ABSENT` prose until Clause-3 ops retip
`cursor/v2711-smalt-retip-bc54` @ `89cf4807a` absorbed onto tip `27436608c+` (⊇ `a29e1cd92`/`60b15c778`):
header + ECOSYSTEM §13.3 aligned to 6-column spine **ALIVE**; full RAID
ledger crown stays **PARTIAL_ALIVE**; Foundry / zero-HITL crown **not**
invented; tip-cargo exclusive flock spared; no cargo.

Honesty tip-pin 2026-11-22 on `cursor/v2711-smalt-raid-honesty-5517` onto live
crown tip `1f335ea82` (post–Gate-1 honesty retip): Observed ggen tip advanced
from stale `27436608c` → `1f335ea82`; smalt evidence pins **held** (`875efbb87`
4-cloud + `91997da76f0f` FinOps); ledger crown remains **PARTIAL_ALIVE**; full
RAID / Foundry crown `ALIVE` **not** invented; no cargo; no ManagePullRequest.

Tip-wins reapply 2026-11-22 on `cursor/v2711-smalt-raid-honesty-tipwin-cbf7`
onto CASTLE-absorbed crown tip `1cedf8465` (parent `1f335ea82`): claimed Smalt
RAID pin `fa2c782de` was **NOT** on crown after CASTLE absorb — integrity
repair reapplied standing content tip-wins. Observed ggen tip advanced
`1f335ea82` → `1cedf8465`; smalt evidence pins **held** (`875efbb87` 4-cloud +
`91997da76f0f` FinOps); ledger crown remains **PARTIAL_ALIVE**; full RAID /
Foundry crown `ALIVE` **not** invented; no cargo; no ManagePullRequest.

GA residual advance 2026-11-22 on `cursor/v2711-smalt-ga-residual-bc54` from
live crown tip `6a3a623ba` (read-only fetch; **no** crown push; distinct from
castle-wire / verify-witness-prod): shallow-cloned smalt @ `91997da76f0f`;
b4b register + treasury/tax/erp/rates modules + module courts **PRESENT**; KD
docs remain **UNVERIFIED**/SOURCE-CHECK **PARTIAL**; mix **UNSUPPORTED** this
env → residual courts **UNKNOWN**; ledger crown stays **PARTIAL_ALIVE**; full
RAID / Foundry `ALIVE` **not** invented; FOUNDRY spine Smalt row unchanged
(already names residual); no ManagePullRequest.
