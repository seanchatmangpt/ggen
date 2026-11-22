# SMALT-RAID-STANDING — v27.1.1 economic ops / 4-cloud RAID

| Field | Value |
|-------|-------|
| Document ID | `SMALT-RAID-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` (economic-ops / Gate-0–Plane-5 adjacent) |
| Target GA | **2027-01-01** |
| Materialized | 2026-11-22 |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `6f920670ffaa53ecb7cbd89524fa4375e087f1f7` (push-first honesty reconfirm `cursor/v2711-smalt-reconfirm-bc54` / `bc-b84e0135`; crown lineage ⊇ Smalt RAID tip-wins absorb `2c144954c` onto `571571720` + §6c FinOps+treasury EXIT **0** authority + OOR retip18 `eb28f0eb0` + ui_layout exact-retip6 `864ba13dc` + Foundry HITL retip3 `6f920670f`; smalt evidence pin held — see §6d). |
| Observed smalt tip | FinOps `91997da76f0f356dc2be914bdb0a5b8227518719` on smalt `cursor/v2711-smalt-finops-test-bc54` (parent `875efbb87bde8422236cf9db7224412962dbc5a7` = smalt `main` @ probe; 4-cloud courts `bc-ad9ebffa` + residual mix courts `bc-c700dafb`; warm clone `/tmp/smalt-probe` HEAD match this pass — **no** new mix EXIT0) |
| Crown standing (from ggen) | **ALIVE (lineage)** (Smalt RAID / GA economic face) — **held** (not newly promoted; no new tip-cargo / mix EXIT0 this pass): prior §6c FinOps EXIT **0** / **19 passed** + treasury EXIT **0** / **8 passed** @ `91997da76f0f` remain ancestors of crown tip `6f920670f` (absorb `2c144954c` / base `571571720`); 4-cloud doctor/aws/gcp/azure/twilio **0,0,0,0,0** retained @ `875efbb87` (`bc-ad9ebffa`); residual tax/erp/rates retained @ `91997da76f0f` (`bc-c700dafb`). Foundry / zero-HITL crown **≠** this claim — do **not** invent Foundry `ALIVE`. |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

This document is a standing ledger, not an execution receipt. Source presence,
cross-repo prose, marketplace pack names, and workflow definitions do **not**
promote the crown claim to `ALIVE`. Sibling court exits recorded by Smalt live
`bc-ad9ebffa` (4-cloud @ `875efbb87`) plus FinOps restore tip `91997da76f0f`
(exit 0 / 19 passed) plus residual mix courts (`bc-c700dafb` @ `91997da76f0f`:
treasury/tax/erp/rates exits **0,0,0,0**) are the authority for the rows marked
exit **0** / **ALIVE** below.

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

**Tracked Smalt RAID crown from this repository: `ALIVE`** (sibling 4-cloud
exits **0** + FinOps court **ALIVE** + residual b4b courts exits **0,0,0,0** @
`91997da76f0f`). Foundry / zero-HITL crown remains **≠** Smalt RAID — do **not**
invent Foundry `ALIVE`.

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

### 4.3 Residual GA economic face / b4b — **ALIVE** @ `91997da76f0f`

Executed 2026-11-22 on clone HEAD `91997da76f0f356dc2be914bdb0a5b8227518719`
(`cursor/v2711-smalt-finops-test-bc54`; siblings `ash_graphlaw` /
`graphlaw/graphlaw_ex` / `ash_r2rml` mounted for path deps; Elixir 1.20.4 /
OTP 29 via mise). Agent `bc-c700dafb`. KD author docs may still say
**UNVERIFIED** — module courts below supersede presence-only prose.

| Plane | Command | HEAD | EXIT | Result | Standing |
|-------|---------|------|------|--------|----------|
| P1 Treasury | `mix test test/treasury/treasury_test.exs` | `91997da76f0f` | **0** | **8 passed** | **ALIVE** |
| P3 Tax | `mix test test/taxsanctions/taxsanctions_test.exs` | `91997da76f0f` | **0** | **8 passed** | **ALIVE** |
| P5 ERP | `mix test test/erp/erp_test.exs` | `91997da76f0f` | **0** | **8 passed** | **ALIVE** |
| P10 Rates | `mix test test/rates/rates_test.exs` | `91997da76f0f` | **0** | **7 passed** | **ALIVE** |

```text
cd /path/to/smalt   # tip 91997da76f0f356dc2be914bdb0a5b8227518719
mix test test/treasury/treasury_test.exs      # → exit 0 (8 passed)
mix test test/taxsanctions/taxsanctions_test.exs  # → exit 0 (8 passed)
mix test test/erp/erp_test.exs                # → exit 0 (8 passed)
mix test test/rates/rates_test.exs            # → exit 0 (7 passed)
```

Promotion rule (satisfied):

1. Real `mix test` summary lines + pinned smalt HEAD cited above.
2. Four-cloud exits **0** + FinOps **ALIVE** + four b4b plane courts green ⇒
   Smalt RAID / GA economic face **ALIVE**.
3. Never from ggen alone, never from module/`docs/b4b` presence alone.
4. Foundry / zero-HITL crown remains a separate claim — **not** sealed by
   these courts.

Replay any single residual court at the pin; a new failing EXIT demotes that
plane (and the RAID crown) until re-proven.

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
| This-env light probe (historical) | prior pass: mix **UNSUPPORTED** — superseded by §4.3 / §6b live courts |
| 6-column spine standing | **ALIVE** (doctor/aws/gcp/azure/twilio/fin_ops) |
| Ledger crown | **ALIVE** (Smalt RAID / GA economic face) — Foundry / zero-HITL crown **not** `ALIVE` |
| Next verifier | Replay §4.3 residual courts at pin; Foundry / zero-HITL remain separate |

---

## 6a. GA residual probe (2026-11-22 — b4b register / economic face)

Sidebranch `cursor/v2711-smalt-ga-residual-bc54` @ `cd28efed7` branched from
crown tip `6a3a623bacd33efdc1ec7efccf6a35141662ca53` (**no** crown push then);
tip-wins absorbed onto crown `f5f44571f9a331d46e2895657621b7efd7127f1b`
without dropping OOR retip11 / CASTLE wire content. Smalt sibling reachable via
GitHub + shallow clone (not a ggen workspace mount).

| Field | Value |
|-------|-------|
| Observed ggen tip (probe) | `6a3a623bacd33efdc1ec7efccf6a35141662ca53` (sidebranch base) |
| Observed ggen tip (crown absorb) | `f5f44571f9a331d46e2895657621b7efd7127f1b` (tip-wins; ⊇ OOR/CASTLE) |
| Observed smalt tip (pinned) | `91997da76f0f356dc2be914bdb0a5b8227518719` (clone HEAD match; FinOps restore commit; parent `875efbb87` = smalt `main`) |
| Smalt `main` @ probe | `875efbb87bde8422236cf9db7224412962dbc5a7` — FinOps pin **not** on `main`; lives on `cursor/v2711-smalt-finops-test-bc54` |
| FinOps court file | `test/fin_ops_calculator_test.exs` **PRESENT** @ pin (prior **ALIVE** authority retained; not re-run) |
| b4b register tree | `docs/b4b/{KD-P1-TREASURY,KD-P3-TAX,KD-P5-ERP,KD-P10-RATES,SOURCE-CHECK-P1-P3,SOURCE-CHECK-P5-P10}.md` **PRESENT** |
| b4b modules | `lib/smalt/{treasury,taxsanctions,erp,rates,fin_ops}/**` **PRESENT** @ pin (KD prose that still says “module absent” is **stale relative to tree**) |
| b4b module courts (source) | **PRESENT** — superseded by executed exits in §4.3 / §6b |
| This-env mix/elixir (historical) | prior **ABSENT** → **UNSUPPORTED**; superseded by §6b |
| SOURCE-CHECK / KD standing (author docs) | SOURCE-CHECK **PARTIAL**; KD prose may lag — courts **ALIVE** supersede |
| 6-column spine | **ALIVE** retained (4-cloud @ `875efbb87` + FinOps @ `91997da76f0f`) |
| Ledger crown | **ALIVE** (Smalt RAID / GA economic face) after §6b; Foundry crown **not** invented |
| Next verifier (historical) | §4.3 executed — see §6b |

---

## 6b. Residual mix courts (2026-11-22 — §4.3 executed)

| Field | Value |
|-------|-------|
| Agent | `bc-c700dafb` (Smalt mix residual courts probe) |
| Honesty sidebranch | `cursor/v2711-smalt-mix-courts-bc54` @ `7334aa971` from ggen tip `9173c6529` (probe; **no** crown push) |
| Crown absorb | `cursor/v2711-smalt-mix-absorb-7abb` tip-wins onto `79b64496b` |
| Smalt HEAD | `91997da76f0f356dc2be914bdb0a5b8227518719` |
| Materialization | shallow clone + siblings `ash_graphlaw` / `graphlaw` / `ash_r2rml`; Elixir **1.20.4** / OTP **29** via mise |
| treasury | EXIT **0** — **8 passed** → **ALIVE** |
| taxsanctions | EXIT **0** — **8 passed** → **ALIVE** |
| erp | EXIT **0** — **8 passed** → **ALIVE** |
| rates | EXIT **0** — **7 passed** → **ALIVE** |
| Residual standing | **ALIVE** (all four EXIT 0) |
| Smalt RAID / GA economic face | **ALIVE** (spine + FinOps + residual) |
| Foundry / zero-HITL | **not** promoted |

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

Tip-wins absorb 2026-11-22 on `cursor/v2711-smalt-ga-residual-absorb-da2d`
onto live crown tip `f5f44571f` (cherry-pick `cd28efed7` + tip rewrite):
Observed ggen tip advanced `6a3a623ba` → `f5f44571f` while preserving OOR
retip11 (`96444e302`/`f5f44571f`) and CASTLE crown-wire absorb
(`05c4d69b5`/`b01224bfa`); smalt pin `91997da76f0f` **held**; b4b **PRESENT**;
residual courts then still **UNKNOWN** (mix **UNSUPPORTED**); ledger crown was
**PARTIAL_ALIVE**; Foundry crown `ALIVE` **not** invented; no cargo;
no ManagePullRequest (parent owns #849).

Residual mix courts 2026-11-22 on `cursor/v2711-smalt-mix-courts-bc54` from
absorb tip `9173c6529` (**no** crown push; agent `bc-c700dafb`): smalt
`91997da76f0f` — treasury/tax/erp/rates exits **0,0,0,0** (8/8/8/7 passed) ⇒
residual **ALIVE**; Smalt RAID / GA economic face **ALIVE**; Foundry /
zero-HITL crown **not** invented; no ManagePullRequest.

Tip-wins absorb 2026-11-22 on `cursor/v2711-smalt-mix-absorb-7abb` onto live
crown tip `79b64496b` (cherry-pick `7334aa971` + rebase over OOR retip12;
agent `bc-d1c195db`): Observed ggen tip advanced `9173c6529` → `79b64496b`
while preserving OOR retip12 (cos **503** / revops **NXDOMAIN** **BLOCKED**),
affidavit size-court honesty, BRCE V8/R01 bodies, and verify_witness prod
honesty; smalt pin `91997da76f0f` **held**; residual **ALIVE**; Smalt RAID /
GA economic face **ALIVE**; Foundry / zero-HITL crown remains **PARTIAL** —
**not** invented; no cargo; no ManagePullRequest.

Exact-tip honesty reconfirm 2026-11-22 on `cursor/v2711-smalt-raid-retip-bc54`
from live crown tip `571571720` (**no** crown push / no ManagePullRequest #849):
smalt clone HEAD `91997da76f0f` — narrowest court
`mix test test/fin_ops_calculator_test.exs` → EXIT **0**, **19 passed**;
treasury residual replay EXIT **0**, **8 passed**; 4-cloud columns **retained**
(not re-executed this pass) @ `875efbb87`; Smalt RAID / GA economic face
**ALIVE** **held**; Foundry / zero-HITL crown **not** invented; no cargo.

Tip-wins absorb 2026-11-22 on `cursor/v2711-smalt-raid-retip-absorb-f21a` onto live
crown tip `571571720` (FF from sidebranch `99bd2938a` / `cursor/v2711-smalt-raid-retip-bc54`;
tip stable — no race reapply): FinOps EXIT **0** / **19 passed** + treasury EXIT **0** /
**8 passed** @ smalt `91997da76f0f` **held**; 4-cloud retained @ `875efbb87`; Smalt RAID /
GA economic face **ALIVE** **held**; Foundry / zero-HITL crown remains **PARTIAL** —
**not** invented; no cargo; no ManagePullRequest.

Push-first honesty reconfirm 2026-11-22 on `cursor/v2711-smalt-reconfirm-bc54`
(`bc-b84e0135`) from live crown tip `6f920670f` (⊇ Smalt RAID absorb `2c144954c` /
`571571720` + OOR retip18 `eb28f0eb0` + Foundry HITL retip3): docs + light probes
only — warm `/tmp/smalt-probe` HEAD `91997da76f0f` (FinOps/treasury/calculator blobs
**PRESENT**; `test "` count **19**); ggen falsifier outside `docs/rfc/**` → zero
material hits; `mix` **ABSENT** / no new mix or tip-cargo EXIT0; prior ALIVE
lineage **held** → standing **ALIVE (lineage)**; Foundry / zero-HITL crown **not**
invented; no cargo; no ManagePullRequest. See §6d.

---

## 6c. Exact-tip honesty reconfirm (2026-11-22 — FinOps+treasury replay)

| Field | Value |
|-------|-------|
| Sidebranch | `cursor/v2711-smalt-raid-retip-bc54` from `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `571571720620cd7462b9a740e3bf0b0fdeebc17a` |
| Observed ggen tip | tip-wins absorb onto crown `571571720620cd7462b9a740e3bf0b0fdeebc17a` (sidebranch probe base identical; tip stable — no race reapply) |
| Observed smalt tip | `91997da76f0f356dc2be914bdb0a5b8227518719` (clone `/tmp/smalt-probe`; parent `875efbb87`) |
| Narrowest court | `mix test test/fin_ops_calculator_test.exs` |
| FinOps EXIT | **0** — **19 passed** → **ALIVE** (reconfirm) |
| Residual replay | `mix test test/treasury/treasury_test.exs` → EXIT **0** — **8 passed** → **ALIVE** |
| 4-cloud this pass | **not re-run** — doctor/aws/gcp/azure/twilio exits **0,0,0,0,0** **retained** @ `875efbb87` (`bc-ad9ebffa`) |
| tax/erp/rates this pass | **retained** @ `91997da76f0f` (`bc-c700dafb`) — exits **0,0,0** |
| Smalt RAID / GA economic face | **ALIVE** **held** (FinOps+treasury reconfirm + retained 4-cloud/residual authority) |
| Foundry / zero-HITL | **not** promoted — RAID `ALIVE` ≠ Foundry crown `ALIVE` |
| Crown absorb | tip-wins `cursor/v2711-smalt-raid-retip-absorb-f21a` onto crown `571571720` (side `99bd2938a` / `cursor/v2711-smalt-raid-retip-bc54`; no ManagePullRequest) |

---

## 6d. Push-first honesty reconfirm (2026-11-22 — docs + light probes; no mix/cargo)

| Field | Value |
|-------|-------|
| Sidebranch | `cursor/v2711-smalt-reconfirm-bc54` from `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `6f920670ffaa53ecb7cbd89524fa4375e087f1f7` |
| Agent | `bc-b84e0135` |
| Observed ggen tip | `6f920670ffaa53ecb7cbd89524fa4375e087f1f7` (⊇ prior Smalt RAID tip-wins absorb `2c144954c` onto `571571720` + §6c FinOps+treasury authority; also ⊇ OOR retip18 `eb28f0eb0` + Foundry HITL retip3) |
| Prior ALIVE lineage | **held** — `git merge-base --is-ancestor 571571720 HEAD` → **0**; `git merge-base --is-ancestor 2c144954c HEAD` → **0** |
| Observed smalt tip | `91997da76f0f356dc2be914bdb0a5b8227518719` (warm `/tmp/smalt-probe` HEAD match; parent `875efbb87`) |
| New mix / tip-cargo EXIT0 this pass | **none** — `mix` **ABSENT** this env; Gate1 tip6 owns cargo thrash — deliberately not re-run |
| Light probe — FinOps court blob | `test/fin_ops_calculator_test.exs` **PRESENT** @ pin (`test "` count **19**; GitHub blob + warm clone) |
| Light probe — calculator module | `lib/smalt/fin_ops/calculator.ex` **PRESENT** @ pin |
| Light probe — treasury court blob | `test/treasury/treasury_test.exs` **PRESENT** @ pin |
| Light probe — ggen falsifier | `rg` outside `docs/rfc/**` for `\bRAID\b\|4-cloud\|economic.?ops` → **zero** material hits (exit **1**) |
| 4-cloud this pass | **not re-run** — doctor/aws/gcp/azure/twilio exits **0,0,0,0,0** **retained** @ `875efbb87` (`bc-ad9ebffa`) |
| FinOps / residual authority | §6c EXIT **0** / **19** + treasury EXIT **0** / **8** + tax/erp/rates **0,0,0** **retained** @ `91997da76f0f` — not re-executed |
| Smalt RAID / GA economic face | **ALIVE (lineage)** **held** — prior ALIVE lineage still on crown tip; do **not** invent a fresh tip-cargo / mix `ALIVE` without EXIT0 |
| Foundry / zero-HITL | **not** promoted — RAID lineage `ALIVE` ≠ Foundry crown `ALIVE` |
| Next exact verifier | Replay §4.2 FinOps at pin (narrowest): `cd /path/to/smalt # tip 91997da76f0f356dc2be914bdb0a5b8227518719` → `mix test test/fin_ops_calculator_test.exs` → exit **0** / **19 passed**; residual replay §4.3; 4-cloud §4.1 if columns drift |

## Retip — FinOps + doctor live replay (2026-11-22 3:36 PM PST)

| Field | Value |
|-------|-------|
| Clone | `/tmp/smalt-raid` @ `91997da76f0f` |
| Elixir | mise `elixir@1.20.5-otp-29` (OTP 29) |
| `just doctor` | EXIT **0** (`/tmp/smalt-doctor3.log`) — after elixir+terraform+certs+tmp |
| `mix test test/fin_ops_calculator_test.exs` | **19 passed**; EXIT **0** (`/tmp/smalt-finops-mix2.log`) |
| TS FinOps (non-substitute) | **9**/EXIT:**0** (`/tmp/smalt-finops-ts.log`) |
| Standing | Smalt RAID FinOps replay **ALIVE** at pin — **≠ Foundry / zero-HITL / path `ALIVE`** |
| Construct | full job still **PENDING_CI** on ggen tip `7bb443e99` |

