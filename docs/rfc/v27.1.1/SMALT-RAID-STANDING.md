# SMALT-RAID-STANDING — v27.1.1 economic ops / 4-cloud RAID

| Field | Value |
|-------|-------|
| Document ID | `SMALT-RAID-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` (economic-ops / Gate-0–Plane-5 adjacent) |
| Target GA | **2027-01-01** |
| Materialized | 2026-11-22 |
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `8a68da481621de793dacae4c7db92b0c2f667a02` (post-`5480d7ada`; tip pin `cursor/v2711-smalt-partial-bc54`) |
| Observed smalt tip | `main` @ `875efbb87bde8422236cf9db7224412962dbc5a7` (live courts `bc-ad9ebffa` + retry `bc-1e876087`; not mounted in this ggen workspace) |
| Crown standing (from ggen) | **PARTIAL_ALIVE** — RAID columns doctor/aws/gcp/azure/twilio **ALIVE** @ `875efbb87`; `fin_ops` **PARTIAL_ALIVE** (burn-path exit **0**; canonical `test/fin_ops_calculator_test.exs` **ABSENT**). **Do not** claim full Smalt RAID / economic-ops `ALIVE` while FinOps is non-canonical. Never invent Foundry crown `ALIVE`. |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

This document is a standing ledger, not an execution receipt. Source presence,
cross-repo prose, marketplace pack names, and workflow definitions do **not**
promote the crown claim to `ALIVE`. Sibling court exits recorded by Smalt live
`bc-ad9ebffa` (4-cloud) and retry `bc-1e876087` (burn-path FinOps) are the
authority for the rows below.

---

## 1. Requirement

For Sovereign Ecosystem **v27.1.1** GA, the **Smalt 4-Cloud RAID** economic-ops
surface SHALL be court-proven before any economic-ops / multi-cloud substrate
claim may be promoted to `ALIVE`.

Admitted surface identity (out-of-repo, observed by Smalt live agents on tip
`875efbb87`, not by local mount in this ggen workspace):

| Column | Role in the RAID | Smalt evidence paths (sibling) | Standing @ `875efbb87` |
|--------|------------------|--------------------------------|------------------------|
| **Doctor** | Substrate preflight | `just doctor` / `scripts/doctor.py` | **ALIVE** (exit **0**) |
| **Azure** | Primary control/data plane column | `docs/azure-parity/**`, `lib/smalt/arm_gateway/**`, `lib/smalt/data_plane/**`; court: `mix test test/data_plane test/internal` | **ALIVE** (exit **0**) |
| **AWS** | Second hyperscaler column | `docs/aws-parity/**`, `lib/smalt/aws/**`; court: `mix test test/aws` | **ALIVE** (exit **0**) |
| **GCP** | Third hyperscaler column | `docs/gcp-parity/**`, `lib/smalt/gcp/**`; court: `mix test test/gcp` | **ALIVE** (exit **0**) |
| **Twilio / fnox edge** | Fourth (comms / edge) column | `docs/twilio-parity/**`, `test/twilio/**`; court: `mix test test/twilio` | **ALIVE** (exit **0**) |

Economic-ops modules on the same sibling (not 4-cloud parity itself, but the
economic face the RAID is meant to host):

- FinOps avoided-spend calculator: `lib/smalt/fin_ops/calculator.ex` (**PRESENT**)
- FinOps court (canonical): `test/fin_ops_calculator_test.exs` — **ABSENT**
- FinOps court (burn-path, non-canonical): `docs/jira/v26.10.18/burn/test/fin_ops_calculator_test.exs` — exit **0** → **PARTIAL_ALIVE** (`bc-1e876087`)
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
| Local mount of `seanchatmangpt/smalt` in this ggen workspace | **not mounted** | courts executed on Smalt live agents — not here |
| Cross-repo smalt citations in ggen jira receipts / marketplace packs | present as **foreign** citations | not execution evidence — do not promote |
| ECOSYSTEM Gate 0 / Plane 5 economic-ops DO | declared **OUT OF SCOPE** for ggen DO | **BLOCKED** from claiming `ALIVE` here |

**Tracked crown from this repository: `PARTIAL_ALIVE`** (sibling RAID columns
**ALIVE**; FinOps burn-path **PARTIAL_ALIVE**; canonical FinOps still open).
Do **not** invent full RAID / economic-ops `ALIVE`.

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
| Smalt tip SHA | `875efbb87bde8422236cf9db7224412962dbc5a7` |
| Live agents | `bc-ad9ebffa` (4-cloud); retry `bc-1e876087` (burn-path FinOps) |
| Doctor | `just doctor` → exit **0** → **ALIVE** |
| AWS | `mix test test/aws` → exit **0** → **ALIVE** |
| GCP | `mix test test/gcp` → exit **0** → **ALIVE** |
| Azure | `mix test test/data_plane test/internal` → exit **0** → **ALIVE** |
| Twilio | `mix test test/twilio` → exit **0** → **ALIVE** |
| FinOps (burn) | `mix test docs/jira/v26.10.18/burn/test/fin_ops_calculator_test.exs` → exit **0** → **PARTIAL_ALIVE** |
| FinOps (canonical) | `test/fin_ops_calculator_test.exs` **ABSENT** — restore still next |

---

## 4. Exact verifier (replay) + next

### 4.1 Replay (RAID columns **ALIVE**)

Run **in a mounted `seanchatmangpt/smalt` checkout** at tip `875efbb87`:

```text
cd /path/to/smalt   # tip 875efbb87bde8422236cf9db7224412962dbc5a7
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

### 4.2 FinOps — PARTIAL_ALIVE (burn path) + next (canonical restore)

```text
# Burn-path (non-canonical) — exit 0 @ 875efbb87 via bc-1e876087:
mix test docs/jira/v26.10.18/burn/test/fin_ops_calculator_test.exs
# → exit 0 ⇒ fin_ops PARTIAL_ALIVE (path non-canonical)

# Canonical subject ABSENT on tip 875efbb87:
test -f test/fin_ops_calculator_test.exs
# → exit 1 (ABSENT)

# Next (required before full RAID / economic-ops ALIVE):
# 1. Restore test/fin_ops_calculator_test.exs onto smalt tip (admit from
#    burn copy only if byte-identity / court law still holds —
#    do not invent assertions).
# 2. mix test test/fin_ops_calculator_test.exs → exit 0
# 3. Cite HEAD SHA + exit 0 in this ledger before any crown ALIVE promotion.
```

Promotion rule:

1. Paste the real `mix test` summary lines (exit 0, counts) into the smalt
   parity receipts with a pinned smalt HEAD SHA.
2. Cite that receipt path + SHA from this ledger.
3. Four-cloud columns exit **0** + FinOps only on burn path = crown
   **PARTIAL_ALIVE** only (columns may be labeled **ALIVE**; FinOps
   **PARTIAL_ALIVE**; full RAID / economic-ops crown stays non-`ALIVE`).
4. Full RAID / economic-ops `ALIVE` requires **canonical** FinOps court exit
   **0** (and any remaining GA economic face named by the register) — never
   from ggen alone, and never from a burn-path-only green.

Until canonical FinOps is restored and green, any ggen claim of full Smalt
RAID / economic-ops `ALIVE` is `REFUSED_UNGROUNDED`.

---

## 5. GA falsifier (2027-01-01)

The GA claim fails if **any** of the following is true:

1. **Missing 4-column court** — Azure, AWS, GCP, or Twilio/fnox column lacks a
   HEAD-pinned green court receipt in `seanchatmangpt/smalt`.
2. **Economic face unproven** — FinOps calculator lacks a **canonical**
   `test/fin_ops_calculator_test.exs` green court (burn-path-only remains
   **PARTIAL_ALIVE**), or any admitted b4b plane required by the GA
   economic-ops register has no green court (or remains loud `[SKIP]` /
   KD-OPEN).
3. **Standing fabrication** — ggen (or any sibling) promotes Smalt 4-Cloud RAID
   / economic ops to full `ALIVE` from docs-only, pack-name, fleet-census, or
   burn-path-only evidence without the canonical smalt verifier above.
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

## 6. Tip reconfirm (2026-11-22 — PARTIAL_ALIVE crown)

| Field | Value |
|-------|-------|
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `8a68da481621de793dacae4c7db92b0c2f667a02` |
| Observed smalt tip | `875efbb87bde8422236cf9db7224412962dbc5a7` via `bc-ad9ebffa` + retry `bc-1e876087` |
| Sibling mount in this ggen workspace | **not mounted** — courts ran on Smalt live |
| RAID columns | doctor/aws/gcp/azure/twilio **ALIVE** (exits **0,0,0,0,0**) |
| FinOps | **PARTIAL_ALIVE** — burn-path exit **0**; canonical `test/fin_ops_calculator_test.exs` **ABSENT** (restore still next) |
| Crown standing | **PARTIAL_ALIVE** — **not** full RAID/`ALIVE`; Foundry crown stays **PARTIAL_ALIVE** |
| Next verifier | §4.2 restore canonical FinOps test → exit 0 |

---

## 7. Provenance

Materialized as the v27.1.1 goal-slice standing ledger for **Smalt 4-Cloud RAID
/ economic ops**. Honesty tip pin 2026-11-22 on `5e00619a4`
(`cursor/v2711-ccs-ops-honesty-bc54`) held **UNKNOWN**+**BLOCKED**. Live
4-cloud land 2026-11-22 on `cursor/v2711-smalt-partial-bc54` against ggen tip
`8a68da481` / smalt tip `875efbb87` (`bc-ad9ebffa`: doctor/aws/gcp/azure/twilio
exits **0,0,0,0,0**). Retry honesty refresh (`bc-1e876087` / this land): RAID
columns labeled **ALIVE**; FinOps upgraded **UNSUPPORTED**→**PARTIAL_ALIVE**
via burn-path `docs/jira/v26.10.18/burn/test/fin_ops_calculator_test.exs`
exit **0**; canonical path still **ABSENT**. Full Smalt RAID / economic-ops
crown and Foundry crown stay **PARTIAL_ALIVE** / non-`ALIVE`.
