# SMALT-RAID-STANDING — v27.1.1 economic ops / 4-cloud RAID

| Field | Value |
|-------|-------|
| Document ID | `SMALT-RAID-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1` (economic-ops / Gate-0–Plane-5 adjacent) |
| Target GA | **2027-01-01** |
| Materialized | 2026-11-22 |
| Observed base | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `5e00619a422a0d8e451029f0aaf8b7a63e27da46` (2026-11-22 honesty pin; prior `origin/main` @ `d173cad5792e837bd4fec730d120c582b9bc5d4c`) |
| Crown standing (from ggen) | **UNKNOWN** (capability not observed in-repo) + **BLOCKED** (DO owner is out-of-repo; smalt not mounted) — never `ALIVE` from ggen |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

This document is a standing ledger, not an execution receipt. Source presence,
cross-repo prose, marketplace pack names, and workflow definitions do **not**
promote the crown claim to `ALIVE`.

---

## 1. Requirement

For Sovereign Ecosystem **v27.1.1** GA, the **Smalt 4-Cloud RAID** economic-ops
surface SHALL be court-proven before any economic-ops / multi-cloud substrate
claim may be promoted to `ALIVE`.

Admitted surface identity (out-of-repo, observed by GitHub API + sparse clone of
`seanchatmangpt/smalt`, not by local mount):

| Column | Role in the RAID | Smalt evidence paths (sibling) |
|--------|------------------|--------------------------------|
| **Azure** | Primary control/data plane column | `docs/azure-parity/**`, `lib/smalt/arm_gateway/**`, `lib/smalt/data_plane/**` |
| **AWS** | Second hyperscaler column | `docs/aws-parity/**`, `lib/smalt/aws/**`, `mix test test/aws` |
| **GCP** | Third hyperscaler column | `docs/gcp-parity/**`, `lib/smalt/gcp/**`, `mix test test/gcp` |
| **Twilio / fnox edge** | Fourth (comms / edge) column | `docs/twilio-parity/**`, fnox route law in KD-TWILIO |

Economic-ops modules on the same sibling (not 4-cloud parity itself, but the
economic face the RAID is meant to host):

- FinOps avoided-spend calculator: `lib/smalt/fin_ops/calculator.ex`
- b4b planes (treasury / tax / ERP / rates KD registers): `docs/b4b/**`
- ERP ledger face: `lib/smalt/erp/**`

ggen’s role under ECOSYSTEM-SPEC-27.1.1: **manufacture / pack / doctrine only**.
Plane-5 / economic-ops **DO** remains outside ggen (`SELECT ≠ CONSTRUCT ≠ DO`).

---

## 2. In-repo evidence (ggen) — UNKNOWN / BLOCKED

Search method on `origin/main` @ `d173cad57` (this branch tip at materialization):

```text
rg -i -n '\bRAID\b|4-cloud|four\.cloud|economic.?ops' \
  --glob '!ontology_catalogue/**' --glob '!ontologies/**' --glob '!target/**'
```

| Probe | Result | Standing |
|-------|--------|----------|
| Literal `RAID` / `4-cloud` / `economic ops` in live crates + docs | **zero** material hits (TopBraid / CloudEvents false positives only) | **UNKNOWN** |
| Workspace member or path dep on smalt | absent from root `Cargo.toml` `[workspace] members` | **UNSUPPORTED** in ggen |
| Local mount `/Users/sac/smalt` or sibling checkout | **not mounted** in this cloud workspace | **BLOCKED** (cannot run smalt courts here) |
| Cross-repo smalt citations in ggen jira receipts / marketplace packs | present as **foreign** citations (`marketplace/packs/kafka-wire-pack.toml`, `docs/jira/v26.11.*/**`) | not execution evidence — do not promote |
| ECOSYSTEM Gate 0 / Plane 5 economic-ops DO | declared **OUT OF SCOPE** for ggen DO | **BLOCKED** from claiming `ALIVE` here |

**Crown claim from this repository: `UNKNOWN` + `BLOCKED`.**

Do not invent a PARTIAL_ALIVE row from pack name echoes or FinOps vocabulary
mirrors inside ggen.

---

## 3. Out-of-repo owner

| Role | Owner |
|------|-------|
| Primary implementation + courts | **`seanchatmangpt/smalt`** |
| Hyperscaler adapter membrane | `smalt/posh` (Go) — when armed |
| Economic-ops FinOps / b4b modules | `smalt` (`lib/smalt/fin_ops`, `docs/b4b`, `lib/smalt/erp`) |
| ggen | standing ledger + optional pack manufacture only — **no ambient DO** |

Sibling tip was **not** pinned by a local `git rev-parse` (repo not mounted).
Remote observation only: README standing still reports PUBLISH-READY **NO —
TYPED-BLOCKED**; azure/aws/gcp/twilio parity receipts observed as
**UNVERIFIED** / **PENDING** coordinator fill (e.g.
`docs/aws-parity/RECEIPT-v26.11.14.md`, `docs/gcp-parity/GCP_WIRE_PARITY.md`,
`docs/azure-parity/AZ2.md`, `docs/twilio-parity/KD-TWILIO.md`). That is
**smalt-local UNKNOWN**, not ggen PARTIAL.

---

## 4. Next exact verifier

Run **in a mounted `seanchatmangpt/smalt` checkout** (not in ggen):

```text
cd /path/to/smalt
just doctor
mix test test/aws
mix test test/gcp
mix test test/fin_ops_calculator_test.exs
# Azure column existence + pending mix cells per AZ2 / azure-gate receipt:
#   docs/verification/azure-gate-2026-11-14.md
# Twilio column (when court recipe is named by coordinator):
#   mix test test/twilio   # or the exact just recipe recorded in TS* receipts
```

Promotion rule for a future ggen standing refresh:

1. Paste the real `mix test` summary lines (exit 0, counts) into the smalt
   parity receipts with a pinned smalt HEAD SHA.
2. Cite that receipt path + SHA from this ledger.
3. Only then may the crown move off `UNKNOWN`/`BLOCKED` — and only to the
   standing the **smalt** receipt actually earns (`PARTIAL_ALIVE` or `ALIVE`).

Until step 1–2 happen, any ggen claim of RAID / economic-ops `ALIVE` is
`REFUSED_UNGROUNDED`.

---

## 5. GA falsifier (2027-01-01)

The GA claim fails if **any** of the following is true:

1. **Missing 4-column court** — Azure, AWS, GCP, or Twilio/fnox column lacks a
   HEAD-pinned green court receipt in `seanchatmangpt/smalt`.
2. **Economic face unproven** — FinOps calculator / admitted b4b plane required
   by the GA economic-ops register has no green court (or remains loud
   `[SKIP]` / KD-OPEN).
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

## 6. Tip reconfirm (2026-11-22)

| Field | Value |
|-------|-------|
| Observed ggen tip | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `5e00619a422a0d8e451029f0aaf8b7a63e27da46` |
| Sibling mount | **not mounted** → courts **BLOCKED** |
| In-repo falsifier `rg` | zero material RAID/4-cloud/economic-ops hits outside standing/rfc docs |
| Crown standing | **UNKNOWN** + **BLOCKED** — **unchanged** (honest; no promotion; never ggen `ALIVE`) |
| Next verifier | §4 (`just doctor`; `mix test test/aws|gcp|fin_ops_calculator_test.exs` in mounted smalt) |

---

## 7. Provenance

Materialized as the v27.1.1 goal-slice standing ledger for **Smalt 4-Cloud RAID
/ economic ops**. Evidence: in-repo ripgrep (empty), ECOSYSTEM-SPEC Gate-0 /
Plane-5 doctrine, and read-only observation of `seanchatmangpt/smalt` parity +
FinOps/b4b trees. No smalt court was executed from this workspace. Honesty tip
pin 2026-11-22 on `5e00619a4` (`cursor/v2711-ccs-ops-honesty-bc54`) — standing
labels held (**UNKNOWN**+**BLOCKED**).
