# ZCODE / POWL bounded-refusal — honesty stall ledger (v27.1.1)

```text
lineage=ALIVE(bounded_refusal tip-cargo @ cd7c2433b ⊆ crown)
exact_tip_reconfirm=STALLED
agents=bc-d4e6c905,bc-934bba30 RUNNING branchName=null
remote_reconfirm_branch=ABSENT
crown_tip=df9597c1817313f8d5173f425af28aaf485d114c
prior_tip_win=c91b225614a3ca0472303c05bab25601db1d51e1 ⊆ crown
```

| Field | Value |
|-------|-------|
| Document ID | `ZCODE-POWL-REFUSAL-STANDING-27.1.1` |
| Honesty sidebranch | `cursor/v2711-zcode-refusal-stall-bc54` (this land; **not** crown) |
| Crown tip observed | `origin/cursor/ecosystem-spec-27-1-1-bc54` @ `df9597c1817313f8d5173f425af28aaf485d114c` |
| Prior tip-win | `cursor/v2711-zcode-powl-refusal-alive-bc54` @ `c91b225614a3ca0472303c05bab25601db1d51e1` — **already ⊆ crown** (`git merge-base --is-ancestor` → true) |
| Lineage ALIVE receipt | [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) (tip blob present) |
| Companion ledger | [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) |
| Cargo this land | **NOT RUN** (docs-only honesty; do not mint new tip-cargo `ALIVE`) |
| ManagePullRequest | **not used** |
| Crown push | **not used** |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BLOCKED_HITL` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*` | `SCHEDULED` | `STALLED`.

**Authority.** This document settles the stalled `zcode POWL refusal reconfirm` agents against tip evidence already on crown. It records lineage `ALIVE` from tip docs + prior tip-win ancestry. It does **not** invent a new exact-tip or hop-5 / foundry `--live` `ALIVE` from RUNNING agents that have not pushed.

---

## 1. Tip claim audit (`docs/rfc/v27.1.1`)

| Artifact on tip `df9597c18` | Claim | Standing held |
|-----------------------------|-------|---------------|
| [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md) | Bounded-refusal tip-cargo **ALIVE** — sjira **6**/bound **4** @ evidence tip `cd7c2433b` | **ALIVE (lineage tip-cargo)** — tip blob present; sources landed via `c91b22561` ⊆ crown |
| [`ZCODE-POWL-OCEL-STANDING.md`](./ZCODE-POWL-OCEL-STANDING.md) | Aggregate hop-5 **PARTIAL_ALIVE**; refuse-path land cites ALIVE receipt | Held; live crown still non-ALIVE |
| Foundry `--dry-run` | **PARTIAL_ALIVE** (exit **2**) | Held |
| Foundry `--live` | **BLOCKED_HITL** / **REFUSED** (`Model access is not configured`) | Held — see [`HITL-LIVE-NEXT-VERIFIER-STANDING.md`](./HITL-LIVE-NEXT-VERIFIER-STANDING.md) |

**Verdict:** tip docs already claim bounded-refusal tip-cargo **ALIVE**. No missing ALIVE receipt to invent. Exact-tip reconfirm of those courts on live crown tip `df9597c18` was the reconfirm agents' job — still open.

---

## 2. Reconfirm agents (observation 2026-11-22)

| Agent | Name | Lifecycle | `branchName` | Remote push of `cursor/v2711-zcode-refusal-reconfirm-bc54` |
|-------|------|-----------|--------------|--------------------------------------------------------------|
| `bc-d4e6c905-2fa8-5da6-b274-104f7fe4fac3` | zcode POWL refusal reconfirm | **RUNNING** | `null` | **ABSENT** (`git ls-remote` / `git fetch` → no remote ref) |
| `bc-934bba30-be78-522e-bedb-318e12f20158` | zcode POWL refusal reconfirm | **RUNNING** | `null` | **ABSENT** |

| Probe | Result |
|-------|--------|
| `git fetch origin cursor/v2711-zcode-refusal-reconfirm-bc54` | **failed** — remote ref missing |
| Local leftover `cursor/v2711-zcode-refusal-reconfirm-bc54` | Exists on some workspaces but is **not** a pushed sidebranch and is **not** absorb evidence |
| Closest pushed tip-win | `origin/cursor/v2711-zcode-powl-refusal-alive-bc54` @ `c91b22561` |

**Reconfirm standing:** **STALLED** (agents **RUNNING** without sidebranch push). Do **not** promote RUNNING / local-only branch presence to tip-cargo or hop-5 `ALIVE`.

---

## 3. Standing matrix (this honesty land)

| Claim | Standing | Evidence |
|-------|----------|----------|
| Bounded-refusal tip-cargo (lineage) | **ALIVE** | Tip [`ZCODE-POWL-REFUSAL-ALIVE-STANDING.md`](./ZCODE-POWL-REFUSAL-ALIVE-STANDING.md); prior tip-win `c91b22561` ⊆ `df9597c18` |
| Exact-tip reconfirm on crown `df9597c18` | **STALLED** / was **SCHEDULED** | Agents RUNNING; remote reconfirm branch **ABSENT** |
| New ALIVE from stalled reconfirm agents | **REFUSED_UNGROUNDED** | Explicit antifalsifier |
| Foundry `--live` / hop-5 crown | **BLOCKED_HITL** / **PARTIAL** | Unchanged; not this land's subject |
| Absorb of `cursor/v2711-zcode-refusal-reconfirm-bc54` onto crown | **UNSUPPORTED** until remote exists with EXIT:0 receipt | Early-settle already recorded ABSENT remote |

---

## 4. Exact next verifier (reconfirm → clear STALLED)

Replay the tip ALIVE courts on the live crown tip (or a sidebranch whose base is that tip). Pass only on **exit 0** with a pushed evidence branch — not from agent RUNNING alone.

```bash
git fetch origin cursor/ecosystem-spec-27-1-1-bc54
git checkout -B cursor/v2711-zcode-refusal-reconfirm-bc54 origin/cursor/ecosystem-spec-27-1-1-bc54
# tip must be df9597c18+ (or later tip-wins absorb of this honesty land)

SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-reconfirm-target \
  cargo test -p ggen-sjira --test zcode_powl_ocel_court
# expect: 6 passed; 0 failed; exit 0

SCCACHE_DISABLE=1 CARGO_BUILD_JOBS=1 \
  CARGO_TARGET_DIR=/home/ubuntu/zcode-powl-refusal-reconfirm-target \
  cargo test -p ggen-graph remediation_bound --lib
# expect: 4 passed; 0 failed; exit 0

# Then push the reconfirm sidebranch + append EXIT:0 receipt to
# ZCODE-POWL-REFUSAL-ALIVE-STANDING.md (exact-tip reconfirm ALIVE).
# Until then: reconfirm remains STALLED; lineage ALIVE held; no crown mint.
```

**Antifalsifiers**

- Agent status **RUNNING** ≠ tip-cargo `ALIVE`
- Local-only `cursor/v2711-zcode-refusal-reconfirm-bc54` ≠ remote evidence
- Lineage ALIVE @ `cd7c2433b` / `c91b22561` ≠ automatic exact-tip ALIVE on `df9597c18` without the replay above
- Foundry `--dry-run` PARTIAL / `--live` BLOCKED_HITL ≠ refuse-path tip-cargo movement

---

## 5. Settlement for crown operators

1. **Prior tip-win already ⊆ crown** — do not re-absorb `cursor/v2711-zcode-powl-refusal-alive-bc54` @ `c91b22561`.
2. **Reconfirm sidebranch ABSENT** — no absorb of `cursor/v2711-zcode-refusal-reconfirm-bc54`.
3. **Lineage standing held ALIVE** via tip docs; **exact-tip reconfirm STALLED**.
4. This honesty sidebranch only; **no** crown push; **no** ManagePullRequest.

Replay ancestry check:

```bash
git merge-base --is-ancestor c91b225614a3ca0472303c05bab25601db1d51e1 \
  origin/cursor/ecosystem-spec-27-1-1-bc54 && echo 'prior tip-win ⊆ crown'
```
