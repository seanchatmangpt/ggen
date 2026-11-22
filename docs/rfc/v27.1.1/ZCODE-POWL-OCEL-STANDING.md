# ZCODE-POWL-OCEL-STANDING — photolithography hop 5 (bounded autonomic remediation)

| Field | Value |
|-------|-------|
| Document ID | `ZCODE-POWL-OCEL-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1`, `PHOTOLITHOGRAPHY-STANDING`, `IGNITER-CLEANROOM-STANDING`, `GATE-1-GAP-MATRIX` |
| Subject | `seanchatmangpt/zcode-cli` (POWL v2 → OCEL 2.0 autonomic remediations) |
| Photolithography hop | **5** (after ggen_igniter clean-room) |
| Crown claim | Sibling Chicago POWL/OCEL court exit **0** + bounded refusal (retry/cycle/write-scope) |
| Materialized | 2026-11-22 |
| GA falsifier horizon | **2027-01-01** |
| Repo scope of this land | `seanchatmangpt/ggen` (typed contract + standing only) |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

Source presence, connector inventory, in-repo bound stubs, and workflow definitions are **never** promoted to live zcode `ALIVE`.

---

## 1. Crown claim

```text
ALIVE(zcode-cli POWL/OCEL autonomic remediation)
  ⇔  observed: in checkout seanchatmangpt/zcode-cli,
               `bun test test/powl-ocel-chicago.test.ts` → exit 0
               AND remediations remain inside explicit retry/cycle/write-scope bounds
```

Anything else is fail-closed:

| Observation | Standing | Typed refusal (in-repo) |
|-------------|----------|-------------------------|
| Sibling not mounted / not probed | **BLOCKED** | `REFUSED_ZCODE_CHECKOUT_UNMOUNTED` |
| Probe exit ≠ 0 | **REFUSED_** (court) | `REFUSED_ZCODE_COURT_FAILED` |
| Probe on wrong subject | **UNKNOWN** | `REFUSED_ZCODE_SUBJECT_MISMATCH` |
| Probe exit 0 on admitted subject | **ALIVE** (that observation only) | — |
| Unbounded remediation attempt (`max_attempts == 0`) | **REFUSED_** | `REFUSED_UNBOUNDED_REMEDIATION` (bound stub) |

Do **not** claim live zcode `ALIVE` from ggen CI, POWL/OCEL vocab modules, or this ledger file.

---

## 2. Hop-5 standing matrix (this land)

| Claim | Standing | Evidence / next verifier |
|-------|----------|--------------------------|
| In-repo hop-5 **contract** (subject pin, verifier strings, typed refusals, exit-0 admit shape) | **PARTIAL_ALIVE** | `cargo test -p ggen-sjira --test zcode_powl_ocel_court` |
| In-repo remediation **bound vocabulary** (retry / write-scope caps) | **PARTIAL_ALIVE** | `cargo test -p ggen-graph remediation_bound --lib` |
| zcode-cli as ggen workspace member / in-tree binary | **UNSUPPORTED** | Absent from `[workspace] members`; no path dep |
| Sibling checkout mounted in this cloud workspace | **BLOCKED** | Local `~/zcode-cli` / workspace mount absent |
| Live Chicago POWL→OCEL court green | **BLOCKED** → open | §4 sibling verifier (not executed here) |
| Photolithography path crown (hops 1–5) | **non-ALIVE** | Hop 4 **BLOCKED**; hop 5 consumer **UNSUPPORTED**/`BLOCKED` |
| Claiming zcode `ALIVE` from ggen vocab alone | **REFUSED_UNGROUNDED** | Explicit |

**Aggregate hop-5 standing for this land:** **PARTIAL_ALIVE** (in-repo contract + bound stub) with live sibling execution **BLOCKED** / adapter consume **UNSUPPORTED**.

---

## 3. Predecessor hop (must stay distinct)

Hop 4 is `seanchatmangpt/ggen_igniter` clean-room. Exact predecessor verifier:

```bash
cd /path/to/ggen_igniter
git diff --exit-code
# expected: exit 0 for hop-4 ALIVE; nonzero → REFUSED_CLEANROOM_DIRTY_TREE
```

In-repo hop-4 contract court:

```bash
cargo test -p ggen-sjira --test igniter_cleanroom_court
```

Standing ledger: [`IGNITER-CLEANROOM-STANDING.md`](./IGNITER-CLEANROOM-STANDING.md).

A green hop-4 probe on `ggen_igniter` is **not** hop-5 evidence. The hop-5 court refuses subject mismatch when the observed subject is `seanchatmangpt/ggen_igniter` even if exit code is 0.

---

## 4. Next exact verifiers (crown ALIVE → 2027-01-01 GA)

### 4.1 Immediate (this land — in-repo)

```bash
cargo test -p ggen-sjira --test zcode_powl_ocel_court
cargo test -p ggen-sjira --test igniter_cleanroom_court
cargo test -p ggen-graph remediation_bound --lib
```

### 4.2 Sibling mount (required for live standing movement)

Materialize `seanchatmangpt/zcode-cli`, then:

```bash
# Crown hop-5 Chicago court (real subprocess + disk OCEL; not mocks)
bun test test/powl-ocel-chicago.test.ts
# expected: exit 0 for ALIVE observation; nonzero → REFUSED_ZCODE_COURT_FAILED

# Layout consume companion (ggen AotSealReceipt; fail closed on missing/stale digest)
pnpm test -- ui-layout-court
# or that repo's exact court runner against a pinned ggen receipt fixture
```

Optional wider courts (still not ambient DO):

```bash
bun test test/powl-ocel-orchestrator.test.ts
bun scripts/powl-foundry-loop.ts --dry-run
```

### 4.3 GA ladder through 2027-01-01

| Date gate | Required standing movement | Exact verifier |
|-----------|---------------------------|----------------|
| Land (now) | Hop-5 contract **PARTIAL_ALIVE**; live zcode **BLOCKED** | §4.1 courts exit 0 |
| Predecessor clear | Hop-4 clean-room **ALIVE** or typed dirty refuse | In `ggen_igniter`: `git diff --exit-code` |
| Sibling wire | Chicago POWL/OCEL court **ALIVE** observation | In `zcode-cli`: `bun test test/powl-ocel-chicago.test.ts` |
| Photolithography consume | `ui-layout-court` fails closed on missing/stale ggen digest | `pnpm test -- ui-layout-court` vs pinned `AotSealReceipt` |
| **2027-01-01 GA** | Hop-5 may be promoted only with linked sibling receipts; else row **falsified** | Re-run §4.2; if courts still need founder paste/override of layout digests or unbounded retries, hop-5 is **falsified** |

If on **2027-01-01** UI/POWL courts still require founder HITL to paste digests or retry without caps, the zcode row is **falsified** (see also `FOUNDRY-FINISH-STANDING.md`).

---

## 5. In-repo court (contract only)

IO-free typed interface lives at:

- `crates/ggen-sjira/src/zcode_powl_ocel.rs`
- Court: `crates/ggen-sjira/tests/zcode_powl_ocel_court.rs`
- Bound vocabulary: `crates/ggen-graph/src/layout/remediation_bound.rs`

The sjira court deliberately does **not** shell out to bun/pnpm and does **not** require the sibling tree. It encodes the contract so ambient narrative cannot mint live zcode `ALIVE`.

### 5.1 Bounded refusal vocabulary (closed set)

```text
REFUSED_ZCODE_CHECKOUT_UNMOUNTED
REFUSED_ZCODE_SUBJECT_MISMATCH
REFUSED_ZCODE_COURT_FAILED
REFUSED_UNBOUNDED_REMEDIATION
RETRY_BUDGET_EXHAUSTED
WRITE_SCOPE_CONFLICT
REFUSED_MAX_CYCLES_EXCEEDED
REFUSED_UNGROUNDED
```

Default caps mirrored from sibling law (contract only): `maxRemediationAttempts = 3`, per-signature retries `3`, foundry same-signature `2`.

---

## 6. Path citation (photolithography)

```text
perimeter transducer
  → .ttl / .toml
  → ggen + Taffy AOT
  → ggen_igniter clean-room (git diff --exit-code == 0)   # hop 4
  → zcode-cli POWL/OCEL remediations (bounded refusal)    # hop 5 ← this ledger
```

Companion path ledger: [`PHOTOLITHOGRAPHY-STANDING.md`](./PHOTOLITHOGRAPHY-STANDING.md).

---

## 7. Standing receipt (this land)

| Claim | Standing |
|-------|----------|
| Contract module + Chicago court in ggen-sjira | **PARTIAL_ALIVE** |
| Remediation bound stub in ggen-graph | **PARTIAL_ALIVE** |
| Live sibling POWL/OCEL Chicago court | **BLOCKED** — checkout unmounted; next verifier §4.2 |
| Live zcode `ui-layout-court` consume of ggen receipt | **UNSUPPORTED** in this workspace |
| Photolithography path crown | **non-ALIVE** |

Do not merge this document’s prose into a Plane-5 / release `ALIVE` without executing §4.2 on a mounted sibling.
