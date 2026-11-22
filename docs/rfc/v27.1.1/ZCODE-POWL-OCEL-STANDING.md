# ZCODE-POWL-OCEL-STANDING — autonomic remediations with bounded refusal

| Field | Value |
|-------|-------|
| Document ID | `ZCODE-POWL-OCEL-STANDING` |
| Companion | `ECOSYSTEM-SPEC-27.1.1`, `GATE-1-GAP-MATRIX` |
| Materialized | 2026-11-22 |
| Repo of record for this land | `seanchatmangpt/ggen` |
| Execution owner | **zcode-cli** (sibling; not a workspace member) |

Standing vocabulary: `UNKNOWN` | `PARTIAL_ALIVE` | `ALIVE` | `BLOCKED` | `BUILD_BROKEN` | `UNSUPPORTED` | `REFUSED_*`.

This document is an evidence ledger + contract fence. Connector-visible sibling
source is never promoted to `ALIVE` for the out-of-repo CLI boundary.

---

## 1. Subject

Gate-1 / Plane-4 claim under audit:

> **zcode-cli** drives hands-free `CONSTRUCT` remediations via POWL v2 ASTs and
> IEEE OCEL 2.0 telemetry, with **explicit bounded refusal** (retry / cycle /
> write-scope caps) — never unbounded autonomic rewrite.

Primary sibling surfaces (observed on `seanchatmangpt/zcode-cli` default tip
`fccef5d3a8927d8e7fbff99e8a5ccacd1d721a89`, 2026-11-22 connector read):

| Path | Role |
|------|------|
| `src/powl-ocel-orchestrator.ts` | POWL v2 AST → Workflow-Net → token replay → OCEL 2.0; `runAutonomicLoop` |
| `src/adapters/foundry-loop.ts` | Gall spine: courts + transactional rollback + `deriveFollowUpPowl` |
| `src/adapters/ui-layout-court.ts` | Taffy/Playwright layout court; `maxRemediationAttempts` default **3** |
| `scripts/powl-foundry-loop.ts` | Dry-run foundry driver (`--dry-run` default; live not ambient) |
| `test/powl-ocel-chicago.test.ts` | Chicago zero-mock: real subprocess + disk OCEL SHA-256 + anti-vacuity |
| `test/powl-ocel-orchestrator.test.ts` | Unit/orchestrator courts |
| `docs/ecosystem-spec-27.1.1.md` | Sibling copy of ECOSYSTEM-SPEC; R-06 marked CLOSED for bounded remediation |

In **this** repo: no `zcode-cli` workspace member; no `powl-ocel-orchestrator.ts`;
no executable foundry loop. ggen OCEL (`crates/ggen-graph/src/ocel/`) and RWR
autonomic controllers are **adjacent process-evidence / MAPE-K rails**, not the
zcode CLI remediation engine.

---

## 2. Standing matrix (ggen fence)

| Claim | Standing | Why |
|-------|----------|-----|
| zcode-cli is a ggen workspace member / in-tree binary | **UNSUPPORTED** | Absent from `[workspace] members`; no path dep |
| Sibling source for POWL/OCEL orchestrator exists (connector) | **PARTIAL_ALIVE** | GitHub contents + SHA above; not a mounted tree |
| Autonomic remediation with **bounded refusal** (contract prose) | **PARTIAL_ALIVE** | Bounds enumerated below from sibling source; in-repo stub mirrors vocabulary only |
| Out-of-repo CLI executes Chicago POWL→OCEL court green | **UNKNOWN** | Not executed in this workspace (sibling not checked out) |
| Photolithography consume of ggen `AotSealReceipt` via `ui-layout-court.ts` | **UNSUPPORTED** → open | Adapter exists in sibling; ggen↔zcode receipt wire not receipted here |
| Promote sandbox / dry-run / README claims to `ALIVE` | **REFUSED_UNGROUNDED** | Explicit |

Do **not** claim `ALIVE` for zcode-cli POWL/OCEL remediations from ggen CI alone.

---

## 3. Bounded refusal (must be explicit)

Autonomic remediations are lawful only inside named bounds. Crossing a bound is
a typed **refusal**, not a silent continue and not an ambient `DO`.

### 3.1 Bound table (from sibling source + Gate-1 bridge stub)

| Bound | Default / law | Refusal when exceeded | Owner surface |
|-------|---------------|----------------------|---------------|
| Max remediation attempts (layout court budget note) | `maxRemediationAttempts = 3` (`DEFAULT_MAX_REMEDIATION_ATTEMPTS`) | Court records `max_remediation_attempts=N` on `REFUSED`; foundry loop owns cycle halt | `ui-layout-court.ts` |
| Max retries per failure signature | `SignatureRetryLimiter` default **3** (`maxRetriesPerSignature`) | `RETRY_BUDGET_EXHAUSTED` → OCEL standing `REFUSED` + `retry_budget_exhausted` event | `powl-ocel-orchestrator.ts` |
| Foundry same-signature retries | `maxSameSignatureRetries` default **2** | `retryBudgetExhausted = true`; loop breaks | `foundry-loop.ts` |
| Autonomic cycle cap | `maxCycles` (required for bounded tests; omit only under documented budget) | Loop stops; optional `ComplianceRefusalError("RETRY_BUDGET_EXHAUSTED")` | `runAutonomicLoop` / `runFoundryLoop` |
| Write-scope disjointness | Unordered mutating leaves must not overlap scopes | `WRITE_SCOPE_CONFLICT` throw / fail-closed validate | `validateDisjointWriteScopes` / `validatePowlTree` |
| Dirty timeout / rollback | Snapshot + Saga LIFO | `DIRTY_TIMEOUT_REFUSED` / `ROLLBACK_FAILED` → standing `REFUSED` | transactional net |
| State-space soundness overflow | Murata reduction; `MAX_STATES` truncation | Standing **`UNKNOWN`** (never fake SOUND) | `verifyStructuralSoundness` |
| Clean-room noop stubs | `["true"]` sync argv | **LOUD_SKIP** — never fake `ALIVE` | `ui-layout-court.ts` |
| Sandbox / missing DO seals | `sandbox: true` | Aggregate demoted to **PARTIAL_ALIVE** at most | `foundry-loop.ts` |

### 3.2 Explicit refusal codes (closed set for this standing)

```text
WRITE_SCOPE_CONFLICT
RETRY_BUDGET_EXHAUSTED
DIRTY_TIMEOUT_REFUSED
ROLLBACK_FAILED
REFUSED                 (OCEL object attribute / court standing)
SUBJECT_MISSING         (exit 2 — binary/subject absent; not a geometry refuse)
LOUD_SKIP               (exit 3 — disabled / noop; not ALIVE)
UNKNOWN                 (truncated soundness — not SOUND, not ALIVE)
REFUSED_UNGROUNDED      (ggen fence: claim without out-of-repo verifier)
REFUSED_UNBOUNDED_REMEDIATION  (in-repo stub: max_attempts == 0 or missing bound)
```

Any remediation path that retries without one of the numeric caps above is
**REFUSED_UNBOUNDED_REMEDIATION** at the Gate-1 fence.

### 3.3 Authority separation

```text
SELECT ≠ CONSTRUCT ≠ DO
```

POWL/OCEL remediations in zcode-cli are **CONSTRUCT** (edit admitted
`.ttl`/`.toml`/templates, re-verify courts, emit OCEL + receipts). They do **not**
grant Plane-5 / BRCE+WebAuthn AS-01 `DO`. Dry-run is the default foundry posture
(`scripts/powl-foundry-loop.ts`).

---

## 4. In-repo Gate-1 stub (non-ALIVE)

`ggen_graph::layout::remediation_bound` mirrors the bound vocabulary as a typed
refusal enum + constants so ggen photolithography consumers can name the same
caps without importing zcode-cli.

Standing of the stub itself: **PARTIAL_ALIVE** (compiles + unit court on refuse
of unbounded attempts). Standing of zcode execution remains **UNKNOWN** /
**UNSUPPORTED** until the out-of-repo verifier below exits 0.

---

## 5. Next exact verifier (out-of-repo CLI)

Materialize sibling checkout, then run the Chicago POWL/OCEL court (real
subprocess + filesystem; not mocks):

```text
# From a mounted seanchatmangpt/zcode-cli tree at a pinned SHA:
bun test test/powl-ocel-chicago.test.ts
```

Optional wider orchestrator court:

```text
bun test test/powl-ocel-orchestrator.test.ts
```

Optional dry-run foundry spine (must not claim ALIVE under `sandbox: true`):

```text
bun scripts/powl-foundry-loop.ts --dry-run
```

Acceptance for promoting the **autonomic bounded-remediation** claim toward
`ALIVE` (still only inside zcode-cli evidence, never from ggen alone):

1. `powl-ocel-chicago` exits 0 with fitness 1.0 + byte-exact OCEL SHA-256 on disk.
2. Anti-vacuity case refuses (fitness &lt; 1) and emits a remediation follow-up POWL id.
3. A deliberate unbounded attempt (`maxRemediationAttempts = 0` or exhausted
   signature budget) surfaces one of the explicit refusal codes in §3.2 — not a
   green `ALIVE` receipt.

Until that boundary executes in a mounted tree, ggen standing stays
**UNSUPPORTED** (execution) / **PARTIAL_ALIVE** (contract + connector inventory).

---

## 6. Replay of this ggen land

```text
test -f docs/rfc/v27.1.1/ZCODE-POWL-OCEL-STANDING.md
cargo test -p ggen-graph remediation_bound --lib
```

Falsifier: any commit that claims zcode POWL/OCEL remediations `ALIVE` without
the §5 sibling court receipt is **REFUSED_UNGROUNDED**.
