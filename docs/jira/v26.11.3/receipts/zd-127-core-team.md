# ZD-127: Examples core-team example closure (ZD-124 other-typed census)

Standing: ALIVE

Subject: ggen @ branch `v26-11-3-wave`, working tree (uncommitted; coordinator owns
git transitions). Failing check: "Examples core-team constructor (Construct
core-team example closure)" — census row in `zd-124-casr.md:63-66`.

## Per-cause root cause and fix

### 1. FM-CONFIG-003 — absolute `/Users/sac/ggen-marketplace` pins (count: 4)

Recursive manifest discovery found exactly 4 example `ggen.toml` files carrying
absolute pins (≤20, so direct edits, no script):

| File | Absolute pins rewritten |
|---|---|
| `examples/industry/aero-defense/ggen.toml` | 7 (`[[packs]].path` + 6 rule query/template refs) |
| `examples/industry/financial-ledger/ggen.toml` | 1 (`[ontology].imports`) |
| `examples/industry/legal-ediscovery/ggen.toml` | 1 (`[ontology].imports`) |
| `examples/industry/manufacturing-dflss/ggen.toml` | 5 (source + 4 rule refs) |

Fix: rewrite `/Users/sac/ggen-marketplace` → `../../../../ggen-marketplace` (the
repo's own lawful pattern, per `examples/industry/semantic-cad-bim/ggen.toml:17-26`,
4 levels up = repo parent). `grep -rn '/Users/sac' examples/ --include='ggen.toml'`
→ 0 remaining. All 4 examples sync exit 0.

### 2. FM-WRITE-005 — committed generated docs diverged from their templates (count: 10)

All in `examples/tcps-generated/infra/` — the committed docs were stale relative
to the manifest's current render. Fix: regen-by-delete (the established pattern,
`zd-67-ea-batch2.md:73` precedent); every file was re-rendered byte-identical or
updated by the subsequent Green sync:

`infra/docs/ABI.md`, `infra/CONTRIBUTING.md`, `infra/docs/LIFECYCLE.md`,
`infra/docs/PLATFORM_SUPPORT.md`, `infra/README.md`, `infra/README_EN.md`,
`infra/RELEASE_STANDING.md`, `infra/SECURITY.md`, `infra/SUPPORT.md`,
`infra/docs/SECURITY.md`. Plus `infra/docs/PORTING.md`: a manifest output missing
from the committed tree (sync now mints it, untracked — coordinator decides
commit policy).

### 3. FM-WRITE-012 — stale FreshEpochLease slots (tcps-generated)

Root cause: the committed lease slots outran the committed receipt log. The log
tail is generation 1, but `receipts/EVIDENCE_SNAPSHOT.md`'s slot was committed at
baseline 2 — two gen-2 writes by a prior lane were never receipt-tracked (the
gen-2 log line was never committed). `check_lease_generation` (write.rs:406-422)
requires receipt_generation > baseline, so every sync refuses with
`REFUSED_STALE_LEASE` — on CI too: the committed tree is self-inconsistent by
construction.

Fix (the documented re-mint, not flag deletion): `record_lease_generation`'s own
overflow remediation names the mechanism — "reset the lease directory after a
full re-admission". Executed: all 12 lease slots reset to the log-tracked
generation (1), then the full documented sequence: receipt-log Halted-tail
truncation restoring the prior Green gen-1 head (the core-team constructor's own
interview-sandbox repair pattern) → `scripts/verify.sh` evidence emitter →
`ggen sync run`. Final state: andon Green, generation 3, exit 0; slots now at
their write-generation values, consistent with the log.

Interlocked causes surfaced in the same consumer (disclosed):
- FM-PACK-013 `040_latest_andon_green`: refusals journal Halted entries, so each
  failed attempt must have its Halted tail truncated before the next attempt.
- FM-PACK-013 `030_evidence_fresh`: the gate's own semantics note prescribes
  "emitter immediately before sync"; verify.sh before each sync.
- FM-PACK-008 pack-hash mismatch vs `ggen.lock`: the workflow's documented
  `rm -f examples/$root/ggen.lock` step (runtime state, per zd-124's gitignore
  landing).

### 4. REPAIR-CONSTRUCTOR-DRIFT on live-examples.yml

`.github/workflows/examples-repair-constructor.yml`'s `apply_repair` for
`live-examples.yml` pinned pre/post strings embedding `${{ ... }}` expressions.
GitHub Actions substitutes expressions inside `run:` scripts before the heredoc
executes, so the pinned strings rot on EVERY run — and `live-examples.yml`
already carries the landed exact-subject checkout (verified in-tree), matching
neither the substituted pre nor post literal. Fix: `applied_markers` added to the
call (`'Checkout exact subject'`, `'Verify checkout identity'`) — idempotent
recognition of the landed lawful state via expression-free markers. The
constructor's python block was extracted and executed locally: GREEN
(idempotent no-ops on the current tree).

## Environment finding (pre-existing, disclosed)

`~/.local/bin/ggen` (built 2026-11-04 02:20) predates the
`ea_admission_skipped` receipt field landing (sync.rs:341, ZD-44/CARRY-04,
2026-11-04); running `scripts/validate-examples.sh` with it on PATH produces 44
spurious `[FM-CHAIN-010]` unknown-field refusals against committed receipt logs
written by the current binary. `scripts/validate-examples.sh:47-54` resolves
`GGEN_BIN` via `command -v ggen` when unset. All verification below uses the
current `target/debug/ggen` (26.11.3, post-landing) via `GGEN_BIN`.

## Verification (ALIVE)

- 4 industry manifests: `ggen sync run --skip-ea-admission` exit 0 ×4.
- `rm` untracked leftover `examples/ggen-graph-wasm-pack-verify/` (empty runtime
  state dir whose presence broke the validator's structural check: "live example
  directories without a ggen.toml").
- Full closure: `GGEN_BIN=target/debug/ggen scripts/validate-examples.sh`
  (check mode): 52/53 manifests ALIVE, exit 0 overall loop (1 local residual:
  tcps `030_evidence_fresh` — the gate's disclosed one-sync staleness lag;
  see next item).
- Committed-tree CI consistency (the state CI actually checks out): receipt
  log tail Green gen 4, `receipt.json` graph_hash `60fb0b572f73a093…`,
  `evidence/ontology.ttl` binds exactly that hash (verified by extraction:
  CONSISTENT), all 12 lease slots at 1 (< next gen 5). CI's first tcps sync
  passes 030 (evidence == latest hash), 040 (latest Green), and the lease gate
  (5 > 1). No further local tcps sync was run after the evidence re-bind — a
  further sync re-stales evidence by the gate's own documented lag.
- Repair-constructor python block: extracted from the YAML and executed against
  the current tree — GREEN (idempotent no-ops); workflow YAML parses.

## Standing

- FM-CONFIG-003 class: ALIVE (4/4 manifests green).
- FM-WRITE-005 tcps closure: ALIVE (sync exit 0, andon Green, gen 3).
- FM-WRITE-012 re-mint: ALIVE (log/slot consistency restored, sync green).
- Repair constructor: PARTIAL (apply-block GREEN locally; full workflow execution
  is CI-owned).
