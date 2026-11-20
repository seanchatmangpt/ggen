# Wave Ledger v26.11.3

Frozen cross-repo wave ledger for milestone v26.11.3. Ground-truth facts
captured live 2026-11-04T14:13:48Z (rebuild after staleness: HEAD moved in
ggen, cgap, arcus, lsp-max and ggen-marketplace since the 72b743d23 build;
claude-code-config-lsp unchanged at 6a04124). ggen HEAD moved once more
mid-write (58a3b410 → 9c1cf48b, a runbook commit touching no receipts; STEP 1
identical, file count 135 before and after — falsifier 1 fired and was
refreshed, same class as the prior build's mid-write catch). **ggen's tree is
dirty pre-sweep — 244 modified/untracked paths as-of-disk at capture time.
Receipt counts and Step-2 tables for ggen are as-of-disk, not as-of-HEAD; flag
this before citing any ggen count downstream.** Step-2 tables are generated from a
mechanical extraction (no hand transcription). Re-run the freeze commands
before citing any count; receipt-cited counts are stale by default.

## Step 0 — Ground-Truth Freeze Facts

Captured with `git branch --show-current`, `git rev-parse HEAD`,
`git tag -l '*26.11.3*'`, `ls docs/jira/v26.11.3/receipts | wc -l`, and an
existence check for a RECEIPTS.md index.

| repo | branch | HEAD | tags | receipts |
|---|---|---|---|---|
| ggen | v26-11-3-wave | 9c1cf48b90b2d24dc330bafb51c26b87eed4de3f | none | 136 |
| cgap | main | b6d685177d6d62b95708b75627fc1b617265a82d | none | 77 |
| arcus | main | 70477e938c7693226331abe3296f4316c77a38e0 | none | 31 |
| lsp-max | master | e23629b8e6bd531444e897c73d2d103dcc7d3d5c | none | 16 |
| claude-code-config-lsp | main | 6a041243aa0c1474ed63e52d6fb0c899dde24fdf | none | 70 |
| ggen-marketplace | main | 808c621c4cb3ce5f394a0dad7b9bee46c59c9ff5 | none | 12 |

RECEIPTS.md index status: ggen, arcus and ggen-marketplace carry
`docs/jira/v26.11.3/RECEIPTS.md` (present; no repo-root copy). cgap, lsp-max
and claude-code-config-lsp have no RECEIPTS.md — loud-MISSING, never silently
skipped. (Changed since last build: ggen-marketplace gained an index; cgap is
the remaining largest gap at 77 receipts.)

No repo carries a `*26.11.3*` tag.

## Step 1 — Standing Summary (raw `uniq -c` pasted verbatim)

First-token-per-receipt scan:
`grep -m1 -oE '\b(ALIVE|PARTIAL|BLOCKED|REFUTED|UNVERIFIED|PASS|SIM-PASS|FAIL-honest)\b'`
per receipt file, sorted and counted. Raw output, unpasted-cleaned:

### ggen (136 receipts)

```
  91 ALIVE
   6 BLOCKED
   8 PARTIAL
  14 PASS
   1 REFUTED
   3 UNVERIFIED
```

### cgap (77 receipts)

```
  49 ALIVE
   6 BLOCKED
   4 PARTIAL
  10 PASS
   2 REFUTED
  6 UNVERIFIED
```

### arcus (31 receipts)

```
  19 ALIVE
   3 BLOCKED
  13 PASS
```

### lsp-max (16 receipts)

```
   4 ALIVE
   1 BLOCKED
   4 PARTIAL
   3 PASS
   1 UNVERIFIED
```

### claude-code-config-lsp (70 receipts)

```
  51 ALIVE
   7 BLOCKED
   1 FAIL-honest
   6 PARTIAL
   5 PASS
   3 UNVERIFIED
```

### ggen-marketplace (12 receipts)

```
  14 ALIVE
```

## Step 2 — Top-10 Load-Bearing Receipts (by size, `ls -S | head -10`)

Format: filename | first non-blank body line | standing token (first token
matched anywhere in the file). NONE means the receipt carries no standing token
under this scan — standing-UNDETERMINED, a finding, not a paste error. Where a
receipt opens with a bare standing token line, the title is taken from the next
non-blank line (falsifier 2).

### ggen

| file | first non-blank body line | standing |
|---|---|---|
| sa-13.md | # SA-13: pack.toml mechanical-conformance pass (ggen-marketplace) | NONE |
| cl-close-2.md | # cl-close-2 — index-only close lane receipt (CL-CLOSE-2, full new-wav | PASS |
| zd-14-ci-final.md | # ZD-14: CI Final Fixes (fmt, census, repair idempotency, EA graph, bo | PARTIAL |
| sa-17-census.md | # SA-17 Fleet-Wide Skill-Conformance Census | BLOCKED |
| k8-cli-flag-depgraph.md | # Receipt: k8 — CLI `--skip-ea-admission` flag + Stage 2c depgraph fai | ALIVE |
| zd-48-fm-codes.md | # zd-48 — FM-code census triage + 8 Chicago courts (CARRY-09, GAP-3 re | ALIVE |
| sa-12.md | # SA-12 — SKILL-APPLICATION: wasm4pm-compat ggen/ conformance to run-g | PASS |
| ds-2.md | # DS-2 Receipt: complete.ex -> src/complete.rs (PG-2 Slice 4) | NONE |
| zd-67-ea-batch2.md | # ZD-67: EA Admission Graphs, Batch 2 (ZB-13 items 3-4) | ALIVE |
| ds-7-mutation.md | # DS-7: sjira mutation battery (C05: mutation-testing the backlog) | ALIVE |

### cgap

| file | first non-blank body line | standing |
|---|---|---|
| af-43-cgap-publish.md | # AF-43 Receipt: PUB-2 six-blocker publish unblock | ALIVE |
| ab-6-header.md | # AB-6 Receipt: AOQ1 Header Kernel (W1-5a) | ALIVE |
| af-10-cbmqs.md | # AF-10 — CBMQS Seven-Gate Evaluator (v26.11.3) | PASS |
| ab-4-aead.md | # Receipt: AB-4 AEAD/Poly1305 kernel (PG-A3, AoQ Wave-1 W1-3b) | NONE |
| rx-2-conformance.md | # RX-2 — RFC-0089 conformance suite: 5-injection court | ALIVE |
| rx-3-scorecard.md | # RX-3 — RFC-0089 §6 Scorecard (measured) | NONE |
| ab-15-ffi-stress.md | # AB-15 — FFI Stress Receipt (Rust + Python, one receipt, both sides) | ALIVE |
| zd-6-w2a-session-ffi.md | # Receipt: W2-A — cgap-ffi session C-ABI surface (zd-6, wave 2) | ALIVE |
| af-37-p99-torture.md | # AF-37 Receipt: the 5-injection p99 torture bench | ALIVE |
| ab-1.md | # AB-1 Receipt — cgap-aoq skeleton (AoQ Wave-1 W1-1) | ALIVE |

### arcus

| file | first non-blank body line | standing |
|---|---|---|
| af-28-reindex.md | # AF-28 Receipt — macro-benchmark: supply-chain sudden-stop re-index | ALIVE |
| a1-ea-graph.md | # Receipt: A1 — arcus EA graph leg of WO-08 (ggen-native authoring) | NONE |
| af-31-compression.md | # AF-31: Benchmark 01 — the 80-day compression, measured honestly | ALIVE |
| ga-6-arcus-flight.md | # GA-6: arcus flight ggen projection (PG-A9 PART A step 3) | PASS |
| af-15-decoy-rotation.md | # AF-15 decoy rotation (Prompt 05 claim → measured receipt) | ALIVE |
| ga-4-aoq-court.md | # GA-4: aoq-pack pack.toml + 9-gate shim court | BLOCKED |
| ga-8-arcus-register.md | # ga-8-arcus-register | ALIVE |
| zd-42-w2d-admission.md | # ZD-42 / W2-D — Admission Path Receipt | ALIVE |
| bm-1-ggen-benchmark.md | # BM-1 ggen Benchmark | ALIVE |
| af-30-cage-match.md | # AF-30 Receipt — The Cage Match (macro-benchmark, executed honestly) | PASS |

### lsp-max

| file | first non-blank body line | standing |
|---|---|---|
| lm-1.md | # lm-1 — IMPLEMENTATION lane LM-1, Wave 2 (G2–G6) | PARTIAL |
| cl-e-server-handle.md | # CL-E — ServerHandle typed child-death failure (IMPLEMENTATION lane r | ALIVE |
| cl-f-eviction.md | # cl-f-eviction — M8 dead-child eviction + no-respawn law (IMPLEMENTAT | PARTIAL |
| af-20-pool-drop.md | # af-20-pool-drop — pool-drop child teardown (IMPLEMENTATION lane AF-2 | PARTIAL |
| cl-g-priority.md | # CL-G Receipt — Priority-string validation (M9 hazard fix) | NONE |
| ab-17-churn-stress.md | # AB-17 — Child-Churn Kill-Stress Court (lsp-max-compositor) | ALIVE |
| zd-91-broken-targets.md | # ZD-91: Broken test targets (ZD-34 pre-existing compile breaks) — rec | ALIVE |
| sa-11.md | # SA-11 — skill-application lane: lsp-max-scaffold vs run-ggen skill | NONE |
| cl-h-end-position.md | # CL-H: Diagnostic range end preserved through compositor relay | PASS |
| cl-n-flake.md | # Receipt: cl-n-flake — crash_isolation load flake triage | PARTIAL |

### claude-code-config-lsp

| file | first non-blank body line | standing |
|---|---|---|
| ab-18-relay-stress.md | # AB-18 — Compositor relay stress (STRESS lane receipt) | ALIVE |
| p12-wiring.md | # P12 — provider wiring receipt (integration wave input) | FAIL-honest |
| c5-protocol.md | # Receipt: C5 protocol round-trip suite (claude-code-config-lsp) | ALIVE |
| m6-arbitration.md | # M6 — Request Arbitration Tests | PARTIAL |
| r1-admission.md | # R1 — admission-controller lane receipt | PARTIAL |
| m11-registration.md | # Receipt: M11 — compositor registered as Claude Code front door | ALIVE |
| m1-schema.md | # M1 — Compositor Manifest Schema Extraction + Manifest Fix | PASS |
| cfl-1.md | # CFL-1 Implementation Lane Receipt (G1 + G2) | ALIVE |
| i3-tokens-substrate.md | # I3 — Semantic-Tokens Substrate Fix (generated classifier hand-correc | ALIVE |
| xd-16-relay-standing.md | # XD-16 — Relay-path standing-unverified reds (root-cause + fix receip | ALIVE |

### ggen-marketplace

| file | first non-blank body line | standing |
|---|---|---|
| zd-28-regression-packs.md | # ZD-28: repair the 7 zd-12 regression packs (ggen 26.10.11 court) | ALIVE |
| zd-62-mkt01-adoption.md | # ZD-62 / MKT-01: adopt ambient ggen 26.11.3 corpus-wide with disclose | ALIVE |
| zd-49-gac-registry.md | # ZD-49: gac:Registry individual for graphlaw-ash-capability-pack | ALIVE |
| zd-65-smalt-gates.md | # ZD-65: smalt/pplan inverted gates — positive-style corpus defect clo | ALIVE |
| zd-20-pack-repairs.md | # ZD-20: pack repairs for the 7 ZD-12 REFUSED packs | ALIVE |
| zd-52-warn-typing.md | # ZD-52: WARN/SKIPPED baseline typing (MKT-03) | ALIVE |
| ga-2-aoq-gates.md | # Receipt: ga-2-aoq-gates (PG-A6 step 2, lane GA-2) | ALIVE |
| zd-12-baseline-reconcile.md | # ZD-12: baseline-to-disk reconcile (qualification/baseline.json) | ALIVE |
| zd-85-changelog-touch.md | # ZD-85: changelog touch — append YD-4 stragglers to [v26.11.3] | ALIVE |
| yd-4-marketplace-changelog.md | # YD-4 — Marketplace CHANGELOG v26.11.3 section cut | ALIVE |

## Falsifiers

1. **Staleness diff (STEP 4 self-check)**: freeze facts re-run immediately after
   write; the diff against this file's Step-0 table must be empty. Any non-empty
   diff means HEAD moved or receipts landed mid-write — refresh, never edit stale.
2. **Token contradiction**: a receipt whose first non-blank line is itself a bare
   standing token is counted once, and its title row is taken from the next
   non-blank line. Any receipt counted twice or zero times under this scan
   contradicts the ledger.
3. **Loud-MISSING**: repos without a RECEIPTS.md index (cgap, lsp-max,
   claude-code-config-lsp) are recorded as ABSENT above. A future ledger that
   lists them as present without a path contradicts this one.
4. **Sum-vs-filecount tension (measured finding)**: arcus tokens sum to 35 over
   31 files, claude-code-config-lsp to 73 over 70, ggen-marketplace to 14 over
   12, while ggen sums to 123 over 136 and lsp-max to 13 over 16 — receipts
   carrying no token under this scan. Measured causes: (a) `grep -o` emits one
   output line per match, and `-m1` bounds matching LINES, not outputs — a
   receipt whose first token-bearing line holds two tokens is counted twice; (b)
   token-less receipts (standing-UNDETERMINED, visible as NONE in Step 2) count
   as files but emit no token. The Step-1 `uniq -c` blocks are pasted verbatim
   as evidence of the raw scan; the per-receipt ground truth is the Step-2
   tables. Recorded, not reconciled. (Final refresh: ggen counts include this
   ledger's own refresh receipt, zd-106-ledger-refresh.md — 136 files, 123
   tokens, 13 token-less.)
5. **Rebuild-determinism**: re-running the extraction must reproduce the Step-2
   tables byte-for-byte. A second run differing in any row falsifies the ledger.
6. **Self-check diff must be empty**: the STEP 4 diff is the gate; a ledger whose
   self-check cannot be run or is skipped loudly fails close-out.
7. **Dirty-tree provenance (new this build)**: ggen's Step-1/Step-2 counts are
   as-of-disk over a dirty tree (244 modified/untracked paths, pre-sweep). A
   later ledger or consumer citing ggen counts against a clean post-sweep HEAD
   (9c1cf48b) must re-extract, not diff against this build.
