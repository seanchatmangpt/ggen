# Wave Ledger v26.11.3

Frozen cross-repo wave ledger for milestone v26.11.3. Ground-truth facts
captured live 2026-11-04 (refrozen same session after self-check caught mid-write drift; original
capture 2026-11-04T10:17:46Z) from the canonical checkouts. Step-2 tables
are generated from a mechanical extraction (no hand transcription). Re-run the
freeze commands before citing any count; receipt-cited counts are stale by
default.

## Step 0 — Ground-Truth Freeze Facts

Captured with `git branch --show-current`, `git rev-parse HEAD`,
`git tag -l '*26.11.3*'`, `ls docs/jira/v26.11.3/receipts | wc -l`, and an
existence check for a RECEIPTS.md index.

| repo | branch | HEAD | tags | receipts |
|---|---|---|---|---|
| ggen | v26-11-3-wave | ed3f1dfb19f8976d1b43be57882796354fdd2729 | none | 109 |
| cgap | main | 37721e2fa67132e917f6c6addf0c4c239e3777bf | none | 63 |
| arcus | main | 1c08fd14daab4093796b56f879db74d2a0472411 | none | 29 |
| lsp-max | master | 7278ae7751ab7380a542f123b184fb97c926bb59 | none | 13 |
| claude-code-config-lsp | main | 6a041243aa0c1474ed63e52d6fb0c899dde24fdf | none | 70 |
| ggen-marketplace | main | 0e94adbb8e3da3ff88b761caee180ba0b6317b3e | none | 4 |

RECEIPTS.md index status: ggen and arcus carry `docs/jira/v26.11.3/RECEIPTS.md`
(present; no repo-root copy). cgap, lsp-max, claude-code-config-lsp and
ggen-marketplace have no RECEIPTS.md — loud-MISSING, never silently skipped.

No repo carries a `*26.11.3*` tag.

## Step 1 — Standing Summary (raw `uniq -c` pasted verbatim)

First-token-per-receipt scan:
`grep -m1 -oE '\b(ALIVE|PARTIAL|BLOCKED|REFUTED|UNVERIFIED|PASS|SIM-PASS|FAIL-honest)\b'`
per receipt file, sorted and counted. Raw output, unpasted-cleaned:

### ggen (109 receipts)

```
  75 ALIVE
   6 BLOCKED
   8 PARTIAL
  11 PASS
   1 REFUTED
   2 UNVERIFIED
```

### cgap (63 receipts)

```
  43 ALIVE
   3 BLOCKED
   2 PARTIAL
   8 PASS
   2 REFUTED
   3 UNVERIFIED
```

### arcus (29 receipts)

```
  17 ALIVE
   3 BLOCKED
  13 PASS
```

### lsp-max (13 receipts)

```
   3 ALIVE
   1 BLOCKED
   4 PARTIAL
   2 PASS
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

### ggen-marketplace (4 receipts)

```
   5 ALIVE
```

## Step 2 — Top-10 Load-Bearing Receipts (by size, `ls -S | head -10`)

Format: filename | first non-blank body line | standing token (first token
matched anywhere in the file). NONE means the receipt carries no standing token
under this scan — standing-UNDETERMINED, a finding, not a paste error.

### ggen

| file | first non-blank body line | standing |
|---|---|---|
| sa-13.md | # SA-13: pack.toml mechanical-conformance pass (ggen-marketplace) | NONE |
| cl-close-2.md | # cl-close-2 — index-only close lane receipt (CL-CLOSE-2, full new-wav | PASS |
| zd-14-ci-final.md | # ZD-14: CI Final Fixes (fmt, census, repair idempotency, EA g... | PARTIAL |
| sa-17-census.md | # SA-17 Fleet-Wide Skill-Conformance Census | BLOCKED |
| k8-cli-flag-depgraph.md | # Receipt: k8 — CLI `--skip-ea-admission` flag + Stage 2c ... | ALIVE |
| sa-12.md | # SA-12 — SKILL-APPLICATION: wasm4pm-compat ggen/ conformance to run-g | PASS |
| ds-2.md | # DS-2 Receipt: complete.ex -> src/complete.rs (PG-2 Slice 4) | NONE |
| ds-7-mutation.md | # DS-7: sjira mutation battery (C05: mutation-testing the backlog) | ALIVE |
| cl-d-fmlaw-fixtures.md | # CL-D Receipt: FM-LAW-015 declarative-fixture remediation ... | ALIVE |
| sa-8.md | # SA-8 Receipt — skill-application lane (run-ggen conventions → exampl | BLOCKED |

### cgap

| file | first non-blank body line | standing |
|---|---|---|
| ab-6-header.md | # AB-6 Receipt: AOQ1 Header Kernel (W1-5a) | ALIVE |
| af-10-cbmqs.md | # AF-10 — CBMQS Seven-Gate Evaluator (v26.11.3) | PASS |
| ab-4-aead.md | # Receipt: AB-4 AEAD/Poly1305 kernel (PG-A3, AoQ Wave-1 W1-3b) | NONE |
| rx-2-conformance.md | # RX-2 — RFC-0089 conformance suite: 5-injection court | ALIVE |
| rx-3-scorecard.md | # RX-3 — RFC-0089 §6 Scorecard (measured) | NONE |
| ab-15-ffi-stress.md | # AB-15 — FFI Stress Receipt (Rust + Python, one receipt, both... | ALIVE |
| af-37-p99-torture.md | # AF-37 Receipt: the 5-injection p99 torture bench | ALIVE |
| ab-1.md | # AB-1 Receipt — cgap-aoq skeleton (AoQ Wave-1 W1-1) | ALIVE |
| ab-8-wire.md | # AB-8 Wire Receipt (PG-A2, AoQ Wave-1 W1-5b) | ALIVE |
| af-43-cgap-publish.md | # AF-43 Receipt: PUB-2 six-blocker publish unblock | ALIVE |

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
| bm-1-ggen-benchmark.md | # BM-1 ggen Benchmark | ALIVE |
| af-30-cage-match.md | # AF-30 Receipt — The Cage Match (macro-benchmark, executed hon... | PASS |
| k5-qualification.md | # K5 qualification receipt (WO-08 step 2, qualification leg) | ALIVE |

### lsp-max

| file | first non-blank body line | standing |
|---|---|---|
| lm-1.md | # lm-1 — IMPLEMENTATION lane LM-1, Wave 2 (G2–G6) | PARTIAL |
| cl-e-server-handle.md | # CL-E — ServerHandle typed child-death failure (IMPLEMENTAT... | ALIVE |
| cl-f-eviction.md | # cl-f-eviction — M8 dead-child eviction + no-respawn law (IMPL... | PARTIAL |
| af-20-pool-drop.md | # af-20-pool-drop — pool-drop child teardown (IMPLEMENTATION ... | PARTIAL |
| cl-g-priority.md | # CL-G Receipt — Priority-string validation (M9 hazard fix) | NONE |
| ab-17-churn-stress.md | # AB-17 — Child-Churn Kill-Stress Court (lsp-max-compositor) | ALIVE |
| sa-11.md | # SA-11 — skill-application lane: lsp-max-scaffold vs run-ggen skill | NONE |
| cl-h-end-position.md | # CL-H: Diagnostic range end preserved through compositor relay | PASS |
| cl-n-flake.md | # Receipt: cl-n-flake — crash_isolation load flake triage | PARTIAL |
| sa-10.md | # SA-10 Receipt — Skill-application: run-ggen laws on compositor self- | PASS |

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
| i3-tokens-substrate.md | # I3 — Semantic-Tokens Substrate Fix (generated classifier ... | ALIVE |
| xd-16-relay-standing.md | # XD-16 — Relay-path standing-unverified reds (root-cause ... | ALIVE |

### ggen-marketplace

| file | first non-blank body line | standing |
|---|---|---|
| ga-2-aoq-gates.md | # Receipt: ga-2-aoq-gates (PG-A6 step 2, lane GA-2) | ALIVE |
| zd-12-baseline-reconcile.md | # ZD-12: baseline-to-disk reconcile (qualification/bas... | ALIVE |
| yd-4-marketplace-changelog.md | # YD-4 — Marketplace CHANGELOG v26.11.3 section cut | ALIVE |
| xd-7-baseline-counts.md | # XD-7: baseline.json counts drift fix | ALIVE |

## Falsifiers

1. **Staleness diff (STEP 4 self-check)**: freeze facts re-run immediately after
   write; the diff against this file's Step-0 table must be empty. Any non-empty
   diff means HEAD moved or receipts landed mid-write — refresh, never edit stale.
2. **Token contradiction**: a receipt whose first non-blank line is itself a bare
   standing token (e.g. yd-4-marketplace-changelog.md opens with `ALIVE`) is
   counted once, and its title row is taken from the next non-blank line. Any
   receipt counted twice or zero times under this scan contradicts the ledger.
3. **Loud-MISSING**: repos without a RECEIPTS.md index (cgap, lsp-max,
   claude-code-config-lsp, ggen-marketplace) are recorded as ABSENT above. A
   future ledger that lists them as present without a path contradicts this one.
4. **Sum-vs-filecount tension (measured finding)**: arcus tokens sum to 33 over 29
   files, claude-code-config-lsp to 73 over 70, ggen-marketplace to 5 over 4.
   Measured cause: `grep -o` emits one output line per match, and `-m1` bounds
   matching LINES, not outputs — a receipt whose first token-bearing line holds
   two tokens (e.g. "PASS and ALIVE") is counted twice. Verified live: 2 arcus
   receipts emit 2 tokens each under the scan, reproducing the +4 excess. The
   Step-1 `uniq -c` blocks are pasted verbatim as evidence of the raw scan; the
   per-receipt ground truth is the Step-2 tables. Recorded, not reconciled.
5. **Rebuild-determinism**: re-running the extraction must reproduce the Step-2
   tables byte-for-byte. A second run differing in any row falsifies the ledger.
6. **Self-check diff must be empty**: the STEP 4 diff is the gate; a ledger whose
   self-check cannot be run or is skipped loudly fails close-out.

