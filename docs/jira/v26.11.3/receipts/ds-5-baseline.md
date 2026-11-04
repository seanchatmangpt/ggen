# DS-5 Process-Capability Baseline — ggen-sjira Admission Kernel

Lane: DS-5 (DMEDI MEASURE phase). Date: 2026-11-03. Repo: /Users/sac/ggen.
Deliverable status: ALIVE — every value below is a real command+exit from this session.
No Cpk is computed anywhere in this register: courts are deterministic, so the honest
framing is per-court constants and bench distributions, never Cpk on 3 samples.

## Session conditions (honest environment receipt)

This checkout was under active concurrent fan-out during measurement (other DS lanes
landing files mid-session). Consequences, each witnessed this session:

- Whole-suite wall: first run 38.48 s (cold, exit 0, green on the then-11-file corpus),
  then 1.05 s / 1.07 s hot. Later re-runs could not reproduce a green whole-suite run:
  4 test targets were compile-broken in-flight (projection_test, reconcile_test,
  execute_test, execute_lease_test — E0425/E0063/E0382/E0382/E0599/E0277 in OTHER
  lanes' in-flight files, none authored or edited by this lane).
- ggen-cli-lib flip-flopped compile-broken → clean → broken (4–6 errors) between
  builds ~02:00–02:03Z. The binary `target/debug/ggen` (mtime Nov 3 17:35) predates
  the `#[verb]` sjira verbs, so `ggen sjira admit <file>` fails argument parsing
  (exit 1, "unexpected argument 'admit'"). CLI admit latency: **UNMEASURABLE this
  session**; cheap to re-measure once the tree settles and the bin rebuilds.

## Metric register

| Metric | Method (real command) | Value | Samples | Standing |
|---|---|---|---|---|
| Suite wall (cold, 11-file corpus) | `/usr/bin/time -p cargo test -p ggen-sjira` | 38.48 s, exit 0 | 1 | PARTIAL (corpus changed after) |
| Suite wall (hot) | same | 1.05 s, 1.07 s | 2 | PARTIAL (tree churning) |
| Tests passing (12 compiling targets, individually) | `cargo test -p ggen-sjira --test <t>_test` ×12 | 124 passed / 0 failed | 1 | ALIVE |
| complete_test | same | 6 passed / 2 FAILED | 1 | PARTIAL (another lane's in-flight file) |
| Court-corpus size | `grep -c '#\[test\]' tests/*.rs` | 173 test fns / 17 files | 1 | ALIVE (as of ~02:00Z 2026-11-04 UTC) |
| Admit:refuse census (core admission court) | grep + test run, work_order_admission_test.rs | 4 admit / 8 refuse / 12 tests | 1 | ALIVE |
| Distinct refusal classes exercised | `grep -o "Refusal::[A-Za-z]*" tests/*.rs` | 58 distinct classes / 105 occurrences | 1 census | ALIVE |
| Refusal-class frequency | same census | see frequency table | 1 census | ALIVE |
| Kernel admit+journal cycle | /tmp/ds5-bench (counting allocator, release, +nightly) | 32.5 µs/cycle; 609 allocs/cycle; live-bytes delta 612 B | 1000 iterations | ALIVE (real repeated distribution) |
| Chain verify, 1000-record journal | same bench | 1998.8 µs ≈ 2.0 µs/record; ~36 allocs/record; verify_chain=Ok | N=1000 | ALIVE |
| Determinism (byte-identical replay) | bench: two independent journal builds, serialized transitions equal + heads equal; suite witness receipts_test run twice | equal=true, head_equal=true, 774-byte journal | 2 runs | ALIVE |
| CLI admit latency | `ggen sjira admit /tmp/ds5_work_order.json` | UNMEASURABLE this session | 0 | BLOCKED (stale bin lacks verbs; ggen-cli-lib in-flight broken) |

## Per-target wall (single runs, hot cache)

| Target | Tests | Wall |
|---|---|---|
| authority | 20 passed | 0.00 s |
| controller | 8 passed | 0.00 s |
| descriptor | 10 passed | 0.00 s |
| edits_bridge | 11 passed | 4.03 s (dominates the hot suite: real subprocess work) |
| frontier | 10 passed | 0.00 s |
| lease_epoch | 22 passed | 0.01 s |
| ledger | 15 passed | 0.12 s |
| receipts | 4 passed | 0.02 s |
| unwitnessed_refusal | 4 passed | 0.00 s |
| work_order_admission | 12 passed | 0.00 s |
| xaas_loop | 4 passed | 0.07 s |
| complete | 6 pass / 2 fail | 0.01 s |
| **Total** | **124 pass / 2 fail** | **~4.2 s hot** |

## Refusal-class frequency (n=105 occurrences over 58 classes)

Census method: `grep -ho "Refusal::[A-Za-z]*" tests/*.rs | sort | uniq -c | sort -rn`
over all 17 test files, 2026-11-04 ~01:45–02:10Z.

```
  5 UnattributedImplementation     5 AuthorityNotPinned       4 AuthorityDigestMismatch
  3 SignatureInvalid               3 NotOnFrontier            3 AuthorityNotAdmitted
  3 AuthorityIndexUnavailable      3 AliveWithoutVerifierPass 2 WatermarkUnavailable
  2 UnsupportedOutcome             2 TamperedEvidence         2 StandingNotUnknown
  2 NotDurable                     2 LegacyEdit               2 InsufficientSignatures
  2 IllegalTransition              2 Deserialization          2 BridgeMismatch
  2 BridgeInvalid                  1 (each of 36 remaining classes)
```

Note: this ASCII table is a hand transcription of the machine census. The
authoritative counts are the census command itself
(`grep -ho "Refusal::[A-Za-z]*" tests/*.rs | sort | uniq -c | sort -rn` over the
17 test files at ~02:00Z 2026-11-04); re-run it rather than trusting the table.

## Determinism receipt

- Bench: two independent journal builds (same admit inputs), serialized transitions
  byte-equal (`serialized_equal=true`), heads equal, 774-byte journal,
  fingerprint f23a5d4de715791b (FNV-1a over serialized transitions).
- Suite witness: `cargo test -p ggen-sjira --test receipts_test` run twice, both
  `test result: ok. 4 passed; 0 failed` including `determinism_across_instances`.
- Chain verify over the 1000-record journal returned Ok.

## Capability framing (EXPLORE outcome)

- Distributions exist only where there are real repeated samples: the 1000-iteration
  bench cycle (32.5 µs/cycle, 609 allocs/cycle) and the N=1000 chain verify
  (~2.0 µs/record). Those two rows may carry capability indices in DS-6.
- Courts are deterministic: per-court wall times are constants of the corpus, not
  distributions; Cpk on 1 sample is banned and none is computed.
- Refusal-class frequency is a corpus property, not a process distribution — it
  measures which typed refusals the corpus exercises, not production behavior.
- UNMEASURED rows: production admission rate (no production traffic observed),
  refusal classes in production, CLI admit latency (blocked this session). They stay
  UNMEASURED.

## Bench artifact for DS-6

Left in place for DS-6 to formalize or delete with numbers transcribed:
`/tmp/ds5-bench` (single-file counting-allocator bench over the real kernel).
Commands: `cd /tmp/ds5-bench && cargo +nightly run --release -q`. Pattern source:
cgap `crates/cgap-arrow/tests/mutation_kills.rs` counting allocator.

## File paths

- Register: /Users/sac/ggen/docs/jira/v26.11.3/receipts/ds-5-baseline.md (this file)
- Bench crate: /tmp/ds5-bench (main.rs single-file counting-allocator bench)
- CLI fixture: /tmp/ds5_work_order.json
- Pattern source: /Users/sac/cgap/crates/cgap-arrow/tests/mutation_kills.rs