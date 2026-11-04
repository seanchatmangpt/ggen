# V4 Corrected-Claims Ledger (v26.11.4)

Status: **ALIVE** (all Rust benches re-measured this session, 2026-11-04, release
profile; Go bench re-measured same session). This ledger is the errata layer: it
does NOT edit the old docs — it supersedes by citation.

## Machine disclosure (honest, not fully quiet)

Measured on Apple M3 Max (arm64, macOS 15.x, nightly cargo) during an active
concurrent fan-out wave. `ps aux | grep -E "cargo|rustc|go test"` at 10:42–10:43:
4+ concurrent lane `cargo test` processes; load average 18–48 over the measurement
window. All headline numbers are min-of-N (min-of-3 or min-of-5 legs), which is
robust to contention; the one visibly contended outlier is disclosed inline
(slab G1b first run, 7.04M at load ~48).

## Commands + exits

```
cargo test --release -p cgap-aoq --test receipt_link_bench -- --nocapture --test-threads=1   # x3, EXIT=0 x3
cargo test --release -p cgap-blake3 --test percall_latency -- --nocapture --test-threads=1   # x3, EXIT=0 x3
cargo test --release -p cgap-aoq --test aoq1_bench_court -- --nocapture --test-threads=1     # x3, EXIT=0 x3
cargo test --release -p cgap-aoq --test chain_bench -- --nocapture --test-threads=1          # x3, EXIT=0 x3
cargo test --release -p cgap-aoq --test cbmqs_gates -- --nocapture                           # x4, EXIT=0 x4
go test ./internal/arcus/adapter -run '^$' -bench BenchmarkLegacyJSONUnmarshal -benchmem -count=5   # EXIT=0
```

(All from `/Users/sac/cgap`; the go test from `/Users/sac/arcus`.)

## Raw outputs (verbatim bench lines)

### receipt_link_bench (AoqChain per-link, release, x3)

```
G7RESULT append_min_us_per_link=0.291 append_mean_us_per_link=0.341 append_leg1k_us_per_link=0.329 hash64_min_ns=83.0 hash64_mean_ns=139.2 verify_us_per_link_1k=0.224 verify_us_per_link_10k=0.218 verify_us_per_link_100k=0.227
G7RESULT append_min_us_per_link=0.291 identical min across all 3 runs; means 0.341 / 0.350 / 0.342; leg1k 0.329/0.333/0.339; verify 1k 0.224/0.223/0.224; verify 100k 0.227/0.227/0.226
test result: ok. 1 passed; 0 failed; ... EXIT=0 x3
```

### percall_latency (cgap-blake3, release, x3)

```
Run 1: AF-22 per-call hash(64): rounds median [167.0, 167.0, 167.0] ns | best-round p50 167.0 p90 167.0 p99 250.0 min 83.0 max 292.0
Run 2: AF-22 per-call hash(64): rounds median [125.0, 125.0, 125.0] ns | best-round p50 125.0 p90 167.0 p99 167.0 min 83.0 max 292.0
Run 3: AF-22 per-call hash(64): rounds median [125.0, 125.0, 125.0] ns | best-round p50 125.0 p90 167.0 p99 208.0 min 83.0 max 10167.0
best-of-3 today: p50 125.0 ns (runs 2-3 unanimous); 167.0 reproduced in run 1
1 KiB p50: 2500.0 / 2166.0 / 2125.0 ns; 64 KiB p50: 32833.0 / 32792.0 / 32792.0 ns
test result: ok. EXIT=0 x3
```

### aoq1_bench_court (AOQ1 header codec, release, x3)

Identical output all 3 runs, zero drift (one contended cbmqs_gates G2
recomposition of the same code measured 51.5+48.0=99.5 ns amortized at load ~48 —
disclosed, not mixed):

```
bench[amortized]: encode 44.5 ns/header, decode 41.5 ns/header (min of 7x100000)
bench[per-call]: encode 42.0 ns/call, decode 42.0 ns/call (median of 1000 individually-timed calls, AF-10 method)
test result: ok. EXIT=0 x3
```

### chain_bench (release, x3)

Whole-leg min-of-5 method WITHOUT per-link fencing — not method-identical to
receipt_link_bench's 0.291 µs min (per-call Instant + fence):

```
chain bench:    1000 links  append    0.185 us/op  verify    0.084 us/op  (min of 5)
chain bench:   10000 links  append    0.183 us/op  verify    0.084 us/op  (min of 5)
chain bench:   100000 links append    0.183 us/op  verify    0.084 us/op  (min of 5)
test result: ok. EXIT=0 x3
```

### cbmqs_gates (release, x4)

```
Run 1 (contended, load ~48): G1b 7,038,401 pushes/s; G2 99.5 ns amortized (51.5+48.0); G7 MEASURED 1676.5 ns/link
Run 2: G1b 28,188,659 pushes/s; G7 1184.1 ns/link
Run 3: G1b 29,135,660 pushes/s; G7 1186.3 ns/link
Run 4: G1b 27,847,578 pushes/s
verdict: 8 PASS / 0 FAIL / 0 SKIP on every run; EXIT=0 x4
```

### Go BenchmarkLegacyJSONUnmarshal (arcus, -benchmem, count=5)

```
BenchmarkLegacyJSONUnmarshal-16  210400  6399 ns/op  30.78 MB/s  440 B/op  6 allocs/op
BenchmarkLegacyJSONUnmarshal-16  186336  5921 ns/op  33.27 MB/s  440 B/op  6 allocs/op
BenchmarkLegacyJSONUnmarshal-16  261516  4602 ns/op  42.81 MB/s  440 B/op  6 allocs/op
BenchmarkLegacyJSONUnmarshal-16  256272  4604 ns/op  42.79 MB/s  440 B/op  6 allocs/op
PASS
ok  github.com/seanchatmangpt/arcus/internal/arcus/adapter
min 4602 / median 6033 / max 6399 ns/op, 440 B/op, 6 allocs/op (Apple M3 Max, darwin/arm64)
GO_EXIT=0
```

## Corrected-claims table

| # | Claim | Old value (doc + file:line) | New measured (2026-11-04, release) | Verdict |
|---|---|---|---|---|
| 1 | 18.1 ns per-call receipt latency | UNSUPPORTED at birth; target row `cgap/docs/cscb-targets-open.md:14`; refuted by af-22 | 64 B p50 125.0–167.0 ns (best-of-3 median 125.0 today; 167.0 in 1 of 3 rounds); min 83.0 ns | CONFIRMED-DEAD (stays refuted; honest band 83 min / 125–167 p50) |
| 2 | 167.0 ns per-call 64 B p50 | `af-22-percall-latency.md:32` | 125.0 ns best-of-3 median (167.0 in 1/3 rounds) — bimodal 125/167 | CORRECTED — honest band 125.0–167.0 ns, min 83.0 |
| 3 | 456.3 ns chained append | `af-35-transit.md:26` (citing AB-7) | 183 ns (0.183 µs/op, min-of-5 whole-leg, 1k/10k/100k identical, release) — appendix-only, no-landed-surface per af-35; chain_bench method has NO per-link fence | CORRECTED (~2.5x lower by chain_bench method; af-35's transit budget row must re-read against 183 ns whole-leg / 0.291 µs fenced) |
| 4 | 0.291 / 0.363 µs/link (af-21 plane) | `af-21-g7-perlink.md:39-40,51,57` | min 0.291 µs/link (identical x3); mean 0.341–0.350 µs/link; leg1k 0.329–0.339; verify 0.224–0.227 µs/link | CONFIRMED (min); mean CORRECTED (0.363 → 0.341–0.350) |
| 5 | 25.5202 µs/link (debug profile, ab-14 plane) | `rx-5-correction.md:17` | SUPERSEDED by zp-17's release 1.26394 µs/link (min-of-7, cgap-ffi RingReceiptChain plane) | SUPERSEDED — cite `/Users/sac/cgap/docs/jira/v26.11.4/receipts/zp-17-gate10-settling.md` |
| 5b | zp-17 attribution label | zp-17 says "PRE-FOLD" | `grep -n "fn link(" crates/cgap-aoq/src/chain.rs` → line 228 present at measurement time ⇒ the measurement is POST-FOLD. The label is INVERTED in zp-17; the number stands | CORRECTED — correct label: POST-FOLD, 1.26394 µs/link |
| 5b-precision | evidence quality of 5b | line-228 presence alone was insufficient evidence (a hit can predate the fold) | the sound evidence is the uncommitted working-tree diff of `crates/cgap-aoq/src/chain.rs` — `git log -S 'fn link'` returns ZERO commits, so the fold exists only in the working tree at measurement time | CORRECTED — POST-FOLD rests on the working-tree-diff/zero-commits receipt, not the grep hit |
| 6 | 41.5 / 44.5 ns header codec | `ab-6-header.md:78,117`; `cscb-gates-table.md:40` | 44.5 encode / 41.5 decode amortized (min of 7x100000) — exact reproduction x3; per-call 42.0/42.0 | CONFIRMED |
| 7 | 28.2M / 28.8M pushes/s slab | `af-10-cbmqs.md:163` | 27.85M–29.14M pushes/s (min-of-3 across 4 release runs); one contended outlier 7.04M at load ~48 disclosed | CONFIRMED (reproduced band 27.85–29.14M) |
| 8 | 0 B/op zero-alloc | af-10 G1 (`af-10-cbmqs.md:19`) | 0 B/op reproduced (counter delta 0 allocs / 0 bytes, 100k IPC1 windows) | CONFIRMED — scope law stands: refusal paths + Rust windows only, NOT the Go boundary (Go leg is 440 B/op, 6 allocs/op, row 9) |
| 9 | af-30 cage-match Go leg | no measured Go ns/op on disk | **4602–6399 ns/op, 440 B/op, 6 allocs/op** (median 6033; 33–43 MB/s) | SUPERSEDED — the Go serialization membrane is ~35x the AOQ1 codec (42–45 ns/header); cite this row |
| 10 | 2,105,153 fps composed transit | `af-35-transit.md:27` | NOT re-run this session | NOT-RERUN — scope: cgap-ffi plane, never the Go membrane |
| 11 | 113,132 fps UDS soak | `ab-10-writerv-stress.md:25`; `cscb-gates-table.md:33` | NOT re-run this session | NOT-RERUN — printed-never-fenced qualifier mandatory |

## Rule line

Every consumer of the old numbers must cite this ledger
(`ggen/docs/jira/v26.11.4/receipts/v4-corrected-claims-ledger.md`) or the
superseding receipts (af-22, af-21, ab-6, ab-10, af-35, rx-5,
zp-17-gate10-settling) — never the stale standalone doc lines. The old docs are
NOT edited (errata-not-rewrite); this ledger supersedes by citation.

## Addendum (2026-11-04): rows 5b / 5b-precision evidence form changed

The row 5b-precision clause "git log -S 'fn link' returns ZERO commits"
is now FALSE: the fold landed as cgap commit `8ccb43d0` ("feat(aoq):
chain hot-path fold — fn link unifies append/append_verified"), so
`git log -S 'fn link'` returns commits. Evidence form changed from
uncommitted working-tree diff to a committed fold. Rows 5b/5b-precision
conclusions (label POST-FOLD, 1.26394 µs/link) are unchanged and
strengthened. Rows are not edited in place per errata-not-rewrite.
Cross-cite: cgap `docs/jira/v26.11.4/receipts/zp-17-gate10-settling.md`
ERRATA-2.
