# ggen v26.11.6 Open Items

Standing: PARTIAL

Date: 2026-11-06. Lane R-GGCLOSE. Open items carried out of the
v26.11.6 census (`wave-closing-receipt.md` in this directory). Nothing landed
or reverted by this lane.

## OI-GG-A — Land the six dirty-file groups (needs-landing)

361 modified + 30 untracked files, all classified in the wave-closing receipt
as in-flight work with named receipts, zero abandoned. Proposed landing order
(independent groups; order minimizes cross-group churn):

1. S-10/SB1 standing-line backfill + receipt-lint gate (receipts diff,
   `scripts/receipt_lint.py`, `scripts/ci/receipt_census.py`,
   `receipts/receipt-standing-map.json`, justfile alias,
   `.github/workflows/ci.yml` receipt-lint job). Largest group; lands the
   gate with the corpus it lints.
2. KD-RC recount (`KNOWN_DIVERGENCES.md` + `kdrc-recount.md`).
3. ZP-24 tera dual-engine census court (`crates/ggen-engine/Cargo.toml`
   dev-dep + `tests/tera_whitespace_census.rs` + `Cargo.lock` fragment).
4. ZD-135 tcps evidence-pack restructure (`.gitignore`,
   `examples/tcps-generated/{ggen.toml,ggen.lock,evidence-pack/}`,
   receipts).
5. ZP-22 ML-DSA-65 crate (`crates/ggen-dsa/`, root `Cargo.toml` member line,
   `Cargo.lock`). Depends on the Cargo.lock fragment shared with group 3 —
   sequence 3 before 5 or land the lock pieces atomically.
6. Wave bookkeeping + examples drift (closure receipts, lane tooling,
   self-host repin gate, examples/ doc/dspy drift) — group into coherent
   commits.

Note: `Cargo.lock` spans groups 3 and 5 — one landing must own the lock or
split it explicitly, not both concurrently.

## OI-GG-B — Binary rotation process gap (process, unfixed)

v26.11.4 reached `~/.local/bin/ggen` via manual `cp` with an ad hoc
`ggen.pre-kdg18` backup file. No versioned release artifact, no tag, no
sha256 receipt binding the installed path to the build. Close condition: a
versioned release channel (tagged artifact + digest receipt) so rotation is
a verified transition, not a filesystem copy. The `ggen.bak-26.10.11` and
`ggen.pre-kdg18` files in `~/.local/bin/` are symptomatic.

## OI-GG-C — ZD-135 ladder re-run with rg live

Registered inside KNOWN_DIVERGENCES.md (KD-RC recount): the ZD-135 receipt's
"rg absent on this host" caveat is stale — `rg` is now on PATH. The
forbidden-surface sub-check remains UNVERIFIED as-run; re-run the 53-check
ladder with rg live before flipping.

## OI-GG-D — Receipt-lint gate proven only locally

The new `receipt-lint` CI job (uncommitted) is witnessed locally
(`scripts/receipt_lint.py` runs clean per the S-12 receipt) but the
workflow diff is unlanded, so CI has never exercised it. Closes with
OI-GG-A group 1 landing plus one green CI run on a descendant head.

## OI-GG-E — KD-G18 fix lives on a wave branch, not main

`f569fba24` (KD-G18 at-most-once fence) is pushed to `v26-11-3-wave` but the
branch has not merged to main; arcus consumes a locally-built binary from the
branch checkout. Close condition: branch merge (or release cut) so the fence
is on the main line.
