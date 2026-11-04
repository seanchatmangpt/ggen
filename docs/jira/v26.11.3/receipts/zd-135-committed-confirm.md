# ZD-135: committed-tree CI-consistency confirmation

Subject: ggen @ 14c6612bfecde172c854536f2f687f3bbd71e03f (pushed, tree clean before/after runs).

Four CI-representative checks executed against the committed tree. **Result: 3/4 exit 0; check 1 is 52/53 — same count and same single failure as ZD-127, no worse.**

## Check results

| # | Check | Exit | Result |
|---|---|---|---|
| 1 | `bash scripts/validate-examples.sh` (check mode) | 1 | 52/53 ALIVE — matches ZD-127, "same" verdict vs "same or better" expectation |
| 2 | `bash scripts/verify-combinatorial-maximalism-pack.sh` (CARGO_TARGET_DIR=target) | 0 | `combinatorial-maximalism-pack: ALIVE` (full ladder: static contract GREEN, deterministic double-sync digest match, gate refusals, tamper refusals, cmdctl actuate/replay) |
| 3 | `cargo fmt --all -- --check` | 0 | clean |
| 4 | `python3 book/scripts/validate_gap_closure.py` | 0 | `obligations=26 declared_gaps=26 evidence_records=18 used_evidence=18 errors=0` |

## Check 1 detail (52/53)

- Command: `GGEN_BIN=/Users/sac/ggen/target/debug/ggen bash scripts/validate-examples.sh`
- **Binary dependency is load-bearing.** With the ambient `~/.local/bin/ggen` (self-labeled 26.11.3 but built before the `ea_admission_skipped` receipt field landed in `crates/ggen-engine/src/sync.rs`), **0/53** manifests pass — every sync refuses with `[FM-CHAIN-010]` on the committed `receipt-log.jsonl` tails. The tree-local binary built from 14c6612bf passes 52. CI builds from source, so the tree-local binary is the CI-representative one; record this so the 52/53 is not misread as a regression.
- Failing manifest: `examples/tcps-generated/ggen.toml` — `[FM-PACK-008]` pack `tcps-evidence` content-hash mismatch: committed `ggen.lock` pins `blake3:2e78e755…` but the pack on disk hashes to `blake3:506fbf32…`. Remediation per the error: restore the pack or delete ggen.lock to re-lock. Pre-existing at 14c6612bf; identical failure in ZD-127.
- Side effects of successful syncs mutated 124 tracked `.ggen-v2/receipt*` files; all restored via `git restore examples/` — final `git status --porcelain` = 0 entries at 14c6612bf.

## Check 2 caveats

- The line-145 forbidden-surface gate (`rg -n 'TODO|FIXME|…'`) was a **silent skip on this host**: no `rg` binary on PATH (`rg` exists only as a zsh function here; bash subprocess does not see it), `if rg …` returned 127 → treated as "no forbidden surface". Fail-open locally; GitHub runners ship rg, so CI executes this gate. Unverified locally, standing UNVERIFIED for that one sub-check.
- Required clearing two stale untracked artifacts left by earlier sessions' runs of this same ladder: `examples/combinatorial-maximalism/ggen.lock` and `examples/combinatorial-maximalism/generated/` (both git-ignored, 0 tracked files, verified before deletion). The ladder's own `test ! -e generated` and the validator's `CONSUMER_AUTHORED_SURFACE_REFUSED` both correctly refuse when a previous run's artifacts persist — worth noting for future ZD lanes: **the ladder is not idempotent over its own leftovers; clean the consumer dir before rerunning.**

## Standing

ALIVE for checks 2–4 (real exits, real output). Check 1 PARTIAL: 52/53, one registered divergence (tcps-generated FM-PACK-008), binary-dependency caveat above. No source edits made; nothing committed (coordinator lands this receipt).
