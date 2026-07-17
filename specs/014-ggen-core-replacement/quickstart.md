# Quickstart: Verifying the ggen-core replacement

This is the end-to-end validation path for this migration once implementation
(`/speckit.tasks` → `/speckit.implement`) is complete. It exercises every acceptance
scenario from `spec.md` in one pass. Every command below is `just`-based, per this
repository's actual tooling (see `research.md`'s note on the `cargo make` vs. `just`
constitution discrepancy).

## 1. Confirm the retired engine is gone

```bash
grep -rn "ggen_core" crates/ src/ tests/ examples/ benches/ 2>/dev/null
# Expected: zero matches (SC-006)
test -d crates/ggen-core && echo "FAIL: ggen-core still exists" || echo "OK: ggen-core removed"
```

## 2. Confirm the workspace builds and all gates pass

```bash
just check      # cargo check --workspace
just lint       # cargo clippy --all-targets -- -D warnings
just test       # cargo test --workspace --tests
just slo-check  # retargeted per docs/jira/v26.7.16/11-DELETION-AND-DEFINITION-OF-DONE.md
```

## 3. Confirm publish safety (User Story 2)

```bash
cargo publish --dry-run --package ggen
./scripts/ci/guard-publish-target.sh
# Expected: both succeed; the guard confirms no other crate shares the name "ggen"
```

## 4. Confirm command parity (User Story 1)

Run each command in `contracts/cli-command-surface.md`'s table against a scratch project and
confirm output matches pre-migration behavior (this is a manual/scripted diff during
implementation, not a single command — see that contract file for the full list).

```bash
cargo run --bin ggen -- sync --dry-run
cargo run --bin ggen -- pack list
cargo run --bin ggen -- doctor
cargo run --bin ggen -- receipt history
```

## 5. Confirm receipt integrity AND authenticity (User Story 3)

```bash
cargo run --bin ggen -- sync
cargo run --bin ggen -- sync   # second run, should chain onto the first
# Tamper with the middle receipt's stored content, then:
cargo run --bin ggen -- receipt verify
# Expected: reports the specific broken receipt (chain integrity)
cargo run --bin ggen -- receipt verify --public-key .ggen/keys/public.pem
# Expected: distinguishes "chain-valid + signature-valid" from "chain-valid + unsigned"
```

## 6. Confirm operational telemetry actually fires (FR-005)

```bash
export RUST_LOG=trace,ggen=trace
cargo run --bin ggen -- sync --audit true 2>&1 | tee otel_sync_output.txt
grep -E "pipeline\.(load|extract|generate|validate|emit)" otel_sync_output.txt
grep -E "pipeline\.duration_ms=|pipeline\.files_generated=" otel_sync_output.txt
# Expected: all five spans present, both attributes populated with non-placeholder values
# (this is the confirmed-broken-today path fixed in 04-RECEIPT-SIGNING-AND-OTEL.md --
#  absence here means the fix did not land, not that telemetry is merely "off")
```

## 7. Confirm the architectural boundary guard (User Story 4)

```bash
./scripts/ci/guard-process-intelligence-boundary.sh
# Expected: passes clean on the current codebase (SC-007's positive case)

# Negative-case check (do this once during implementation validation, not routinely):
# temporarily add `use praxis_graphlaw::chatman;` anywhere under crates/ or src/, re-run
# the guard, confirm it fails with a clear message, then revert the change.
```

## 8. Confirm no compatibility shim was left behind (FR-011)

```bash
grep -rn "pub use.*ggen_core\|re-export.*ggen_core" crates/ src/ 2>/dev/null
# Expected: zero matches
```
