# court-fresh-epoch — blocker receipt (v26.11.15)

STATUS: PROMOTED

The "BLOCKED-class by design" label is stale. `court-fresh-epoch` is already on the
`pre-commit:` line of `justfile` (witnessed exit 0 on 2026-10-20). Exit code 2 from
`scripts/fresh_epoch_lease_court.sh` is the script's typed, fail-closed refusal when its
before-witness fails; it is not a design block.

## Environmental blockers seen in the v26.11.15 wave (not code defects)

1. `.cargo/config.toml` sets `rustc-wrapper = "sccache"`; sccache is not installed in the
   cloud container, so every cargo call exits 101. Unblock: install sccache or run with
   `RUSTC_WRAPPER=`.
2. Under heavy concurrent builds (load ~32) `rustc` for `purrdf-rdf` was killed with no
   diagnostics and the court exited 2 after ~20 minutes. A standalone
   `RUSTC_WRAPPER= cargo build -p purrdf-rdf` exited 0. Unblock: run the court on a quiet
   machine with free disk.

## What was NOT witnessed in this wave

The court has not been re-run to exit 0 here. The exit-5 mapping of `[FM-WRITE-012]`
(`cli_boundary::sync_run_stale_lease_refusal_exits_five`) was read, not run.
