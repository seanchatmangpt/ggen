# Evidence — 2026-07-17 gate-verification pass

Captured from real `just check`/`just lint`/`just test` runs (2026-ggen-core-replacement
branch, post-`cargo clean`, warm rebuild), the first time `just test` was run end-to-end on this
branch (see `tasks.md`'s Twenty-fifth follow-up finding for the full narrative).

- `just-check.txt` — `just check` → exit 0.
- `just-lint.txt` — `just lint` → exit 0. Note: this is the root `ggen` package only, not
  `--workspace` (documented, accepted gap — see CLAUDE.md's `just lint` row).
- `just-test.txt` — `just test` (`cargo test --workspace --tests`) → exit 101. **226 of 227**
  test binaries green, **2672 tests passed**. The 1 failing binary,
  `crates/praxis-graphlaw/tests/chatman_acceptance_agents.rs` (7 tests), fails on fixtures that
  don't exist anywhere -- confirmed absent from the vendored source (`~/praxis`) too, a
  pre-existing upstream gap unrelated to this branch's own work, deliberately left untouched per
  this repo's no-edit-vendored-crates precedent. Recorded as its own open ticket in `tasks.md`,
  not silenced.

Scoped claim: everything within `2026-ggen-core-replacement`'s actual scope (the ggen-core
retirement migration and the v26.7.16/17 gap-closure work) is independently green. `just test`'s
literal exit code is non-zero only because of the one named, out-of-scope, vendored-crate gap.
