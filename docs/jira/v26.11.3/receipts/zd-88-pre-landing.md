# ZD-88: Pre-Landing Gate Receipt (v26-11-3-wave)

Standing: UNSAFE — sweep must not land red

Date: 2026-11-04
Branch: `v26-11-3-wave` (169 dirty entries at gate time)
Verdict: **UNSAFE — sweep must not land red**

## Gate table

| # | Gate | Command | Exit | Verdict |
|---|---|---|---|---|
| 1 | fmt | `cargo fmt --all -- --check` | 1 | RED — flap class confirmed |
| 2 | check | `cargo check --workspace` | 0 | PASS |
| 3 | clippy (ggen-abb-sbb) | `cargo clippy --locked --manifest-path crates/ggen-abb-sbb/Cargo.toml --all-targets -- -D warnings` | 101 | RED — post-ZD-70 lint drift; retry-after-wait did not clear |
| 4 | diff --check | `git diff --check` | 2 | RED — 6 files with blank line at EOF |
| 5 | gap closure | `python3 book/scripts/validate_gap_closure.py` | 0 | PASS — errors=0 (26/26/18/18) |
| 6a | config tests | `cargo test -p ggen-config` | 0 | PASS — 11 passed |
| 6b | engine keys | `cargo test -p ggen-engine --lib keys` | 0 | PASS — 19 passed |
| 6c | sync surface | `cargo test -p ggen-cli-lib --test sync_surface_test` | 0 | PASS — 6 passed |

## Classifications (file:line)

### Gate 1 — cargo fmt RED

Unformatted code in dirty files. Diff sites:

- `crates/ggen-engine/src/generation_rules.rs:853`
- `crates/ggen-engine/src/sync.rs:1872`
- `crates/ggen-engine/src/sync.rs:3989` (`write_receipt` signature)
- `crates/ggen-engine/tests/fm_gen_merge_refusal_court.rs:46,60,76`
- `crates/ggen-engine/tests/fm_tpl_frontmatter_parse_court.rs:14`
- `crates/ggen-sjira/tests/execute_test.rs:372,380,385`

### Gate 3 — clippy RED (ggen-abb-sbb)

17 errors on first run (49 lint mentions on retry), all `clippy::expect_used` /
`clippy::unwrap_used` under `-D warnings`, in:

- `crates/ggen-abb-sbb/tests/ea_projection_test.rs:44,66,77,81,83,92` (… through the file)

Retry protocol: ZD-70 surface `crates/ggen-abb-sbb/tests/ea_projection_test.rs` was NOT
dirty and last-modified ~75 min before gate time; `crates/ggen-abb-sbb/src/depgraph.rs` IS
dirty. One retry after 60s bounded wait still exits 101 — the lint drift is settled state,
not mid-edit flap. Requires expect/unwrap remediation or an explicit allow with disclosure
by the owning lane.

### Gate 4 — git diff --check RED

Blank line at EOF (6 files):

- `examples/affidavit-verify/docs/affidavit_reference.md:25`
- `examples/ggen-cli-verify/docs/chicago_tdd_tools_boundary.md:31`
- `examples/tcps-generated/VERIFICATION.md:25`
- `examples/tcps-generated/infra/docs/ABI.md:18`
- `examples/tcps-generated/receipts/EVIDENCE_SNAPSHOT.md:14`
- `examples/zero-code-inventoryctl/docs/command-reference.md:51`

## Resolution path

`cargo fmt --all` + strip trailing blank EOF lines in the 6 docs + ZD-70 lane remediates
expect/unwrap in `ea_projection_test.rs` (or registers the allow). Re-run this gate set
before the terminal sweep.
