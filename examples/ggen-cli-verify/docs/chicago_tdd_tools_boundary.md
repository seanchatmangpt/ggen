# CLI Boundary Proofs (chicago-tdd-tools)

Generated from `ctt:CliBoundaryTest` individuals. Each row is one
Chicago-style `#[test]` in `tests/chicago_tdd_tools_boundary.rs` that
crosses a real binary boundary via `CliHarness` — no mocks.

## Prerequisites

`CliHarness::cargo_bin` resolves each binary below via `CARGO_BIN_EXE_*`,
then this workspace's own `target/{debug,release}/`, then a `PATH` search
(see `chicago_tdd_tools::cli_proof::CliHarness::cargo_bin`). If a binary
below is not a `[[bin]]` target of *this* consumer crate's own workspace,
it must be built and placed on `PATH` before `cargo test` will pass —
derived from `ctt:binary`, not hand-maintained:

- `ggen`
- `receiptctl`


| Test | Binary | Args | Exit | Axiom covered |
|------|--------|------|------|---------------|
| `ggen_graph_validate_missing_file_fails_closed` | `ggen` | `graph validate --files /tmp/does-not-exist-xyz.ttl` | 1 | graph validate against a nonexistent file fails closed with a named-file error, not a panic |
| `ggen_help_lists_usage` | `ggen` | `--help` | 0 | ggen --help exits 0 with usage text |
| `ggen_unknown_subcommand_fails_closed` | `ggen` | `bogus-noun` | 1 | an unknown ggen subcommand exits nonzero with a clap error on stderr |
| `ggen_version_emits_name` | `ggen` | `--version` | 0 | ggen --version exits 0 and prints its own name and version |
| `receiptctl_help_lists_verbs` | `receiptctl` | `--help` | 0 | receiptctl --help exits 0 with usage text |
| `receiptctl_known_noun_unrecognized_verb_fails_closed` | `receiptctl` | `algorithm frobnicate` | 1 | a known noun with an unrecognized verb exits nonzero with a clap error on stderr, distinct from an entirely-unknown top-level noun |
| `receiptctl_unexpected_flag_fails_closed` | `receiptctl` | `algorithm list --bogus-flag` | 1 | an unrecognized flag on an otherwise-valid command exits nonzero with a clap error on stderr |
| `receiptctl_unknown_verb_fails_closed` | `receiptctl` | `frobnicate` | 1 | an unknown subcommand exits nonzero with a clap error on stderr |
| `receiptctl_version_emits_name` | `receiptctl` | `--version` | 0 | receiptctl --version exits 0 and prints a version string |

