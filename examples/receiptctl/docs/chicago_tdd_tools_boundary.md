# CLI Boundary Proofs (chicago-tdd-tools)

Generated from `ctt:CliBoundaryTest` individuals. Each row is one
Chicago-style `#[test]` in `tests/chicago_tdd_tools_boundary.rs` that
crosses a real binary boundary via `CliHarness` — no mocks.

| Test | Binary | Args | Exit | Axiom covered |
|------|--------|------|------|---------------|
| `receiptctl_help_lists_verbs` | `receiptctl` | `--help` | 0 | receiptctl --help exits 0 with usage text |
| `receiptctl_unknown_verb_fails_closed` | `receiptctl` | `frobnicate` | 1 | an unknown subcommand exits nonzero with a clap error on stderr |
| `receiptctl_version_emits_name` | `receiptctl` | `--version` | 0 | receiptctl --version exits 0 and prints a version string |

