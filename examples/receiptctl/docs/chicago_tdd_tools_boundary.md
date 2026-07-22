# CLI Boundary Proofs (chicago-tdd-tools)

Generated from `ctt:CliBoundaryTest` individuals. Each row is one
Chicago-style `#[test]` in `tests/chicago_tdd_tools_boundary.rs` that
crosses a real binary boundary via `CliHarness` — no mocks.

| Test | Binary | Args | Exit | Axiom covered |
|------|--------|------|------|---------------|
| `receiptctl_algorithm_list_succeeds` | `receiptctl` | `algorithm list` | 0 | receiptctl algorithm list (argv composed from clap-noun-verb-pack's AlgorithmList command) exits 0 and prints a JSON array |
| `receiptctl_help_lists_verbs` | `receiptctl` | `--help` | 0 | receiptctl --help exits 0 with usage text |
| `receiptctl_known_noun_unrecognized_verb_fails_closed` | `receiptctl` | `algorithm frobnicate` | 1 | a known noun with an unrecognized verb exits nonzero with a clap error on stderr, distinct from an entirely-unknown top-level noun |
| `receiptctl_receipt_emit_fails_without_required_arg` | `receiptctl` | `receipt emit` | 1 | receipt emit without its required --sync-receipt-id flag fails closed via clap's own required-argument enforcement |
| `receiptctl_session_login_fails_without_required_arg` | `receiptctl` | `session login` | 1 | session login without its required --token flag fails closed via clap's own required-argument enforcement |
| `receiptctl_unexpected_flag_fails_closed` | `receiptctl` | `algorithm list --bogus-flag` | 1 | an unrecognized flag on an otherwise-valid command exits nonzero with a clap error on stderr |
| `receiptctl_unknown_verb_fails_closed` | `receiptctl` | `frobnicate` | 1 | an unknown subcommand exits nonzero with a clap error on stderr |
| `receiptctl_user_create_fails_without_required_arg` | `receiptctl` | `user create` | 1 | user create without its required --name flag fails closed via clap's own required-argument enforcement |
| `receiptctl_version_emits_name` | `receiptctl` | `--version` | 0 | receiptctl --version exits 0 and prints a version string |

