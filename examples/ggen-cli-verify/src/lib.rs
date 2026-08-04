//! `ggen-cli-verify` — committed, real consumer of `chicago-tdd-tools-pack`.
//!
//! Unlike `examples/receiptctl` (which proves the pack against a small
//! example binary), this consumer targets ggen's own real CLI: the ontology
//! in `schema/domain.ttl` describes `ctt:CliBoundaryTest` individuals whose
//! `ctt:binary` is `"ggen"`. `ggen sync run` renders
//! `tests/chicago_tdd_tools_boundary.rs`, which spawns the real compiled
//! `ggen` binary via `chicago_tdd_tools::cli_proof::CliHarness` and asserts
//! on its actual exit codes and stdout/stderr. No mocks, no stubs.
//!
//! This crate carries no library logic of its own — it exists only to give
//! the generated tests a home and a dev-dependency on `chicago-tdd-tools`.
