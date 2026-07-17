//! Project scaffolding — presentation/UX logic backing `ggen init` and `ggen wizard`.
//!
//! Ported from `ggen-core::{cli_generator, project_generator}` as part of the
//! ggen-core retirement migration (`specs/014-ggen-core-replacement/tasks.md` T029-T030).
//!
//! This module lives inside `ggen-cli` rather than a standalone crate: it is
//! presentation/UX logic invoked only by `ggen init`/`ggen wizard`, not a generally
//! reusable library capability. See
//! `docs/jira/v26.7.16/07-PROJECT-SCAFFOLDING-PORT.md` ("Target design") for the
//! rationale — first-principles design here means not manufacturing a library
//! abstraction for a single caller.
//!
//! ## Wiring status
//!
//! As of the port (T029-T030), `cli_generator`/`project_generator` had zero callers:
//! `cmds/init.rs` and `cmds/wizard.rs` still used their own pre-existing logic. Re-pointing
//! those two commands at this module is tracked separately as T043 in
//! `specs/014-ggen-core-replacement/tasks.md`.
//!
//! `transaction` (`FileTransaction`) and `preflight` (`PreFlightValidator`) were added in
//! Phase 3 ("non-colliding noun re-points") specifically to get `cmds/init.rs` off its
//! `ggen_core::codegen::FileTransaction` / `ggen_core::validation::PreFlightValidator`
//! imports; `cmds/init.rs` is their sole caller. `cmds/wizard.rs` is out of scope for that
//! port and continues to use `ggen_core::codegen::FileTransaction` directly.

pub mod cli_generator;
pub mod preflight;
pub mod project_generator;
pub mod transaction;
