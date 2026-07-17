//! Domain-shaped types ported out of `ggen-core`'s `domain/` tree.
//!
//! Ported from `ggen-core/src/domain/{error.rs,mcp_config.rs}`
//! (specs/014-ggen-core-replacement, docs/jira/v26.7.16/12-OPEN-QUESTIONS.md item 2). Named
//! `domain::{error,mcp_config}` (not flattened to the crate root) deliberately: the crate root
//! already re-exports a module named `error` from `receipt::error` (see `lib.rs`), so a
//! crate-root `error` module here would collide; keeping the original `domain::` prefix also
//! matches the path shape the still-open `12-OPEN-QUESTIONS.md`/`tasks.md` T053 bucket already
//! uses to refer to this code ("domain::error"/"domain::mcp_config"), minimizing the diff for
//! any consumer re-pointing an import.

pub mod error;
pub mod mcp_config;
