//! DEPRECATED: Old command layer - Use `cmds` module instead
//!
//! **Deprecation Notice**: This module structure is deprecated as of v2.0.0 and will be
//! removed in v2.1.0 (February 2026). Please migrate to the new three-layer architecture:
//!
//! - **CLI Layer**: `cli/src/cmds/` - Command-line interface and presentation
//! - **Domain Layer**: `cli/src/domain/` - Business logic and domain operations
//! - **Runtime Layer**: `ggen-core/`, `ggen-ai/` - Core generation and AI functionality
//!
//! **Migration Path**:
//! ```rust
//! // OLD (DEPRECATED)
//! use ggen_cli::commands::template::GenerateArgs;
//! use ggen_cli::commands::marketplace::SearchArgs;
//!
//! // NEW (v2.0.0+)
//! use ggen_cli::cmds::template::TemplateCmd;
//! use ggen_cli::cmds::market::MarketCmd;
//! use ggen_cli::domain::template;
//! use ggen_cli::domain::marketplace;
//! ```
//!
//! **Timeline**:
//! - v2.0.0 (Current): Deprecation warnings, both systems work
//! - v2.1.0 (Feb 2026): This module will be removed
//! - v2.2.0 (May 2026): Clean architecture, no legacy code
//!
//! **See**: `docs/MIGRATION_V1_TO_V2.md` and `.claude/refactor-v2/deprecation-plan.md`

#![deprecated(
    since = "2.0.0",
    note = "Use `cli/src/cmds/` and `cli/src/domain/` instead. Will be removed in v2.1.0 (Feb 2026). See docs/MIGRATION_V1_TO_V2.md"
)]

pub mod ai;
pub mod marketplace;
pub mod project;
pub mod template;
pub mod utils;

// Re-export commonly used commands
pub use utils::doctor;
