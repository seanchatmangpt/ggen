//! Size/row caps enforced by this crate's tools. Only `MAX_QUERY_RESULT_ROWS`
//! below is a genuine re-export (from `ggen-engine`, never re-literaled, so
//! it cannot drift). The two byte caps are ggen-mcp-local literals chosen to
//! match `ggen-lsp`'s `MAX_CONTENT_BYTES`/`MAX_PATH_BYTES` convention by
//! value, not by import -- those consts are private (non-`pub`) in
//! `crates/ggen-lsp/src/mcp/mod.rs`, so a real re-export isn't possible
//! without a cross-crate API change there. See
//! `.claude/rules/coding-agent-mistakes.md`'s Contract Drift mistake class.

/// Server-enforced ceiling on inbound SPARQL query text, matching (by value,
/// not by import -- see module doc) `ggen-lsp`'s private `MAX_CONTENT_BYTES`
/// convention for MCP tool input (`crates/ggen-lsp/src/mcp/mod.rs`).
pub const MAX_QUERY_TEXT_BYTES: usize = 1 << 20; // 1 MiB

/// Ceiling on a `root`/path-shaped parameter, matching (by value, not by
/// import -- see module doc) `ggen-lsp`'s private `MAX_PATH_BYTES`
/// convention.
pub const MAX_PATH_BYTES: usize = 4096;

/// The Fortune-5 SPARQL row cap ggen's own sync pipeline already enforces.
/// Re-exported (not re-literaled) so this crate and `ggen-engine` can never
/// silently drift apart.
pub use ggen_engine::sync::MAX_QUERY_RESULT_ROWS;
