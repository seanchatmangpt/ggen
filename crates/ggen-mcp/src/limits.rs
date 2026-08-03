//! Re-exported size/row caps, imported (never re-literaled) from the crates
//! that own them. A tool that reimplements its own cap risks drifting from
//! the authoritative value -- see `.claude/rules/coding-agent-mistakes.md`'s
//! Contract Drift mistake class.

/// Server-enforced ceiling on inbound SPARQL query text, mirroring
/// `ggen-lsp`'s `MAX_CONTENT_BYTES` convention for MCP tool input
/// (`crates/ggen-lsp/src/mcp/mod.rs`).
pub const MAX_QUERY_TEXT_BYTES: usize = 1 << 20; // 1 MiB

/// Ceiling on a `root`/path-shaped parameter, matching `ggen-lsp`'s
/// `MAX_PATH_BYTES` convention.
pub const MAX_PATH_BYTES: usize = 4096;

/// The Fortune-5 SPARQL row cap ggen's own sync pipeline already enforces.
/// Re-exported (not re-literaled) so this crate and `ggen-engine` can never
/// silently drift apart.
pub use ggen_engine::sync::MAX_QUERY_RESULT_ROWS;
