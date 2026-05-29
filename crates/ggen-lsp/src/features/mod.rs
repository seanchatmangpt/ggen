//! Delivered LSP feature implementations.
//!
//! Each module implements one capability as a pure function over
//! `(FileType, content)` (or a workspace root), returning real data — the
//! analyzers parse, these project to LSP types. The server advertises a
//! capability ONLY because the corresponding module here genuinely delivers it
//! (advertised == delivered).

pub mod code_lens;
pub mod formatting;
pub mod inlay_hint;
pub mod semantic_tokens;
pub mod workspace_symbol;
