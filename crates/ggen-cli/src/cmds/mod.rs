//! Command Router Module - clap-noun-verb v26.5.19 Auto-Discovery
//!
//! This module provides the entry point for clap-noun-verb auto-discovery.
//! All noun modules with `\[verb\]` functions are automatically discovered and registered.
//!
//! ## Architecture
//! ```text
//! cmds (router) -> auto-discovery -> [verb] functions -> domain (async logic)
//! ```
//!
//! ## Removed Commands
//!
//! The following commands were removed in v26.5.19:
//! - `ggen generate` → Use `ggen sync`
//! - `ggen validate` → Use `ggen sync --validate-only`
//! - `ggen template *` → Use `ggen sync`
//! - `ggen project *` → Add back in v26.5.19+
//! - `ggen graph *` → Add back in v26.5.19+
//! - `ggen ontology *` → Add back in v26.5.19+
//! - `ggen marketplace *` → Add back in v26.5.19+
//! - `ggen ai *` → Add back in v26.5.19+
//! - `ggen test *` → Add back in v26.5.19+
//! - `ggen utils *` → Add back in v26.5.19+
//! - `ggen ci *` → Add back in v26.5.19+
//! - `ggen workflow *` → Add back in v26.5.19+

// Shared helpers for command modules
pub mod helpers;

// Core commands: ggen sync & ggen init
pub mod git_hooks;
pub mod init;
// REMOVED (2026-07-17, ggen-core removal, docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md):
// `inverse_sync` was the sole production-code consumer of ggen-core outside tests
// (ProvenanceEnvelope, InversePipeline, receipt::provenance_envelope::CoherenceReport). No
// ggen-engine/ggen-graph port was ever built for these types; the decision was to abandon
// this functionality rather than port it. File deleted (cmds/inverse_sync.rs); recoverable
// from git history if ever needed.
// ARCHIVED (v26.7.16 routing flip): replaced by ggen-engine's `sync` noun
// (verb `run`). A root FLAT verb named "sync" here and a noun named "sync"
// registered by ggen-engine both become top-level clap subcommands named
// "sync" -- clap_builder's debug_asserts panics on the duplicate name in
// debug builds, and release builds would silently shadow one path with the
// other. REMOVED (2026-07-18, release hardening pass): sync.rs still imported
// ggen_core::* against the fully-deleted ggen-core crate, and was unreachable
// via this mod tree besides. File deleted (cmds/sync.rs); recoverable from
// git history if ever needed.
// pub mod sync;
// REMOVED (2026-07-17, ggen-core removal): wizard.rs imported ggen_core::codegen APIs
// with no ggen-engine port ever built; abandoned rather than ported. File deleted
// (cmds/wizard.rs); recoverable from git history if ever needed.

// Command modules - clap-noun-verb auto-discovery
// ARCHIVED (v26.5.28): a2a/framework/mcp/sigma not provable as finished; gated
// behind default-off `experimental` so they leave the default CLI surface while
// the code is preserved (non-deletion doctrine). See cmds/mod.rs feature note.
#[cfg(feature = "experimental")]
pub mod a2a;
pub mod agent; // AGI-facing lifecycle surface (`ggen agent <verb>`) over crate::agent::PackAgent (ported from ggen_core, T041)
pub mod capability; // capability surfaces → atomic packs (`ggen capability enable/list/inspect`)
                    // ARCHIVED (v26.7.16 routing flip): replaced by ggen-engine's `doctor` noun
                    // (verb `run`). Same root-flat-verb-vs-noun collision as `sync` above.
                    // REMOVED (2026-07-18, release hardening pass): doctor.rs still imported
                    // ggen_core::* against the fully-deleted ggen-core crate, and was
                    // unreachable via this mod tree besides. File deleted (cmds/doctor.rs);
                    // recoverable from git history if ever needed.
                    // pub mod doctor;
#[cfg(feature = "experimental")]
pub mod framework; // Framework bridge commands (LangChain, etc.)
                   // ARCHIVED (v26.7.16 routing flip): replaced by ggen-engine's `graph` noun
                   // (verb `validate`). REMOVED (2026-07-18, release hardening pass): graph.rs
                   // still imported ggen_core::* against the fully-deleted ggen-core crate, and
                   // was unreachable via this mod tree besides. File deleted (cmds/graph.rs);
                   // recoverable from git history if ever needed.
                   // pub mod graph;
#[cfg(feature = "lsp")]
pub mod lsp; // ggen lsp noun (start/check/init/serve/mine/metrics/replay/field-status/emit_pack/verify_pack) — opt-in: --features lsp
#[cfg(feature = "experimental")]
pub mod mcp; // MCP delivered via `ggen lsp serve --protocol mcp` (lsp feature) + ggen-lsp-mcp binary
pub mod ontology; // ggen ontology noun (list/status/info/search) — embedded and marketplace ontology management
pub mod pack; // Singular alias for `packs` noun (golden-path: ggen pack add <name>)
pub mod packs; // lockfile-oriented multi-pack management (`ggen packs install/list/validate/show`)
pub mod packs_receipt; // pack-install receipt emitter (full-closure, fail-closed) — invoked by `pack add`
pub mod policy;
// ARCHIVED (v26.7.16 routing flip): replaced by ggen-engine's `receipt` noun
// (verbs `verify`/`history`). File retained on disk at cmds/receipt.rs, not
// deleted, per this project's fix-forward doctrine.
// pub mod receipt; // ggen receipt verify / info — cryptographic receipt CLI surface (BUG-005)
// REMOVED (2026-07-17, ggen-core removal): sigma.rs imported ggen_core::dflss/manifest
// APIs with no ggen-engine port ever built; abandoned rather than ported. File deleted
// (cmds/sigma.rs); recoverable from git history if ever needed.
pub mod utils;

// ---------------------------------------------------------------------
// Noun `--help` descriptions, registered explicitly for the same reason
// `ggen-engine/src/verbs/mod.rs` (lines 18-32 there) registers its own 5
// nouns this way: clap-noun-verb's `#[verb]` macro can ALSO auto-scrape a
// noun's `about` text by re-reading its source file's `//!` doc comment
// from disk at runtime via a `file!()`-derived path — but that path only
// resolves when the process's current directory happens to be this repo
// checkout. Since `ggen` is invoked from arbitrary target-project
// directories, that scrape silently fails there and the noun `--help`
// line comes back blank. Registering here instead is robust regardless of
// working directory: `__NOUN_REGISTRY` entries all run before
// `__VERB_REGISTRY` entries at first access, and
// `CommandRegistry::register_noun` is first-writer-wins
// (`entry(..).or_insert_with(..)`), so this registration always wins over
// the macro's own runtime scrape attempt. `ggen-cli` is the binary crate
// itself, so no extra force-link shim (like ggen-engine needed) is
// required for these distributed-slice statics to be linked in.
//
// Each `about` string below is copied verbatim from the corresponding
// file's own leading `//!` module doc comment (joined across a
// same-sentence line wrap only where the source has no blank `//!` line
// between them); see that file's own doc comment for authorship. Noun
// keys are the raw file stem with NO kebab-case normalization (confirmed
// in clap-noun-verb-macros 26.7.4's `__auto__` noun-inference branch,
// `Path::file_stem()`).
//
// Only 7 of the 9 files this was originally scoped to are real nouns.
// Confirmed by grepping every `#[verb(...)]`/`#[verb]` attribute in each
// file (bare `#[verb]` = auto-inferred noun = file stem; explicit
// `#[verb(verb_name, noun_name)]` = whatever noun_name says):
//
//   - `init.rs` declares its single verb as `#[verb("init", "root")]` —
//     an explicit ROOT-LEVEL (flat) verb, not a noun. Its `--help` about
//     text already comes from the verb function's own `///` doc comment,
//     parsed by the macro from the AST at COMPILE TIME (`extract_docstring`
//     in clap-noun-verb-macros 26.7.4's `lib.rs`) — never re-read from disk
//     at runtime, so it does not suffer the CWD-dependent bug this block
//     exists to fix. Registering a noun ALSO named "init" here would add a
//     second top-level `ggen init` subcommand alongside the existing root
//     verb of the same name; confirmed live, this hard-panics at startup
//     (`clap_builder`'s `debug_asserts::assert_app`: "Command cli: command
//     name `init` is duplicated"), reproduced both inside this checkout and
//     from `/tmp` before this comment was written. Deliberately omitted.
//   - `git_hooks.rs` has ZERO `#[verb]` functions anywhere in it (confirmed:
//     `grep -n '#\[verb' crates/ggen-cli/src/cmds/git_hooks.rs` returns
//     nothing) — it is a plain helper module (`install_git_hooks` etc.)
//     called from `init.rs`'s `perform_init`, not a CLI noun at all. `ggen
//     git_hooks` is not a real command; registering a noun for it would
//     fabricate a top-level subcommand with no verbs under it. Deliberately
//     omitted. (`crate::run_for_node`'s `KNOWN_NOUNS` list in
//     `crates/ggen-cli/src/lib.rs` includes the hyphenated `"git-hooks"` —
//     that list also includes several other nouns archived out of the
//     default build, e.g. `market`/`lifecycle`/`ci`/`template`/`wizard`; it
//     is a stale aspirational whitelist, not evidence this noun exists.)
// ---------------------------------------------------------------------

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_AGENT_NOUN: fn() = register_agent_noun;
fn register_agent_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "agent",
        "Agent noun — the AGI-facing CLI surface over `crate::agent::PackAgent`.",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_CAPABILITY_NOUN: fn() = register_capability_noun;
fn register_capability_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "capability",
        "Capability noun — resolve and enable capability surfaces (`ggen capability <verb>`).",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_ONTOLOGY_NOUN: fn() = register_ontology_noun;
fn register_ontology_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "ontology",
        "Ontology Commands - Embedded and Marketplace Ontology Management",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_PACK_NOUN: fn() = register_pack_noun;
fn register_pack_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "pack",
        "Pack Commands (singular alias for `packs`)",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_PACKS_NOUN: fn() = register_packs_noun;
fn register_packs_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "packs",
        "Packs noun — lockfile-oriented, multi-pack project management (`ggen packs <verb>`).",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_POLICY_NOUN: fn() = register_policy_noun;
fn register_policy_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "policy",
        "This module provides policy management commands wired to the marketplace layer.",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_UTILS_NOUN: fn() = register_utils_noun;
fn register_utils_noun() {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(
        "utils",
        "Utils Commands - clap-noun-verb v3.4.0 Migration",
    );
}

use crate::prelude::*;

/// Setup and run the command router using clap-noun-verb v26.5.19 auto-discovery
pub fn run_cli() -> Result<()> {
    // Handle --version flag before delegating to clap-noun-verb
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|arg| arg == "--version" || arg == "-V") {
        log::info!("ggen {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }

    // Use clap-noun-verb's auto-discovery to find all [verb] functions
    clap_noun_verb::run().map_err(GgenError::from_clap_error)?;
    Ok(())
}
