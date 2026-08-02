//! Command router for clap-noun-verb auto-discovery.
//!
//! Command modules expose `#[verb]` functions. Noun descriptions are registered
//! explicitly because runtime source scraping is not reliable when `ggen` is
//! invoked outside this repository checkout.

pub mod git_hooks;
pub mod helpers;
pub mod init;

#[cfg(feature = "experimental")]
pub mod a2a;
pub mod agent;
pub mod bblock;
pub mod capability;
#[cfg(feature = "experimental")]
pub mod framework;
#[cfg(feature = "lsp")]
pub mod lsp;
#[cfg(feature = "experimental")]
pub mod mcp;
pub mod ontology;
pub mod pack;
pub mod packs;
pub mod packs_receipt;
pub mod policy;
pub mod sbb;
pub mod utils;

fn register_noun(name: &'static str, about: &'static str) {
    ::clap_noun_verb::cli::registry::CommandRegistry::register_noun(name, about);
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_AGENT_NOUN: fn() = register_agent_noun;
fn register_agent_noun() {
    register_noun(
        "agent",
        "Agent noun — the AGI-facing CLI surface over `crate::agent::PackAgent`.",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_BBLOCK_NOUN: fn() = register_bblock_noun;
fn register_bblock_noun() {
    register_noun(
        "bblock",
        "Fortune 5 deployment building blocks and provider-specific pack groups.",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_CAPABILITY_NOUN: fn() = register_capability_noun;
fn register_capability_noun() {
    register_noun(
        "capability",
        "Capability noun — resolve and enable capability surfaces (`ggen capability <verb>`).",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_ONTOLOGY_NOUN: fn() = register_ontology_noun;
fn register_ontology_noun() {
    register_noun(
        "ontology",
        "Ontology Commands - Embedded and Marketplace Ontology Management",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_PACK_NOUN: fn() = register_pack_noun;
fn register_pack_noun() {
    register_noun("pack", "Pack Commands (singular alias for `packs`)");
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_PACKS_NOUN: fn() = register_packs_noun;
fn register_packs_noun() {
    register_noun(
        "packs",
        "Packs noun — lockfile-oriented, multi-pack project management (`ggen packs <verb>`).",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_POLICY_NOUN: fn() = register_policy_noun;
fn register_policy_noun() {
    register_noun(
        "policy",
        "Policy management commands wired to the marketplace layer.",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_SBB_NOUN: fn() = register_sbb_noun;
fn register_sbb_noun() {
    register_noun(
        "sbb",
        "Solution Building Block capability density, distribution, receipts, and replay.",
    );
}

#[linkme::distributed_slice(::clap_noun_verb::cli::registry::__NOUN_REGISTRY)]
static REGISTER_UTILS_NOUN: fn() = register_utils_noun;
fn register_utils_noun() {
    register_noun("utils", "Utils Commands - clap-noun-verb migration surface");
}

use crate::prelude::*;

/// Setup and run the command router using clap-noun-verb auto-discovery.
pub fn run_cli() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.iter().any(|arg| arg == "--version" || arg == "-V") {
        log::info!("ggen {}", env!("CARGO_PKG_VERSION"));
        return Ok(());
    }
    clap_noun_verb::run().map_err(GgenError::from_clap_error)?;
    Ok(())
}
