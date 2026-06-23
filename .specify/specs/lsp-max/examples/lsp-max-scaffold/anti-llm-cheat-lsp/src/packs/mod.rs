// Pack modules compiled from lsp.ttl — one submodule per rule pack.
pub mod cheat_surface;
pub mod cheat_tests;
pub mod cheat_receipts;
pub mod cheat_claims;
pub mod cheat_smells;

use lsp_max::RulePack;

/// All rule packs for this server, compiled from the ontology.
/// Used by the CLI (`packs list`, `rules list`) and the workspace scanner.
pub fn all_packs() -> Vec<RulePack> {
    vec![
        cheat_surface::pack(),
        cheat_tests::pack(),
        cheat_receipts::pack(),
        cheat_claims::pack(),
        cheat_smells::pack(),
    ]
}