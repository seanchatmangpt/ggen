#![allow(clippy::unwrap_used)]
//! End-to-End Tests - Complete User Workflows
//!
//! Tests full execution paths from CLI invocation to final output.
//!
//! 80/20 Focus: Real-world user scenarios
//!
//! ARCHIVED SECTIONS (2026-08-03): 12 of this file's original 17 tests invoked CLI nouns
//! that no longer exist in the current binary and have been removed below (not rewritten),
//! because there is no current command doing the same thing to repoint them at. Confirmed
//! live against the current `ggen --help` noun list (`init`, `receipt`, `bblock`, `ontology`,
//! `law`, `mod`, `sbb`, `pack`, `sync`, `packs`, `telco`, `agent`, `wizard`, `policy`,
//! `capability`, `maximalism`, `utils`, `vision2030`, `doctor`, `graph`, `help`): none of
//! `template`, `market`, `project`, `lifecycle`, `ai` exist, and `graph` now has only a
//! `validate` subcommand (no `import`/`query`).
//!
//! Removed, no current equivalent:
//! - `e2e_template_generate_complete`, `e2e_template_with_nested_structure`,
//!   `e2e_scenario_template_development`, `e2e_performance_large_template`: all called
//!   `ggen template generate_tree` (arbitrary YAML-described directory-tree templating with
//!   `--var` substitution). Confirmed live: `ggen template --help` -> `error: unrecognized
//!   subcommand 'template'`. `ggen sync run` is the live code-generation path but is a
//!   fundamentally different command (SPARQL-over-RDF + Tera templates driven by
//!   `ggen.toml`/an ontology, not a standalone YAML tree with `{{var}}` substitution and a
//!   `--var` CLI flag) -- rewriting onto it would assert something these tests were never
//!   about.
//! - `e2e_recovery_invalid_template_graceful`: same `template generate_tree` noun. Also,
//!   even before this archival it was passing for the wrong reason: the command already
//!   failed at argument-parsing (`unrecognized subcommand 'template'`) before ever reaching
//!   the malformed YAML it was meant to test, so its `.failure()` assertion was vacuously
//!   true and was not actually exercising template-parse error recovery.
//! - `e2e_lifecycle_complete_workflow`, `e2e_lifecycle_list_phases`: called `ggen lifecycle
//!   run`/`list` against a `make.toml` phase manifest. Confirmed live: `ggen lifecycle
//!   --help` -> `error: unrecognized subcommand 'lifecycle'`; no current noun manages
//!   phase-based manifests.
//! - `e2e_graph_import_and_query`: called `ggen graph import`/`ggen graph query`. Confirmed
//!   live: `ggen graph --help` now lists only a `validate` subcommand -- `import`/`query`
//!   are gone with no current replacement (`validate` checks RDF/SHACL constraints; it does
//!   not load a graph into a queryable store or run SPARQL against one).
//! - `e2e_project_gen_complete`, `e2e_project_with_git_init`,
//!   `e2e_scenario_new_microservice_project`: called `ggen project gen --name ... --template
//!   rust-cli|rust-lib ...`. Confirmed live: `ggen project --help` -> `error: unrecognized
//!   subcommand 'project'`. `ggen init` is the live project-bootstrap command but scaffolds
//!   a *ggen* codegen project (`schema/domain.ttl` + Tera `templates/`) -- it takes
//!   `--path`/`--name`/`--force`/`--skip-hooks`/`--version`/`--description` and has no
//!   `--template` selector for named archetypes like `rust-cli`/`rust-lib`, so it is not the
//!   same operation. `e2e_scenario_new_microservice_project`'s `market search` and `doctor`
//!   steps do have current equivalents (see the rewritten marketplace tests below, and
//!   `doctor` itself is still a real noun), but its central step -- generate a project from a
//!   named template -- does not, so the scenario as a whole has no current equivalent.
//! - `e2e_ai_generate_template` (`#[cfg(feature = "live-llm-tests")]`, not among the 15
//!   failures under the default `integration` feature since it never compiles without
//!   `live-llm-tests`): called `ggen ai generate`. Confirmed live: `ggen --help` has no `ai`
//!   noun at all.
//!
//! Rewritten to a real current equivalent (verified live, 2026-08-03):
//! - `e2e_marketplace_search_complete`, `e2e_marketplace_search_with_filters`,
//!   `e2e_recovery_network_timeout_graceful`: `ggen market search <query> --limit <n>` ->
//!   `ggen pack search <query> --limit <n>` (confirmed real local-registry search, JSON
//!   `{query, results[], total}`; a query with no matches returns an empty `results` array
//!   with exit 0 -- graceful, same as the old marketplace behavior). `pack search` has no
//!   `--category` flag (only `<QUERY>` and `--limit`), so the `--category backend` filter
//!   from the old `market search microservice --category backend` fixture was dropped rather
//!   than force-fit onto an unsupported flag.
//! - `e2e_marketplace_package_info`: `ggen market info <id>` -> `ggen pack show <id>`
//!   (confirmed real JSON pack detail). The old fixture's `rust-cli-template` id does not
//!   exist in the local registry, and `pack show` on an unknown id is NOT graceful (exit 1,
//!   `Pack '...' not found at marketplace/packs/...`, confirmed live) -- so the fixture id
//!   was changed to `mcp-rust`, a real entry confirmed present via `ggen pack list`.
//! - `e2e_marketplace_list_installed`: `ggen market list` -> `ggen pack list` (confirmed
//!   real JSON array of the local registry's packs, `total: 11` as of this rewrite).

use assert_cmd::Command;

// ============================================================================
// E2E: Marketplace Search and Discovery (noun renamed `market` -> `pack`; see
// the module-level archival note above for what changed and why)
// ============================================================================

#[test]
fn e2e_marketplace_search_complete() {
    Command::cargo_bin("ggen")
        .unwrap()
        .args(["pack", "search", "rust", "--limit", "10"])
        .assert()
        .success();
}

#[test]
fn e2e_marketplace_search_with_filters() {
    Command::cargo_bin("ggen")
        .unwrap()
        .args(["pack", "search", "microservice", "--limit", "5"])
        .assert()
        .success();
}

#[test]
fn e2e_marketplace_package_info() {
    Command::cargo_bin("ggen")
        .unwrap()
        .args(["pack", "show", "mcp-rust"])
        .assert()
        .success();
}

#[test]
fn e2e_marketplace_list_installed() {
    Command::cargo_bin("ggen")
        .unwrap()
        .args(["pack", "list"])
        .assert()
        .success();
}

// ============================================================================
// E2E: Error Recovery Scenarios
// ============================================================================

#[test]
fn e2e_recovery_network_timeout_graceful() {
    // A search query with no matches still succeeds (empty results), it does not error.
    Command::cargo_bin("ggen")
        .unwrap()
        .args([
            "pack",
            "search",
            "nonexistent-package-xyz-123",
            "--limit",
            "1",
        ])
        .assert()
        .success();
}
