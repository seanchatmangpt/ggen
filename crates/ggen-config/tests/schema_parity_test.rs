//! L5 condition 1 (全製品面が意味図式から生成される) drift guard for the "ggen.toml has two
//! schemas" gap documented in `.claude/rules/architecture.md`: `ggen_config::manifest::types::
//! GgenManifest` (this crate) and `ggen_engine::config::GgenConfig` (a sibling crate) are two
//! independently hand-written Rust struct hierarchies that both parse `ggen.toml` and share
//! several top-level table names (`[project]`, `[ontology]`, `[packs]`, `[templates]`,
//! `[law]`) without any shared type or automated cross-check between them.
//!
//! This test does NOT unify the two schemas -- that would be a much larger change than L5
//! condition 1 calls for here. It only prevents *new, silent* drift: a real `syn::parse_file`
//! over both structs' own source text (not a hand-maintained copy of their field lists) is
//! diffed against a frozen "known shared surface" constant. If either struct's field set
//! changes such that the live intersection no longer matches the frozen constant -- a shared
//! field renamed/removed on one side only, or a new field name added to both independently --
//! this test fails loudly and names exactly which field and which struct, rather than staying
//! silent the way `crates/ggen-engine/src/sync.rs`'s runtime dispatch (which parses whichever
//! schema matches, with no cross-check) does today.
//!
//! Chicago TDD: no mocks -- this reads the two real `.rs` source files off disk from the
//! actual git worktree and parses them with the real `syn` parser (same crate/version already
//! vendored by `ggen-cheat-scanner`, see this crate's `Cargo.toml` dev-dependency comment).

use std::collections::BTreeSet;
use std::path::PathBuf;

/// The top-level table names both schemas are currently known to share. This is the frozen
/// baseline the test protects: growing or shrinking the *actual* intersection (computed fresh
/// from source on every run, see `shared_top_level_field_names` below) without a matching,
/// conscious edit to this constant is exactly the "silent divergence" this test exists to
/// catch. If you are here because this test just failed: that is it working, not a bug --
/// go update `.claude/rules/architecture.md`'s "ggen.toml has two schemas" note and this
/// constant together, with an explanation of what changed and why.
const KNOWN_SHARED_TABLE_NAMES: &[&str] = &["project", "ontology", "packs", "templates", "law"];

/// Absolute path to `crates/ggen-config/src/manifest/types.rs` (this crate's own source --
/// `CARGO_MANIFEST_DIR` at test time is `crates/ggen-config`).
fn ggen_manifest_source_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/manifest/types.rs")
}

/// Absolute path to `crates/ggen-engine/src/config.rs` (a sibling crate's source, read as a
/// plain file -- this test intentionally does NOT add a `ggen-engine` crate dependency; the
/// two schemas are independent by design (see architecture.md), and adding a compile-time
/// dependency here would be exactly the kind of coupling condition 1's PARTIAL status warns
/// is still missing, not a step toward closing it).
fn ggen_config_source_path() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../ggen-engine/src/config.rs")
}

/// Parse `source` as a Rust file and return the sorted, de-duplicated set of named-field
/// identifiers on the first `pub struct` whose name is exactly `struct_name`. Panics (via
/// `expect`) if the file fails to parse or the struct is not found -- both are real defects in
/// this test's own assumptions about the source tree, not conditions to swallow.
fn struct_field_names(source: &str, struct_name: &str) -> BTreeSet<String> {
    let file =
        syn::parse_file(source).unwrap_or_else(|e| panic!("failed to parse source as Rust: {e}"));
    for item in &file.items {
        if let syn::Item::Struct(item_struct) = item {
            if item_struct.ident == struct_name {
                return match &item_struct.fields {
                    syn::Fields::Named(named) => named
                        .named
                        .iter()
                        .map(|f| {
                            f.ident
                                .as_ref()
                                .unwrap_or_else(|| panic!("named field with no ident"))
                                .to_string()
                        })
                        .collect(),
                    syn::Fields::Unnamed(_) => panic!(
                        "struct `{struct_name}` found but is a tuple struct, not a named-field struct"
                    ),
                    syn::Fields::Unit => panic!(
                        "struct `{struct_name}` found but is a unit struct, not a named-field struct"
                    ),
                };
            }
        }
    }
    panic!("struct `{struct_name}` not found in source");
}

/// Real, non-mocked read of both struct definitions off disk, parsed with the real `syn`
/// parser. Returns `(ggen_manifest_fields, ggen_config_fields)`.
fn read_both_schemas() -> (BTreeSet<String>, BTreeSet<String>) {
    let manifest_path = ggen_manifest_source_path();
    let config_path = ggen_config_source_path();

    let manifest_src = std::fs::read_to_string(&manifest_path).unwrap_or_else(|e| {
        panic!(
            "failed to read {}: {e} (has GgenManifest moved?)",
            manifest_path.display()
        )
    });
    let config_src = std::fs::read_to_string(&config_path).unwrap_or_else(|e| {
        panic!(
            "failed to read {}: {e} (has GgenConfig moved? this test intentionally reads the \
             sibling crate's source as plain text, not via a Cargo dependency -- see this \
             file's module doc comment)",
            config_path.display()
        )
    });

    let manifest_fields = struct_field_names(&manifest_src, "GgenManifest");
    let config_fields = struct_field_names(&config_src, "GgenConfig");
    (manifest_fields, config_fields)
}

/// Every table name this test currently treats as "known shared" must actually be a field on
/// BOTH structs. This catches the direction the task names explicitly: one schema silently
/// losing a table the other still has (e.g. `GgenConfig` drops `law` in a future refactor
/// while `GgenManifest` keeps it -- today's `sync.rs` schema-dispatch would not notice, this
/// test does).
#[test]
fn known_shared_table_names_exist_on_both_schemas() {
    let (manifest_fields, config_fields) = read_both_schemas();

    let mut missing_from_manifest = Vec::new();
    let mut missing_from_config = Vec::new();
    for name in KNOWN_SHARED_TABLE_NAMES {
        if !manifest_fields.contains(*name) {
            missing_from_manifest.push(*name);
        }
        if !config_fields.contains(*name) {
            missing_from_config.push(*name);
        }
    }

    assert!(
        missing_from_manifest.is_empty(),
        "GgenManifest (crates/ggen-config/src/manifest/types.rs) no longer has field(s) \
         {missing_from_manifest:?}, previously part of the known-shared surface with \
         GgenConfig ({:?}). This is exactly the silent schema-drift condition 1's PARTIAL \
         status warns about -- either restore the field or, if the divergence is intentional, \
         update KNOWN_SHARED_TABLE_NAMES in this test with an explanation.",
        KNOWN_SHARED_TABLE_NAMES
    );
    assert!(
        missing_from_config.is_empty(),
        "GgenConfig (crates/ggen-engine/src/config.rs) no longer has field(s) \
         {missing_from_config:?}, previously part of the known-shared surface with \
         GgenManifest ({:?}). This is exactly the silent schema-drift condition 1's PARTIAL \
         status warns about -- either restore the field or, if the divergence is intentional, \
         update KNOWN_SHARED_TABLE_NAMES in this test with an explanation.",
        KNOWN_SHARED_TABLE_NAMES
    );
}

/// The *actual* intersection of the two structs' field-name sets, computed fresh from source
/// on every run, must equal the frozen `KNOWN_SHARED_TABLE_NAMES` exactly -- not just be a
/// superset of it. This is the half of the guard `known_shared_table_names_exist_on_both_schemas`
/// does not cover: a brand-new field name added independently to both structs (e.g. both grow
/// an `[observability]` table under different authors, coincidentally using the same name)
/// would silently pass that test but is exactly the kind of undetected convergence-without-
/// coordination this condition needs surfaced, not waved through.
#[test]
fn shared_field_intersection_matches_frozen_baseline_exactly() {
    let (manifest_fields, config_fields) = read_both_schemas();
    let known: BTreeSet<String> = KNOWN_SHARED_TABLE_NAMES
        .iter()
        .map(|s| s.to_string())
        .collect();

    let actual_intersection: BTreeSet<String> = manifest_fields
        .intersection(&config_fields)
        .cloned()
        .collect();

    let newly_shared: BTreeSet<&String> = actual_intersection.difference(&known).collect();
    let no_longer_shared: BTreeSet<&String> = known.difference(&actual_intersection).collect();

    assert!(
        newly_shared.is_empty() && no_longer_shared.is_empty(),
        "GgenManifest/GgenConfig's actual shared top-level field-name intersection has drifted \
         from the frozen KNOWN_SHARED_TABLE_NAMES baseline {known:?}.\n\
         Newly shared (present in both structs' source, not yet in the baseline): {newly_shared:?}\n\
         No longer shared (baseline says both have it, one no longer does): {no_longer_shared:?}\n\
         This is the silent-drift condition this test exists to catch -- inspect both structs' \
         source (crates/ggen-config/src/manifest/types.rs's GgenManifest, \
         crates/ggen-engine/src/config.rs's GgenConfig), confirm whether the change is \
         intentional, and if so update KNOWN_SHARED_TABLE_NAMES here with an explanation."
    );
}

/// Sanity check on this test's own assumption: fail loudly (not silently return empty sets) if
/// either source file becomes unreadable or the target struct disappears, so a future rename
/// of `GgenManifest`/`GgenConfig` themselves is caught here too, not just their fields.
#[test]
fn both_schema_structs_are_still_named_and_parseable() {
    let (manifest_fields, config_fields) = read_both_schemas();
    assert!(
        !manifest_fields.is_empty(),
        "GgenManifest parsed with zero named fields -- did the struct definition change shape?"
    );
    assert!(
        !config_fields.is_empty(),
        "GgenConfig parsed with zero named fields -- did the struct definition change shape?"
    );
}
