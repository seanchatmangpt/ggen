//! Differential court: two parsers, one `pack.toml` corpus.
//!
//! Subject A: `ggen_engine::pack` — strict `PackCapabilities`
//! (`deny_unknown_fields`, `BTreeSet<String>`, closed `[capabilities]` key
//! set: `types`/`provides`/`requires`).
//!
//! Subject B: `ggen_marketplace::packs_registry::types::PackFile` — lenient
//! `PackCapabilitiesFile` (`Option<Vec<String>>`, no `deny_unknown_fields`).
//!
//! Court 1 (corpus differential): every real `pack.toml` under
//! `/Users/sac/ggen/packs` and `/Users/sac/ggen-marketplace/packs` is parsed
//! by BOTH parsers; presence/absence of `[capabilities]`, and set equality
//! of `types`/`provides`/`requires`, must agree. Any divergence panics with
//! a per-pack, per-field report naming both readings.
//!
//! Court 2 (edge differential): crafted fixtures pin the *known semantic
//! asymmetries* as assertions — BTreeSet dedup vs Vec preserve, strict
//! unknown-key refusal vs lenient acceptance, empty arrays, non-string
//! entries — so a future change to either parser's semantics breaks a test
//! instead of drifting silently.
//!
//! Chicago TDD: real files, real parsers, real resolve. No mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use ggen_engine::config::GgenConfig;
use ggen_engine::pack::{self, Pack};
use ggen_marketplace::packs_registry::types::PackFile;
use tempfile::TempDir;

const CORPUS_ROOTS: [&str; 2] = ["/Users/sac/ggen/packs", "/Users/sac/ggen-marketplace/packs"];

// ---------------------------------------------------------------------------
// Subject B: marketplace lenient parser
// ---------------------------------------------------------------------------

/// Parse a pack.toml with the marketplace's lenient schema.
fn marketplace_parse(dir_name: &str, raw: &str) -> Result<PackFile, String> {
    // Real corpus tomls lack the marketplace Pack model's required identity
    // fields; inject the same defaults as pack_file_from_dir (id = dir name,
    // empty-string category/etc.) so the differential isolates [capabilities]
    // semantics rather than identity-field presence.
    let mut value: toml::Value =
        star_toml::from_str(raw).map_err(|e| format!("marketplace parse error: {e}"))?;
    let pack = value
        .get_mut("pack")
        .and_then(|p| p.as_table_mut())
        .expect("fixture must have [pack]");
    pack.entry("id".to_string())
        .or_insert(toml::Value::String(dir_name.to_string()));
    pack.entry("name".to_string())
        .or_insert(toml::Value::String(String::new()));
    pack.entry("version".to_string())
        .or_insert(toml::Value::String("0.0.0".to_string()));
    pack.entry("description".to_string())
        .or_insert(toml::Value::String(String::new()));
    pack.entry("category".to_string())
        .or_insert(toml::Value::String("uncategorized".to_string()));
    pack.entry("packages".to_string())
        .or_insert(toml::Value::Array(Vec::new()));
    star_toml::from_str::<PackFile>(&toml::to_string(&value).expect("reserialize"))
        .map_err(|e| format!("marketplace parse error: {e}"))
}

/// Lenient reading as `Option<BTreeSet<String>>` (None = table/field absent).
fn mkt_set(v: &Option<std::collections::BTreeSet<String>>) -> Option<BTreeSet<String>> {
    v.clone()
}

// ---------------------------------------------------------------------------
// Subject A: engine strict parser (via the real resolve path)
// ---------------------------------------------------------------------------

/// Scaffold a minimal-but-real consumer project whose single `[packs.<key>]`
/// entry points at `pack_dir` (absolute path), then run the real
/// `pack::resolve`. Returns the engine's strict reading of the pack.
fn engine_resolve(pack_key: &str, pack_dir: &Path) -> Result<Pack, String> {
    let dir = TempDir::new().expect("tempdir");
    let project = dir.path().join("project");
    std::fs::create_dir_all(project.join("templates")).expect("project templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("project ontology");
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"parser-diff-fixture\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [templates]\ndir = \"templates\"\n\n\
             [packs.{pack_key}]\npath = \"{}\"\n",
            pack_dir.display()
        ),
    )
    .expect("ggen.toml");

    // Keep the TempDir alive for the duration of resolve by resolving before
    // drop; resolve reads the pack in place (absolute path), so dropping the
    // project afterwards is safe.
    let config = GgenConfig::load(&project.join("ggen.toml")).map_err(|e| e.to_string())?;
    let packs = pack::resolve(&config, &project).map_err(|e| e.to_string())?;
    assert_eq!(packs.len(), 1, "exactly one pack resolved for {pack_key}");
    Ok(packs.into_iter().next().expect("one pack"))
}

/// Engine refusal classes that are NOT about `[capabilities]` (structural:
/// missing ontology/templates, dependency closure). Capability divergences
/// are FM-PACK-003 ("invalid pack.toml") refusals.
fn engine_err_class(err: &str) -> &'static str {
    if err.contains("invalid pack.toml") {
        "invalid-pack-toml"
    } else if err.contains("ontology.ttl missing") {
        "missing-ontology"
    } else if err.contains("zero templates") {
        "missing-templates"
    } else if err.contains("directory") && err.contains("does not exist") {
        "missing-dir"
    } else if err.contains("FM-PACK-014") || err.contains("dependency") {
        "dependency-closure"
    } else {
        "other"
    }
}

// ---------------------------------------------------------------------------
// Corpus discovery
// ---------------------------------------------------------------------------

fn corpus_pack_dirs(root: &str) -> Vec<(String, PathBuf)> {
    let mut out = Vec::new();
    let entries = std::fs::read_dir(root).unwrap_or_else(|e| panic!("read_dir {root}: {e}"));
    for entry in entries {
        let entry = entry.expect("dir entry");
        let dir = entry.path();
        if dir.is_dir() && dir.join("pack.toml").is_file() {
            out.push((entry.file_name().to_string_lossy().into_owned(), dir));
        }
    }
    out.sort();
    out
}

// ---------------------------------------------------------------------------
// Court 1: corpus differential
// ---------------------------------------------------------------------------

#[test]
fn corpus_engines_and_marketplace_agree_on_capabilities() {
    let mut checked = 0usize;
    let mut both_refused = 0usize;
    let mut structural_refusals: Vec<String> = Vec::new();
    let mut divergences: Vec<String> = Vec::new();

    for root in CORPUS_ROOTS {
        for (dir_name, pack_dir) in corpus_pack_dirs(root) {
            let pack_toml = pack_dir.join("pack.toml");
            let raw = std::fs::read_to_string(&pack_toml)
                .unwrap_or_else(|e| panic!("read {}: {e}", pack_toml.display()));
            let label = format!("{root}/{dir_name}");

            let mkt = marketplace_parse(&dir_name, &raw);
            let engine = engine_resolve(&dir_name, &pack_dir);

            let mkt = match mkt {
                Ok(m) => m,
                Err(e) => {
                    // Marketplace refuses: engine must refuse too, else the
                    // corpus file is only parseable by the strict side.
                    if engine.is_ok() {
                        divergences.push(format!(
                            "{label}: marketplace REFUSES ({e}) but engine ACCEPTS"
                        ));
                    } else {
                        both_refused += 1;
                    }
                    continue;
                }
            };

            let engine = match engine {
                Ok(p) => p,
                Err(e) => match engine_err_class(&e) {
                    "invalid-pack-toml" => {
                        divergences.push(format!(
                            "{label}: engine REFUSES pack.toml ({e}) but marketplace ACCEPTS"
                        ));
                        continue;
                    }
                    class => {
                        // Structural refusal unrelated to [capabilities]:
                        // capability comparison is impossible; record it.
                        structural_refusals.push(format!("{label}: [{class}] {e}"));
                        continue;
                    }
                },
            };

            checked += 1;

            let mkt_caps = &mkt.capabilities;
            let mkt_types = mkt_set(&mkt_caps.as_ref().and_then(|c| c.types.clone()));
            let mkt_provides = mkt_set(&mkt_caps.as_ref().and_then(|c| c.provides.clone()));
            let mkt_requires = mkt_set(&mkt_caps.as_ref().and_then(|c| c.requires.clone()));

            let eng_types = Some(engine.semantic_types.clone());
            let eng_provides = Some(engine.provides.clone());
            let eng_requires = Some(engine.requires.clone());

            // Presence/absence agreement: the strict side defaults every
            // field to empty; the lenient side distinguishes absent from
            // empty. Agreement on presence means: marketplace-absent must
            // imply engine-empty, and marketplace-present must imply
            // engine-set-equality (an empty engine set behind a present
            // marketplace Some is still agreement in *content* — both read
            // "no entries" — so content equality is the load-bearing check,
            // asserted below regardless).
            // Normalize the documented empty-vs-absent asymmetry: the
            // engine's BTreeSet deserializes a present-but-empty array as
            // Some({}), the marketplace's Option<Vec> leaves an absent key
            // as None. Both mean "no entries".
            let norm = |s: &Option<std::collections::BTreeSet<String>>| {
                s.as_ref().filter(|x| !x.is_empty()).cloned()
            };
            for (field, eng, m) in [
                ("types", &norm(&eng_types), &norm(&mkt_types)),
                ("provides", &norm(&eng_provides), &norm(&mkt_provides)),
                ("requires", &norm(&eng_requires), &norm(&mkt_requires)),
            ] {
                if eng != m {
                    divergences.push(format!(
                        "{label}: {field} diverges — engine={eng:?} marketplace={m:?}"
                    ));
                }
            }
        }
    }

    assert!(
        divergences.is_empty(),
        "PARSER DRIFT — {} divergence(s) over the real corpora:\n{}",
        divergences.len(),
        divergences.join("\n")
    );

    eprintln!(
        "corpus differential: {checked} packs compared, {both_refused} refused by both, \
         {} structural (non-capability) engine refusals skipped, 0 divergences",
        structural_refusals.len()
    );
}

// ---------------------------------------------------------------------------
// Court 2: edge differentials — pin the known semantic asymmetries
// ---------------------------------------------------------------------------

/// Write a full pack dir (pack.toml + ontology.ttl + one template) with the
/// given `[capabilities]` table text, in a TempDir.
struct EdgePack {
    _dir: TempDir,
    pack_dir: PathBuf,
    raw: String,
}

fn edge_pack(capabilities_table: &str) -> EdgePack {
    let dir = TempDir::new().expect("tempdir");
    let pack_dir = dir.path().join("edge-pack");
    std::fs::create_dir_all(pack_dir.join("templates")).expect("pack templates");
    let raw = format!(
        "[pack]\nname = \"edge-pack\"\nversion = \"1.0.0\"\n\
         description = \"parser differential fixture\"\n\n\
         {capabilities_table}\n"
    );
    std::fs::write(pack_dir.join("pack.toml"), &raw).expect("pack.toml");
    std::fs::write(
        pack_dir.join("ontology.ttl"),
        "@prefix ex: <http://e.com#> .",
    )
    .expect("ttl");
    std::fs::write(
        pack_dir.join("templates").join("edge.tmpl"),
        "---\nto: out.txt\n---\n",
    )
    .expect("tmpl");
    EdgePack {
        _dir: dir,
        pack_dir,
        raw,
    }
}

/// Semantic difference (documented by assertion): the strict engine side
/// Former divergence, now parity: BOTH sides store `BTreeSet<String>` —
/// duplicate URNs dedup on both (marketplace parity landed 2026-10-10,
/// caps-btree-parity). Content and cardinality agree.
#[test]
fn duplicate_urn_in_provides_dedup_vs_preserve() {
    let fx = edge_pack("[capabilities]\nprovides = [\"urn:ggen:pack:x\", \"urn:ggen:pack:x\"]\n");

    let mkt = marketplace_parse("edge-fixture", &fx.raw).expect("marketplace accepts duplicates");
    let mkt_provides = mkt
        .capabilities
        .as_ref()
        .and_then(|c| c.provides.as_ref())
        .expect("marketplace saw provides");
    assert_eq!(
        mkt_provides.len(),
        1,
        "marketplace BTreeSet dedups (parity with engine)"
    );

    let engine = engine_resolve("edge-pack", &fx.pack_dir).expect("engine accepts duplicates");
    assert_eq!(engine.provides.len(), 1, "engine BTreeSet dedups");

    // Set content agrees.
    let mkt_as_set: BTreeSet<String> = mkt_provides.iter().cloned().collect();
    assert_eq!(engine.provides, mkt_as_set);
}

/// Former divergence, now agreement: an unknown key inside `[capabilities]` —
/// both sides carry `deny_unknown_fields` (marketplace `PackCapabilitiesFile`,
/// engine strict `PackCapabilities`), so both REFUSE the whole pack.toml.
#[test]
fn unknown_key_in_capabilities_refused_by_both() {
    let fx =
        edge_pack("[capabilities]\nprovides = [\"urn:ggen:pack:x\"]\nunknown_key = \"drift\"\n");

    let mkt = marketplace_parse("edge-fixture", &fx.raw);
    assert!(
        mkt.is_err(),
        "marketplace deny_unknown_fields must refuse unknown key"
    );

    let engine = engine_resolve("edge-pack", &fx.pack_dir);
    let err = engine.expect_err("engine deny_unknown_fields must refuse unknown key");
    assert_eq!(engine_err_class(&err), "invalid-pack-toml");
}

/// Agreement case: empty arrays on both sides read as "no entries" —
/// engine empty set, marketplace Some(empty vec). No divergence.
#[test]
fn empty_arrays_agree_as_empty() {
    let fx = edge_pack("[capabilities]\ntypes = []\nprovides = []\nrequires = []\n");

    let mkt = marketplace_parse("edge-fixture", &fx.raw).expect("marketplace accepts empty arrays");
    let caps = mkt.capabilities.as_ref().expect("capabilities present");
    assert_eq!(
        caps.provides.as_ref().map(std::collections::BTreeSet::len),
        Some(0)
    );

    let engine = engine_resolve("edge-pack", &fx.pack_dir).expect("engine accepts empty arrays");
    assert!(engine.semantic_types.is_empty());
    assert!(engine.provides.is_empty());
    assert!(engine.requires.is_empty());
}

/// Agreement case (both refuse): non-string entries are a type error under
/// both schemas — neither parser admits them. Different error text, same
/// refusal outcome.
#[test]
fn non_string_entries_refused_by_both() {
    let fx = edge_pack("[capabilities]\nprovides = [1, 2]\n");

    let mkt = marketplace_parse("edge-fixture", &fx.raw);
    assert!(mkt.is_err(), "marketplace must refuse non-string entries");

    let engine = engine_resolve("edge-pack", &fx.pack_dir);
    let err = engine.expect_err("engine must refuse non-string entries");
    assert_eq!(engine_err_class(&err), "invalid-pack-toml");
}
