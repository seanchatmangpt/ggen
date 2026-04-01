//! E2E: `.ggen/packs.lock` + pack cache → μ pipeline → receipt pack provenance.
//!
//! Chicago TDD: real filesystem, no mocks.

use chrono::Utc;
use ggen_core::packs::lockfile::{LockedPack, PackLockfile, PackSource};
use ggen_core::v6::pipeline::{PipelineConfig, StagedPipeline};
use ggen_core::v6::vocabulary::{AllowedVocabulary, VocabularyRegistry};
use tempfile::TempDir;

struct EnvVarGuard {
    key: &'static str,
    previous: Option<std::ffi::OsString>,
}

impl EnvVarGuard {
    fn set(key: &'static str, value: &std::path::Path) -> Self {
        let previous = std::env::var_os(key);
        std::env::set_var(key, value.as_os_str());
        Self { key, previous }
    }
}

impl Drop for EnvVarGuard {
    fn drop(&mut self) {
        match &self.previous {
            None => std::env::remove_var(self.key),
            Some(v) => std::env::set_var(self.key, v),
        }
    }
}

const PACK_ONTOLOGY: &str = r#"
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex: <http://example.org/pack#> .
ex:PackRoot a rdfs:Resource ;
    rdfs:label "pack e2e" .
"#;

const PACK_CONSTRUCT: &str = r#"PREFIX ex: <http://example.org/pack#>
PREFIX gen: <http://ggen.dev/gen#>
CONSTRUCT {
  ex:PackRoot gen:annotatedBy gen:PackQuery .
}
WHERE {
  ex:PackRoot a ?t .
}"#;

#[test]
fn pack_lockfile_pipeline_populates_receipt_pack_provenance() {
    let project = TempDir::new().expect("project tempdir");
    let cache = TempDir::new().expect("cache tempdir");
    let _guard = EnvVarGuard::set("GGEN_PACK_CACHE_DIR", cache.path());

    let pack_id = "projection-rust";
    let pack_dir = cache.path().join(pack_id);
    std::fs::create_dir_all(pack_dir.join("ontology")).expect("ontology dir");
    std::fs::create_dir_all(pack_dir.join("queries")).expect("queries dir");
    std::fs::write(pack_dir.join("ontology/pack.ttl"), PACK_ONTOLOGY).expect("ontology");
    std::fs::write(pack_dir.join("queries/substrate.rq"), PACK_CONSTRUCT).expect("query");

    let ggen_dir = project.path().join(".ggen");
    std::fs::create_dir_all(&ggen_dir).expect(".ggen");
    let lock_path = ggen_dir.join("packs.lock");

    let mut lf = PackLockfile::new("6.0.1");
    lf.add_pack(
        pack_id.to_string(),
        LockedPack {
            version: "1.0.0".to_string(),
            source: PackSource::Local {
                path: cache.path().to_path_buf(),
            },
            integrity: None,
            installed_at: Utc::now(),
            dependencies: vec![],
        },
    );
    lf.save(&lock_path).expect("save lockfile");

    let config = PipelineConfig::new("pack-e2e", "1.0.0")
        .with_base_path(project.path())
        .with_output_dir("output")
        .with_receipt_path(".ggen/receipt.json");

    let mut pipeline = StagedPipeline::new(config).expect("pipeline new");

    let mut registry = VocabularyRegistry::with_standard_vocabularies();
    registry.add_allowed(
        AllowedVocabulary::new("http://example.org/pack#", "ex").with_description("pack e2e"),
    );
    registry.add_allowed(
        AllowedVocabulary::new("http://ggen.dev/gen#", "gen").with_description("gen ir"),
    );
    pipeline = pipeline.with_vocabulary_registry(registry);

    let receipt = pipeline.run().expect("pipeline run");

    let p0 = receipt
        .packs
        .iter()
        .find(|p| p.pack_id == "projection-rust")
        .expect("projection-rust provenance");
    assert!(
        p0.digest.starts_with("sha256:"),
        "digest should be sha256 hex, got {}",
        p0.digest
    );
    assert!(
        p0.queries_contributed
            .iter()
            .any(|q| q.contains("projection-rust::substrate")),
        "expected pack query name in receipt, got {:?}",
        p0.queries_contributed
    );
    assert_eq!(p0.signature, "local:unsigned");
}
