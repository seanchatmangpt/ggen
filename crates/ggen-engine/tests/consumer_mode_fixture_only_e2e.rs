//! Chicago-TDD end-to-end proof of consumer-mode fixtureOnly emission
//! suppression (OS-13, WP-5 consumer half).
//!
//! Real filesystem, real GraphLaw graph engine, real SPARQL extraction,
//! real Tera rendering, real `ggen_engine::sync` pipeline — no mocks.
//! A fixture pack marks one `AshExtensionSpec` individual
//! `aex:fixtureOnly true` (the exact idiom ggen-marketplace's
//! ash-extension packs use); the tests prove:
//!
//! 1. Default (flag off): current behavior — both specs fan out into
//!    `lib/` installers.
//! 2. Opt-in flag (`SyncOptions.consumer_mode = true`): the fixture-marked
//!    spec's installer is suppressed with a typed skip naming the spec IRI,
//!    while the live spec still emits.
//! 3. Permanent config key (`[templates] consumer_mode = true`): same
//!    suppression with `SyncOptions.consumer_mode = false` — the consumer
//!    pins the protection in its manifest.
//! 4. Whole-file projection whose query touches a fixture-marked spec is
//!    suppressed whole.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const PACK_ONTOLOGY: &str = r#"@prefix aex: <http://example.com/aex#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

aex:fixtureOnly a rdf:Property ;
    rdfs:domain aex:AshExtensionSpec ;
    rdfs:comment "boolean marker for pack-owned worked examples. fixtureOnly=true individuals remain available to gates/audits but MUST NOT fan out consumer artifacts." .

aex:AuditTrailSpec a aex:AshExtensionSpec ;
    rdfs:label "audit_trail" ;
    aex:fixtureOnly true .

aex:LiveSpec a aex:AshExtensionSpec ;
    rdfs:label "live" .
"#;

const FANOUT_TEMPLATE: &str = r#"---
to: "lib/{{ label }}.ex"
skip_empty: true
sparql: |
  PREFIX aex: <http://example.com/aex#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  SELECT ?s ?label WHERE {
    ?s a aex:AshExtensionSpec ;
       rdfs:label ?label .
  }
---
defmodule App do
  def spec, do: "{{ label }}"
end
"#;

const WHOLE_TEMPLATE: &str = r#"---
to: "lib/all_specs.ex"
skip_empty: true
sparql: |
  PREFIX aex: <http://example.com/aex#>
  PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
  SELECT ?s ?label WHERE {
    ?s a aex:AshExtensionSpec ;
       rdfs:label ?label .
  }
---
# All specs: {% for r in results %}{{ r.label }} {% endfor %}
"#;

fn write_fixture_pack(root: &Path, name: &str) {
    let pack_dir = root.join(name);
    std::fs::create_dir_all(pack_dir.join("templates")).expect("mkdir pack templates");
    std::fs::write(
        pack_dir.join("pack.toml"),
        format!(
            "[pack]\nname = \"{name}\"\nversion = \"0.1.0\"\ndescription = \"test pack {name}\"\n"
        ),
    )
    .expect("write pack.toml");
    std::fs::write(pack_dir.join("ontology.ttl"), PACK_ONTOLOGY).expect("write ontology.ttl");
    std::fs::write(pack_dir.join("templates/specs.ex.tmpl"), FANOUT_TEMPLATE)
        .expect("write fanout template");
    std::fs::write(pack_dir.join("templates/all_specs.ex.tmpl"), WHOLE_TEMPLATE)
        .expect("write whole template");
}

fn write_consumer(root: &Path, name: &str, templates_table_extra: &str) -> PathBuf {
    let project = root.join(name);
    std::fs::create_dir_all(project.join("templates")).expect("mkdir templates");
    std::fs::write(project.join("ontology.ttl"), "").expect("write ontology.ttl");
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"{name}\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\nfixpack = {{ path = \"../fixpack\" }}\n\n\
             [templates]\ndir = \"templates\"{templates_table_extra}\n"
        ),
    )
    .expect("write ggen.toml");
    project
}

fn options(consumer_mode: bool) -> SyncOptions {
    SyncOptions {
        consumer_mode,
        ..Default::default()
    }
}

fn read_report_skip_targets(report: &ggen_engine::sync::SyncReport) -> Vec<String> {
    report
        .skipped
        .iter()
        .map(|(p, _)| p.display().to_string())
        .collect()
}

#[test]
fn default_off_emits_both_specs_into_lib() {
    let dir = TempDir::new().expect("tempdir");
    write_fixture_pack(dir.path(), "fixpack");
    let project = write_consumer(dir.path(), "consumer", "");

    let report = sync(&project, options(false)).expect("sync must succeed");
    assert!(
        project.join("lib/audit_trail.ex").is_file(),
        "default (consumer-mode off) must still emit the fixture-marked spec's installer"
    );
    assert!(project.join("lib/live.ex").is_file());
    assert!(
        report.skipped.is_empty(),
        "no skips expected with the flag off: {:?}",
        read_report_skip_targets(&report)
    );
}

#[test]
fn consumer_mode_flag_suppresses_fixture_spec_but_emits_live_spec() {
    let dir = TempDir::new().expect("tempdir");
    write_fixture_pack(dir.path(), "fixpack");
    let project = write_consumer(dir.path(), "consumer", "");

    let report = sync(&project, options(true)).expect("sync must succeed");

    assert!(
        !project.join("lib/audit_trail.ex").exists(),
        "fixtureOnly-marked spec's installer must be suppressed in consumer mode"
    );
    assert!(
        project.join("lib/live.ex").is_file(),
        "unmarked (live) spec must still emit in consumer mode"
    );

    let audit_skips: Vec<_> = report
        .skipped
        .iter()
        .filter(|(p, _)| p.display().to_string().contains("audit_trail"))
        .collect();
    assert_eq!(
        audit_skips.len(),
        1,
        "exactly one typed skip for the fixture-marked spec: {:?}",
        report.skipped
    );
    let (_, reason) = audit_skips[0];
    assert!(
        reason.contains("consumer-mode") && reason.contains("fixtureOnly"),
        "skip reason must be typed, got: {reason}"
    );
    assert!(
        reason.contains("http://example.com/aex#AuditTrailSpec"),
        "skip reason must name the marked spec IRI, got: {reason}"
    );
}

#[test]
fn config_key_consumer_mode_is_permanent_per_consumer() {
    let dir = TempDir::new().expect("tempdir");
    write_fixture_pack(dir.path(), "fixpack");
    let project = write_consumer(dir.path(), "consumer", "\nconsumer_mode = true");

    let report = sync(&project, options(false)).expect("sync must succeed");

    assert!(
        !project.join("lib/audit_trail.ex").exists(),
        "config-key consumer mode must suppress the fixture-marked spec's installer"
    );
    assert!(project.join("lib/live.ex").is_file());
    assert!(
        report.skipped.len() == 2,
        "both fixture-derived outputs must be skipped: {:?}",
        report.skipped
    );
    assert!(
        !project.join("lib/all_specs.ex").exists(),
        "whole projection must be suppressed under config-key consumer mode too"
    );
}

#[test]
fn whole_file_projection_touching_fixture_spec_is_suppressed() {
    let dir = TempDir::new().expect("tempdir");
    // Pack with ONLY the whole-file projection template (no fan-out).
    let pack_dir = dir.path().join("fixpack");
    std::fs::create_dir_all(pack_dir.join("templates")).expect("mkdir pack templates");
    std::fs::write(
        pack_dir.join("pack.toml"),
        "[pack]\nname = \"fixpack\"\nversion = \"0.1.0\"\ndescription = \"test pack\"\n",
    )
    .expect("write pack.toml");
    std::fs::write(pack_dir.join("ontology.ttl"), PACK_ONTOLOGY).expect("write ontology.ttl");
    std::fs::write(pack_dir.join("templates/all_specs.ex.tmpl"), WHOLE_TEMPLATE)
        .expect("write whole template");
    let project = write_consumer(dir.path(), "consumer", "");

    let report = sync(&project, options(true)).expect("sync must succeed");

    assert!(
        !project.join("lib/all_specs.ex").exists(),
        "whole-file projection naming a fixture-marked spec must be suppressed"
    );
    let (_, reason) = &report.skipped[0];
    assert!(
        reason.contains("consumer-mode")
            && reason.contains("http://example.com/aex#AuditTrailSpec"),
        "typed skip must name the marked spec IRI, got: {reason}"
    );
}
