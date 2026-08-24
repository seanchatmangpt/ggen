//! Chicago-TDD end-to-end proof for `packs/autofde-execution-profile-pack`.
//!
//! Uses the real filesystem, graph engine, SPARQL gates, Tera renderer and
//! `ggen_engine::sync` pipeline. It proves generation, outer-JSON safety,
//! exact revision/source/materialization binding, non-vacuous verification,
//! idempotency, and fail-closed refusal when a profile is incomplete or
//! attempts to carry an authority token. No actuation exists in this pack or test.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};

use ggen_engine::sync::{sync, SyncOptions};
use support::{assert_gate_refuses, assert_idempotent, read, read_json, scaffold_pack_with_ontology};

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

const PROFILE: &str = r#"@prefix afxp: <http://seanchatmangpt.github.io/packs/autofde-execution-profile#> .
@prefix dcterms: <http://purl.org/dc/terms/> .
@prefix prov: <http://www.w3.org/ns/prov#> .

<urn:test:memory-counter> a prov:Plan ;
    dcterms:type afxp:GymActExecutionProfile ;
    dcterms:identifier "memory-counter" ;
    dcterms:source <urn:test:benchmark-source> ;
    prov:wasDerivedFrom <urn:test:experiment-plan> ;
    afxp:provider "memory" ;
    afxp:benchmarkRevision "0123456789abcdef0123456789abcdef01234567" ;
    afxp:scenario "" ;
    afxp:configJson "{\"initial\":{\"counter\":0}}" ;
    afxp:capabilityRef "" ;
    afxp:capabilityBinding "increment" ;
    afxp:payloadJson "{\"key\":\"counter\",\"amount\":1}" ;
    afxp:expectedJson "{\"counter\":1}" ;
    afxp:inputSchemaJson "{\"type\":\"object\"}" ;
    afxp:authorityRef "" ;
    afxp:actionRef "urn:test:action:increment" .
"#;

fn parse_inner(profile: &serde_json::Value, key: &str) -> serde_json::Value {
    serde_json::from_str(
        profile[key]
            .as_str()
            .unwrap_or_else(|| panic!("{key} must be a JSON lexical string")),
    )
    .unwrap_or_else(|e| panic!("{key} must contain valid JSON: {e}"))
}

#[test]
fn execution_profile_pack_generates_powerless_revision_bound_json_and_is_idempotent() {
    let (_dir, project) = scaffold_pack_with_ontology(
        &packs_dir().join("autofde-execution-profile-pack"),
        PROFILE,
    );

    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("real sync must manufacture execution profile JSON");

    let value = read_json(&project, "generated/autofde/execution-profiles.json");
    assert_eq!(value["schema"], "urn:autofde:execution-profile:v1");
    assert_eq!(value["generated_by"], "ggen:autofde-execution-profile-pack");
    assert_eq!(value["authority_mode"], "external-only");
    let profiles = value["profiles"].as_array().expect("profiles array");
    assert_eq!(profiles.len(), 1);
    let profile = &profiles[0];
    assert_eq!(profile["profile_id"], "memory-counter");
    assert_eq!(profile["provider"], "memory");
    assert_eq!(
        profile["benchmark_revision"],
        "0123456789abcdef0123456789abcdef01234567"
    );
    assert_eq!(profile["source_ref"], "urn:test:benchmark-source");
    assert_eq!(profile["derived_from"], "urn:test:experiment-plan");
    assert_eq!(parse_inner(profile, "config_json")["initial"]["counter"], 0);
    assert!(profile["capability_ref"].is_null());
    assert_eq!(profile["capability_binding"], "increment");
    assert_eq!(parse_inner(profile, "payload_json")["key"], "counter");
    assert_eq!(parse_inner(profile, "expected_json")["counter"], 1);
    assert_eq!(parse_inner(profile, "input_schema_json")["type"], "object");
    assert!(profile["authority_ref"].is_null());

    let generated = read(&project, "generated/autofde/execution-profiles.json");
    for forbidden in ["\"principal\"", "\"nonce\"", "\"expires_at\"", "\"execution_grant\""] {
        assert!(
            !generated.contains(forbidden),
            "powerless profile output must not contain authority token field {forbidden}: {generated}"
        );
    }

    assert_idempotent(&project);
}

#[test]
fn execution_profile_pack_escapes_malformed_inner_json_instead_of_injecting_outer_json() {
    let malformed = PROFILE.replace(
        "afxp:configJson \"{\\\"initial\\\":{\\\"counter\\\":0}}\"",
        "afxp:configJson \"{not-json}\"",
    );
    let (_dir, project) = scaffold_pack_with_ontology(
        &packs_dir().join("autofde-execution-profile-pack"),
        &malformed,
    );

    sync(
        &project,
        SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("powerless manufacturer must safely serialize lexical data");

    let value = read_json(&project, "generated/autofde/execution-profiles.json");
    assert_eq!(value["profiles"][0]["config_json"], "{not-json}");
    assert!(serde_json::from_str::<serde_json::Value>("{not-json}").is_err());
}

#[test]
fn execution_profile_pack_refuses_vacuous_verification_or_ambiguous_selector() {
    let (_dir, project) = scaffold_pack_with_ontology(
        &packs_dir().join("autofde-execution-profile-pack"),
        PROFILE,
    );

    let sabotage = PROFILE
        .replace("afxp:capabilityRef \"\"", "afxp:capabilityRef \"urn:test:capability\"")
        .replace("afxp:expectedJson \"{\\\"counter\\\":1}\"", "afxp:expectedJson \"{}\"");
    assert_gate_refuses(&project, &sabotage, "020_selector_and_verification");
}

#[test]
fn execution_profile_pack_refuses_authority_token_facts() {
    let (_dir, project) = scaffold_pack_with_ontology(
        &packs_dir().join("autofde-execution-profile-pack"),
        PROFILE,
    );

    let sabotage = format!("{PROFILE}\n<urn:test:memory-counter> afxp:nonce \"forbidden\" .\n");
    assert_gate_refuses(&project, &sabotage, "030_no_authority_tokens");
}
