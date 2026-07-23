use ggen_marketplace::marketplace::{
    PartPassport, PassportViolationCode, SubstitutionViolationCode,
};

const CROWN_PASSPORT: &str =
    include_str!("../../../examples/part-passport/crown-kernel.json");

fn passport() -> PartPassport {
    serde_json::from_str(CROWN_PASSPORT).expect("canonical passport fixture must deserialize")
}

#[test]
fn canonical_fixture_is_admitted() {
    let passport = passport();
    let report = passport.validate();
    assert!(report.is_valid(), "{:#?}", report.violations);
}

#[test]
fn canonical_fixture_projects_a_causal_nameplate() {
    let label = passport().render_nameplate();
    assert!(label.contains("GGEN PART PASSPORT"));
    assert!(label.contains("MODEL: crown-kernel"));
    assert!(label.contains("INPUT: application/ld+json"));
    assert!(label.contains("OUTPUT: application/json"));
    assert!(label.contains("MARKS: O* D NAA II LCS IV NI RET"));
}

#[test]
fn invalid_digest_is_refused_before_substitution() {
    let mut candidate = passport();
    candidate.identity.artifact_digest = "sha256:not-a-digest".to_string();

    let validation = candidate.validate();
    assert!(validation
        .violations
        .iter()
        .any(|violation| violation.code == PassportViolationCode::InvalidDigest));

    let required = passport();
    let substitution = candidate.can_substitute_for(&required);
    assert!(!substitution.compatible);
    assert!(substitution
        .violations
        .iter()
        .any(|violation| violation.code == SubstitutionViolationCode::InvalidCandidate));
}

#[test]
fn widened_authority_breaks_interchangeability() {
    let required = passport();
    let mut candidate = required.clone();
    candidate
        .polarity
        .requires_authorities
        .insert("deploy:production".to_string());

    let report = candidate.can_substitute_for(&required);
    assert!(!report.compatible);
    assert!(report
        .violations
        .iter()
        .any(|violation| violation.code == SubstitutionViolationCode::CausalPolarity));
}

#[test]
fn lower_resource_draw_preserves_substitutability() {
    let required = passport();
    let mut candidate = required.clone();
    candidate.identity.artifact_digest =
        format!("blake3:{}", "c".repeat(64));
    for mark in &mut candidate.conformity {
        mark.artifact_digest
            .clone_from(&candidate.identity.artifact_digest);
    }
    for mark in &mut candidate.verifiers {
        mark.artifact_digest
            .clone_from(&candidate.identity.artifact_digest);
    }
    candidate.resources.max_memory_pages = 32;
    candidate.resources.max_fuel = 4_000_000;
    candidate.resources.max_execution_ms = 10;

    let report = candidate.can_substitute_for(&required);
    assert!(report.compatible, "{:#?}", report.violations);
}
