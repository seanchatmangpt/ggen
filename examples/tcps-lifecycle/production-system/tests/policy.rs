use tcps_production::policy::{DeploymentDocument, PolicyRefusal, ReleaseDocument};

const RELEASE: &str = include_str!("../../release/release.toml");
const DEPLOYMENT: &str = include_str!("../../deploy/production.toml");

#[test]
fn canonical_release_and_deployment_policies_are_admitted() {
    ReleaseDocument::parse(RELEASE).expect("release policy");
    DeploymentDocument::parse(DEPLOYMENT).expect("deployment policy");
}

#[test]
fn release_rebuild_or_unsigned_artifact_is_refused() {
    let rebuild = RELEASE.replace(
        "rebuild_between_environments = false",
        "rebuild_between_environments = true",
    );
    assert_eq!(
        ReleaseDocument::parse(&rebuild).expect_err("rebuild must refuse"),
        PolicyRefusal::PromotionRailIncomplete
    );

    let unsigned = RELEASE.replace(
        "require_signed_artifact = true",
        "require_signed_artifact = false",
    );
    assert_eq!(
        ReleaseDocument::parse(&unsigned).expect_err("unsigned must refuse"),
        PolicyRefusal::ReleaseRailIncomplete
    );
}

#[test]
fn deployment_without_rollback_receipts_or_distinct_authority_is_refused() {
    let no_rollback = DEPLOYMENT.replace(
        "rollback_on_receipt_gap = true",
        "rollback_on_receipt_gap = false",
    );
    assert_eq!(
        DeploymentDocument::parse(&no_rollback).expect_err("rollback must be mandatory"),
        PolicyRefusal::DeploymentRailIncomplete
    );

    let collapsed = DEPLOYMENT.replace(
        "authorizer = \"praxis-blue-river-dam\"",
        "authorizer = \"tcps-auto-select\"",
    );
    assert_eq!(
        DeploymentDocument::parse(&collapsed).expect_err("actors must be distinct"),
        PolicyRefusal::AuthorityCollapse
    );
}

#[test]
fn unknown_policy_fields_are_refused() {
    let unknown = format!("{DEPLOYMENT}\nunknown = true\n");
    assert_eq!(
        DeploymentDocument::parse(&unknown).expect_err("unknown field must refuse"),
        PolicyRefusal::MalformedPolicy
    );
}
