use ggen_engine::manufacturing_kernel::{
    AdmittedSemanticDelta, CourtObligation, ManufacturingKernel, ManufacturingRefusal, ProjectionId,
    ProjectionSpec,
};

const NAME_PREDICATE: &str = "https://example.com/schema/name";
const CLOUD_PREDICATE: &str = "https://example.com/schema/cloud";

fn delta(
    identity: &str, predicates: &[&str],
) -> Result<AdmittedSemanticDelta, ManufacturingRefusal> {
    AdmittedSemanticDelta::new(identity, predicates.iter().copied())
}

#[test]
fn semantic_delta_selects_only_the_minimum_downstream_closure(
) -> Result<(), Box<dyn std::error::Error>> {
    let rust = ProjectionSpec::new("rust")?.observing(NAME_PREDICATE);
    let docs = ProjectionSpec::new("docs")?.depending_on("rust")?;
    let proof = ProjectionSpec::new("proof")?.depending_on("rust")?;
    let cloud = ProjectionSpec::new("cloud")?.observing(CLOUD_PREDICATE);

    let admitted = delta("blake3:delta-name-v1", &[NAME_PREDICATE])?;
    let plan = ManufacturingKernel.plan(&admitted, [cloud, proof, docs, rust])?;

    let ids = plan
        .projections
        .iter()
        .map(|projection| projection.id.as_str())
        .collect::<Vec<_>>();

    assert_eq!(ids, vec!["rust", "docs", "proof"]);
    assert!(!plan.is_noop());
    assert_eq!(plan.delta_identity, "blake3:delta-name-v1");
    assert_eq!(plan.changed_predicates.len(), 1);
    assert!(plan.changed_predicates.contains(NAME_PREDICATE));
    Ok(())
}

#[test]
fn manufacture_plan_is_insertion_order_invariant() -> Result<(), Box<dyn std::error::Error>> {
    let a = ProjectionSpec::new("a")?.observing(NAME_PREDICATE);
    let b = ProjectionSpec::new("b")?.depending_on("a")?;
    let c = ProjectionSpec::new("c")?.depending_on("a")?;
    let admitted = AdmittedSemanticDelta::new(
        "blake3:delta-v1",
        [NAME_PREDICATE, CLOUD_PREDICATE],
    )?;

    let forward = ManufacturingKernel.plan(&admitted, [a.clone(), b.clone(), c.clone()])?;
    let reverse = ManufacturingKernel.plan(&admitted, [c, b, a])?;

    assert_eq!(forward, reverse);
    assert_eq!(forward.plan_hash_hex, reverse.plan_hash_hex);
    Ok(())
}

#[test]
fn transitive_projection_dependencies_close_to_the_leaf(
) -> Result<(), Box<dyn std::error::Error>> {
    let source = ProjectionSpec::new("source")?.observing(NAME_PREDICATE);
    let middle = ProjectionSpec::new("middle")?.depending_on("source")?;
    let leaf = ProjectionSpec::new("leaf")?.depending_on("middle")?;
    let admitted = delta("receipt:semantic-change-42", &[NAME_PREDICATE])?;

    let plan = ManufacturingKernel.plan(&admitted, [leaf, source, middle])?;
    let ids = plan
        .projections
        .iter()
        .map(|projection| projection.id.as_str())
        .collect::<Vec<_>>();

    assert_eq!(ids, vec!["source", "middle", "leaf"]);
    Ok(())
}

#[test]
fn every_affected_projection_carries_the_baseline_court(
) -> Result<(), Box<dyn std::error::Error>> {
    let projection = ProjectionSpec::new("runtime")?.observing(NAME_PREDICATE);
    let admitted = delta("blake3:delta-name-v1", &[NAME_PREDICATE])?;

    let plan = ManufacturingKernel.plan(&admitted, [projection])?;
    let court = &plan.projections[0].court;

    for obligation in [
        CourtObligation::Determinism,
        CourtObligation::Provenance,
        CourtObligation::Falsifier,
        CourtObligation::Correspondence,
        CourtObligation::ReceiptReplay,
        CourtObligation::AuthorityBoundary,
    ] {
        assert!(court.contains(&obligation));
    }
    Ok(())
}

#[test]
fn unrelated_delta_is_a_receiptable_noop() -> Result<(), Box<dyn std::error::Error>> {
    let projection = ProjectionSpec::new("runtime")?.observing(NAME_PREDICATE);
    let admitted = delta("blake3:cloud-only-v1", &[CLOUD_PREDICATE])?;

    let plan = ManufacturingKernel.plan(&admitted, [projection])?;

    assert!(plan.is_noop());
    assert_eq!(plan.delta_identity, "blake3:cloud-only-v1");
    assert!(!plan.plan_hash_hex.is_empty());
    Ok(())
}

#[test]
fn changing_admitted_delta_identity_changes_plan_identity(
) -> Result<(), Box<dyn std::error::Error>> {
    let projection = ProjectionSpec::new("runtime")?.observing(NAME_PREDICATE);
    let first = delta("blake3:first", &[NAME_PREDICATE])?;
    let second = delta("blake3:second", &[NAME_PREDICATE])?;

    let first_plan = ManufacturingKernel.plan(&first, [projection.clone()])?;
    let second_plan = ManufacturingKernel.plan(&second, [projection])?;

    assert_ne!(first_plan.plan_hash_hex, second_plan.plan_hash_hex);
    Ok(())
}

#[test]
fn blank_delta_identity_is_refused() {
    let result = AdmittedSemanticDelta::new("  ", [NAME_PREDICATE]);

    assert_eq!(result, Err(ManufacturingRefusal::EmptyDeltaIdentity));
}

#[test]
fn blank_delta_predicate_is_refused() {
    let result = AdmittedSemanticDelta::new("blake3:delta-v1", [" "]);

    assert_eq!(result, Err(ManufacturingRefusal::EmptyDeltaPredicate));
}

#[test]
fn blank_projection_trigger_is_refused_before_planning(
) -> Result<(), Box<dyn std::error::Error>> {
    let projection = ProjectionSpec::new("runtime")?.observing(" ");
    let admitted = delta("blake3:delta-v1", &[NAME_PREDICATE])?;

    let result = ManufacturingKernel.plan(&admitted, [projection]);

    assert_eq!(
        result,
        Err(ManufacturingRefusal::EmptyProjectionTrigger(
            ProjectionId::new("runtime")?
        ))
    );
    Ok(())
}

#[test]
fn unknown_dependency_is_refused() -> Result<(), Box<dyn std::error::Error>> {
    let projection = ProjectionSpec::new("docs")?.depending_on("missing")?;
    let admitted = delta("blake3:delta-v1", &[])?;

    let result = ManufacturingKernel.plan(&admitted, [projection]);

    assert_eq!(
        result,
        Err(ManufacturingRefusal::UnknownDependency {
            projection: ProjectionId::new("docs")?,
            dependency: ProjectionId::new("missing")?,
        })
    );
    Ok(())
}

#[test]
fn projection_cycle_is_refused() -> Result<(), Box<dyn std::error::Error>> {
    let a = ProjectionSpec::new("a")?.depending_on("b")?;
    let b = ProjectionSpec::new("b")?.depending_on("a")?;
    let admitted = delta("blake3:delta-v1", &[])?;

    let result = ManufacturingKernel.plan(&admitted, [a, b]);

    assert_eq!(
        result,
        Err(ManufacturingRefusal::ProjectionCycle {
            members: vec![ProjectionId::new("a")?, ProjectionId::new("b")?],
        })
    );
    Ok(())
}

#[test]
fn duplicate_projection_identity_is_refused() -> Result<(), Box<dyn std::error::Error>> {
    let first = ProjectionSpec::new("runtime")?;
    let second = ProjectionSpec::new("runtime")?;
    let admitted = delta("blake3:delta-v1", &[])?;

    let result = ManufacturingKernel.plan(&admitted, [first, second]);

    assert_eq!(
        result,
        Err(ManufacturingRefusal::DuplicateProjection(ProjectionId::new(
            "runtime"
        )?))
    );
    Ok(())
}
