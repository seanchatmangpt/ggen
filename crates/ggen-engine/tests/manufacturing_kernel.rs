use ggen_engine::manufacturing_kernel::{
    CourtObligation, ManufacturingKernel, ManufacturingRefusal, ProjectionId, ProjectionSpec,
};
use ggen_graph::RdfDelta;

const NAME_PREDICATE: &str = "https://example.com/schema/name";
const CLOUD_PREDICATE: &str = "https://example.com/schema/cloud";

fn add_quad(predicate: &str, object: &str) -> String {
    format!(
        "<https://example.com/subject> <{predicate}> \"{object}\" <https://example.com/graph> ."
    )
}

#[test]
fn semantic_delta_selects_only_the_minimum_downstream_closure(
) -> Result<(), Box<dyn std::error::Error>> {
    let rust = ProjectionSpec::new("rust")?.observing(NAME_PREDICATE);
    let docs = ProjectionSpec::new("docs")?.depending_on("rust")?;
    let proof = ProjectionSpec::new("proof")?.depending_on("rust")?;
    let cloud = ProjectionSpec::new("cloud")?.observing(CLOUD_PREDICATE);

    let delta = RdfDelta::new(vec![add_quad(NAME_PREDICATE, "Alice")], Vec::new());
    let plan = ManufacturingKernel.plan(&delta, [cloud, proof, docs, rust])?;

    let ids = plan
        .projections
        .iter()
        .map(|projection| projection.id.as_str())
        .collect::<Vec<_>>();

    assert_eq!(ids, vec!["rust", "docs", "proof"]);
    assert!(!plan.is_noop());
    assert_eq!(plan.changed_predicates.len(), 1);
    assert!(plan.changed_predicates.contains(NAME_PREDICATE));
    Ok(())
}

#[test]
fn manufacture_plan_is_insertion_order_invariant() -> Result<(), Box<dyn std::error::Error>> {
    let a = ProjectionSpec::new("a")?.observing(NAME_PREDICATE);
    let b = ProjectionSpec::new("b")?.depending_on("a")?;
    let c = ProjectionSpec::new("c")?.depending_on("a")?;
    let delta = RdfDelta::new(vec![add_quad(NAME_PREDICATE, "Alice")], Vec::new());

    let forward = ManufacturingKernel.plan(&delta, [a.clone(), b.clone(), c.clone()])?;
    let reverse = ManufacturingKernel.plan(&delta, [c, b, a])?;

    assert_eq!(forward, reverse);
    assert_eq!(forward.plan_hash_hex, reverse.plan_hash_hex);
    Ok(())
}

#[test]
fn deletion_triggers_the_same_projection_closure() -> Result<(), Box<dyn std::error::Error>> {
    let source = ProjectionSpec::new("source")?.observing(NAME_PREDICATE);
    let downstream = ProjectionSpec::new("downstream")?.depending_on("source")?;
    let delta = RdfDelta::new(Vec::new(), vec![add_quad(NAME_PREDICATE, "Alice")]);

    let plan = ManufacturingKernel.plan(&delta, [source, downstream])?;
    let ids = plan
        .projections
        .iter()
        .map(|projection| projection.id.as_str())
        .collect::<Vec<_>>();

    assert_eq!(ids, vec!["source", "downstream"]);
    Ok(())
}

#[test]
fn every_affected_projection_carries_the_baseline_court(
) -> Result<(), Box<dyn std::error::Error>> {
    let projection = ProjectionSpec::new("runtime")?.observing(NAME_PREDICATE);
    let delta = RdfDelta::new(vec![add_quad(NAME_PREDICATE, "Alice")], Vec::new());

    let plan = ManufacturingKernel.plan(&delta, [projection])?;
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
    let delta = RdfDelta::new(vec![add_quad(CLOUD_PREDICATE, "gcp")], Vec::new());

    let plan = ManufacturingKernel.plan(&delta, [projection])?;

    assert!(plan.is_noop());
    assert!(!plan.delta_hash_hex.is_empty());
    assert!(!plan.plan_hash_hex.is_empty());
    Ok(())
}

#[test]
fn unknown_dependency_is_refused() -> Result<(), Box<dyn std::error::Error>> {
    let projection = ProjectionSpec::new("docs")?.depending_on("missing")?;
    let delta = RdfDelta::new(Vec::new(), Vec::new());

    let result = ManufacturingKernel.plan(&delta, [projection]);

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
    let delta = RdfDelta::new(Vec::new(), Vec::new());

    let result = ManufacturingKernel.plan(&delta, [a, b]);

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
    let delta = RdfDelta::new(Vec::new(), Vec::new());

    let result = ManufacturingKernel.plan(&delta, [first, second]);

    assert_eq!(
        result,
        Err(ManufacturingRefusal::DuplicateProjection(ProjectionId::new(
            "runtime"
        )?))
    );
    Ok(())
}

#[test]
fn invalid_rdf_delta_is_refused_before_planning() -> Result<(), Box<dyn std::error::Error>> {
    let projection = ProjectionSpec::new("runtime")?.observing(NAME_PREDICATE);
    let delta = RdfDelta::new(vec!["not an n-quad".to_string()], Vec::new());

    let result = ManufacturingKernel.plan(&delta, [projection]);

    assert!(matches!(
        result,
        Err(ManufacturingRefusal::InvalidRdfDelta(_))
    ));
    Ok(())
}
