//! Chicago-TDD e2e proof for the semantic-diataxis-sa2a pack.
//! Real filesystem, real sync, real SPARQL admission gates, generated
//! human + SA2A + SPG + marketplace projections, and adversarial fixtures.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};

use serde_json::Value;
use support::{
    assert_gate_refuses, assert_idempotent, read, read_json, scaffold_pack_with_ontology,
};

const BASE_SHA: &str = "ff96f04e8c7b851e5cca53f3faf5ce1d5f43ce6e";

const VALID_FIXTURE: &str = r#"
@prefix sd: <https://ggen.dev/semantic-diataxis#> .
@prefix ex: <https://example.test/semantic-diataxis#> .

ex:Subject a sd:SemanticSubject ;
    sd:repository "seanchatmangpt/ggen" ;
    sd:revision "ff96f04e8c7b851e5cca53f3faf5ce1d5f43ce6e" ;
    sd:digest "sha256:aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" .

ex:DocumentationCategory a sd:CapabilityCategory .

ex:GenerateSemanticDocs a sd:Capability ;
    sd:slug "generate-semantic-docs" ;
    sd:title "Generate Semantic Diataxis projections" ;
    sd:standing "UNKNOWN" ;
    sd:category ex:DocumentationCategory ;
    sd:consequence "constructive" .

ex:PublishSemanticDocs a sd:Capability ;
    sd:slug "publish-semantic-docs" ;
    sd:title "Publish admitted Semantic Diataxis projections" ;
    sd:standing "UNKNOWN" ;
    sd:category ex:DocumentationCategory ;
    sd:consequence "consequential" .

ex:Tutorial a sd:SemanticDocument, sd:Tutorial ;
    sd:docPath "docs/semantic-diataxis/tutorial.md" ;
    sd:slug "semantic-diataxis-tutorial" ;
    sd:title "Learn Semantic Diataxis" ;
    sd:semanticSubject ex:Subject ;
    sd:standing "UNKNOWN" ;
    sd:provenance "synthetic Chicago-TDD fixture" ;
    sd:owner "semantic-diataxis-sa2a-pack" ;
    sd:freshnessBoundary "ff96f04e8c7b851e5cca53f3faf5ce1d5f43ce6e" ;
    sd:falsifier "The terminal observation is absent after the exercise." ;
    sd:authority "NONE" ;
    sd:relatedCapability ex:GenerateSemanticDocs ;
    sd:learningGoal "Learn to manufacture four Diataxis projections from one semantic subject." ;
    sd:startingState "A consumer project with the semantic-diataxis-sa2a-pack mounted." ;
    sd:prerequisite "The consumer RDF graph validates through all pack admission gates." ;
    sd:exercise "Run ggen sync and inspect the generated Tutorial, How-To, Reference and Explanation projections." ;
    sd:checkpointObservation "Generated projections bind the exact repository revision and semantic digest." ;
    sd:terminalObservation "A second sync is idempotent and the machine descriptors remain byte-stable." ;
    sd:cleanup "Delete the scratch consumer; the pack itself performs no external actuation." .

ex:ConstructHowTo a sd:SemanticDocument, sd:HowTo ;
    sd:docPath "docs/semantic-diataxis/how-to-construct.md" ;
    sd:slug "construct-semantic-docs" ;
    sd:title "How to construct Semantic Diataxis projections" ;
    sd:semanticSubject ex:Subject ;
    sd:standing "UNKNOWN" ;
    sd:provenance "synthetic Chicago-TDD fixture" ;
    sd:owner "semantic-diataxis-sa2a-pack" ;
    sd:freshnessBoundary "ff96f04e8c7b851e5cca53f3faf5ce1d5f43ce6e" ;
    sd:falsifier "Any generated projection binds a different subject revision or digest." ;
    sd:authority "NONE" ;
    sd:relatedCapability ex:GenerateSemanticDocs ;
    sd:achieves "Construct all documentation views and machine descriptors from one RDF subject." ;
    sd:precondition "The semantic subject and document contracts pass pack admission." ;
    sd:requiresCapability ex:GenerateSemanticDocs ;
    sd:requiresAuthority "NONE" ;
    sd:procedure "Run ggen sync; inspect generated docs, SA2A descriptor, SPG candidate and capability projection." ;
    sd:expectedObservation "All projections name the same exact subject and carry no runtime standing." ;
    sd:postcondition "Generated artifacts exist and a second sync writes nothing." ;
    sd:failureMap "A gate refusal names the violated semantic contract; generation does not proceed." ;
    sd:rollback "Fix the RDF source and rerun; never hand-edit a generated projection." ;
    sd:receiptSchema "ggen sync receipt" ;
    sd:replayProcedure "Run the same sync against the same exact subject and compare output bytes." ;
    sd:consequence "constructive" .

ex:PublishHowTo a sd:SemanticDocument, sd:HowTo ;
    sd:docPath "docs/semantic-diataxis/how-to-publish.md" ;
    sd:slug "publish-semantic-docs" ;
    sd:title "How to publish admitted Semantic Diataxis projections" ;
    sd:semanticSubject ex:Subject ;
    sd:standing "UNKNOWN" ;
    sd:provenance "synthetic Chicago-TDD fixture" ;
    sd:owner "semantic-diataxis-sa2a-pack" ;
    sd:freshnessBoundary "ff96f04e8c7b851e5cca53f3faf5ce1d5f43ce6e" ;
    sd:falsifier "The publication receipt is absent or the observed artifact digest differs." ;
    sd:authority "NONE" ;
    sd:relatedCapability ex:PublishSemanticDocs ;
    sd:achieves "Publish already-admitted generated projections." ;
    sd:precondition "Construction is complete and an external publication authority has been admitted." ;
    sd:requiresCapability ex:PublishSemanticDocs ;
    sd:requiresAuthority "publication-authority" ;
    sd:procedure "Submit the generated candidate to the external SA2A/BRCE authority boundary; only that boundary may perform DO." ;
    sd:expectedObservation "The external executor returns an observed publication consequence and durable receipt." ;
    sd:postcondition "The published artifact identity is bound to the exact admitted subject." ;
    sd:failureMap "A refused authority or mismatched observation blocks only the consequential edge." ;
    sd:rollback "Use the external executor's declared rollback/recovery path; documentation does not bypass it." ;
    sd:receiptSchema "chatman.publication-receipt.v1" ;
    sd:replayProcedure "Replay the external receipt against the exact subject and independently compare the published identity." ;
    sd:consequence "consequential" .

ex:Reference a sd:SemanticDocument, sd:Reference ;
    sd:docPath "docs/semantic-diataxis/reference.md" ;
    sd:slug "semantic-diataxis-reference" ;
    sd:title "Semantic Diataxis contract reference" ;
    sd:semanticSubject ex:Subject ;
    sd:standing "UNKNOWN" ;
    sd:provenance "synthetic Chicago-TDD fixture" ;
    sd:owner "semantic-diataxis-sa2a-pack" ;
    sd:freshnessBoundary "ff96f04e8c7b851e5cca53f3faf5ce1d5f43ce6e" ;
    sd:falsifier "A generated contract field cannot be traced to the admitted RDF subject." ;
    sd:authority "NONE" ;
    sd:relatedCapability ex:GenerateSemanticDocs ;
    sd:contract "Tutorial, How-To, Reference and Explanation are projections of one exact semantic subject." ;
    sd:schemaSource "packs/semantic-diataxis-sa2a-pack/ontology.ttl" ;
    sd:machineContract "How-To requires goal, precondition, capability, authority requirement, observation, postcondition, falsifier, receipt and replay." .

ex:Explanation a sd:SemanticDocument, sd:Explanation ;
    sd:docPath "docs/semantic-diataxis/explanation.md" ;
    sd:slug "semantic-diataxis-explanation" ;
    sd:title "Why Semantic Diataxis separates documentation from execution" ;
    sd:semanticSubject ex:Subject ;
    sd:standing "UNKNOWN" ;
    sd:provenance "synthetic Chicago-TDD fixture" ;
    sd:owner "semantic-diataxis-sa2a-pack" ;
    sd:freshnessBoundary "ff96f04e8c7b851e5cca53f3faf5ce1d5f43ce6e" ;
    sd:falsifier "A documentation projection can directly grant or perform consequential authority." ;
    sd:authority "NONE" ;
    sd:relatedCapability ex:GenerateSemanticDocs ;
    sd:concept "Documentation is a typed semantic interface between knowledge and lawful execution." ;
    sd:rationale "Agents need exact subjects, pre/postconditions and evidence without conflating instructions with authority." ;
    sd:tradeoff "More semantic metadata is required up front, but stale prose and package-wide readiness booleans stop controlling execution decisions." ;
    sd:alternative "Independent hand-maintained Markdown views are simpler initially but permit drift and cannot support deterministic SA2A routing." .

ex:FutureExtension a sd:ExtensionClaim ;
    sd:extensionPredicate ex:futurePredicate ;
    sd:extensionValue "preserved but not interpreted" ;
    sd:standing "UNSUPPORTED" .
"#;

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

fn project_with_fixture(fixture: &str) -> (tempfile::TempDir, PathBuf) {
    scaffold_pack_with_ontology(&packs_dir().join("semantic-diataxis-sa2a-pack"), fixture)
}

fn sync_fixture(fixture: &str) -> (tempfile::TempDir, PathBuf) {
    let (dir, project) = project_with_fixture(fixture);
    ggen_engine::sync::sync(
        &project,
        ggen_engine::sync::SyncOptions {
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("semantic Diataxis fixture must sync");
    (dir, project)
}

#[test]
fn semantic_diataxis_generates_human_and_machine_projections_and_is_idempotent() {
    let (_dir, project) = sync_fixture(VALID_FIXTURE);

    let tutorial = read(&project, "docs/semantic-diataxis/tutorial.md");
    assert!(tutorial.contains("**Quadrant:** Tutorial"));
    assert!(tutorial.contains(BASE_SHA));
    assert!(tutorial.contains("Semantic digest"));

    let constructive = read(&project, "docs/semantic-diataxis/how-to-construct.md");
    assert!(constructive.contains("**Document authority:**"));
    assert!(constructive.contains("**Required action authority:**"));
    assert!(constructive.contains("constructive"));

    let consequential = read(&project, "docs/semantic-diataxis/how-to-publish.md");
    assert!(consequential.contains("publication-authority"));
    assert!(consequential.contains("consequential"));

    let reference = read(&project, "docs/semantic-diataxis/reference.md");
    assert!(reference.contains("Semantic Diataxis contract reference"));
    assert!(reference.contains("How-To requires goal, precondition, capability"));

    let explanation = read(&project, "docs/semantic-diataxis/explanation.md");
    assert!(explanation.contains("**Quadrant:** Explanation"));
    assert!(explanation.contains("never actuation authority"));

    let sa2a = read_json(
        &project,
        "generated/semantic-diataxis/sa2a/publish-semantic-docs.json",
    );
    assert_eq!(sa2a["schema"], "chatman.semantic-diataxis.sa2a.v1");
    assert_eq!(sa2a["required_authority"], "publication-authority");
    assert_eq!(sa2a["document_authority"], "NONE");
    assert_eq!(sa2a["construction_state"], "CANDIDATE");
    assert_eq!(sa2a["standing"], "NONE");
    assert_eq!(sa2a["semantic_equivalence"], "UNCLAIMED");
    assert_eq!(sa2a["exact_subject"]["revision"], BASE_SHA);

    let constructive_spg = read_json(
        &project,
        "generated/semantic-diataxis/spg/construct-semantic-docs.json",
    );
    assert_eq!(constructive_spg["schema"], "chatman.spg.v1");
    assert_eq!(constructive_spg["state"], "CANDIDATE");
    assert_eq!(constructive_spg["standing"], "NONE");
    assert_eq!(constructive_spg["nodes"][1]["class"], "CONSTRUCT");
    assert_eq!(constructive_spg["edges"][0]["consequence"], "constructive");
    assert_eq!(constructive_spg["edges"][0]["authority_required"], "NONE");
    assert_eq!(constructive_spg["edges"][0]["receipt_required"], false);

    let consequential_spg = read_json(
        &project,
        "generated/semantic-diataxis/spg/publish-semantic-docs.json",
    );
    assert_eq!(consequential_spg["nodes"][1]["class"], "DO");
    assert_eq!(
        consequential_spg["edges"][0]["authority_required"],
        "publication-authority"
    );
    assert_eq!(
        consequential_spg["edges"][0]["consequence"],
        "consequential"
    );
    assert_eq!(consequential_spg["edges"][0]["receipt_required"], true);
    assert!(consequential_spg["prior_art"]
        .as_array()
        .is_some_and(|items| !items.is_empty()));

    let capability = read_json(
        &project,
        "generated/semantic-diataxis/marketplace/publish-semantic-docs.json",
    );
    assert_eq!(capability["schema"], "chatman.marketplace-capability.v1");
    assert_eq!(capability["standing"], "UNKNOWN");
    assert_eq!(capability["consequence"], "consequential");
    assert_eq!(capability["production_ready"], Value::Null);

    assert_idempotent(&project);
}

fn assert_mutation_refused(old: &str, new: &str, gate: &str) {
    let bad = VALID_FIXTURE.replacen(old, new, 1);
    assert_ne!(bad, VALID_FIXTURE, "mutation must change the fixture");
    let (_dir, project) = project_with_fixture(&bad);
    assert_gate_refuses(&project, &bad, gate);
}

#[test]
fn missing_common_document_field_is_refused() {
    assert_mutation_refused(
        "    sd:docPath \"docs/semantic-diataxis/tutorial.md\" ;\n",
        "",
        "010_required",
    );
}

#[test]
fn ambiguous_single_valued_routing_is_refused() {
    assert_mutation_refused(
        "    sd:title \"Learn Semantic Diataxis\" ;",
        "    sd:title \"Learn Semantic Diataxis\", \"Conflicting title\" ;",
        "020_single_valued",
    );
}

#[test]
fn document_with_two_diataxis_quadrants_is_refused() {
    assert_mutation_refused(
        "ex:ConstructHowTo a sd:SemanticDocument, sd:HowTo ;",
        "ex:ConstructHowTo a sd:SemanticDocument, sd:HowTo, sd:Tutorial ;",
        "030_exactly_one_quadrant",
    );
}

#[test]
fn incomplete_howto_contract_is_refused() {
    assert_mutation_refused(
        "    sd:replayProcedure \"Run the same sync against the same exact subject and compare output bytes.\" ;\n",
        "",
        "040_procedure_contract",
    );
}

#[test]
fn documentation_cannot_self_grant_authority() {
    assert_mutation_refused(
        "    sd:authority \"NONE\" ;",
        "    sd:authority \"SELF_GRANTED\" ;",
        "050_authority_boundary",
    );
}

#[test]
fn mutable_subject_revision_is_refused() {
    assert_mutation_refused(
        "    sd:revision \"ff96f04e8c7b851e5cca53f3faf5ce1d5f43ce6e\" ;",
        "    sd:revision \"main\" ;",
        "060_exact_subject_identity",
    );
}

#[test]
fn alive_without_evidence_is_refused() {
    assert_mutation_refused(
        "    sd:standing \"UNKNOWN\" ;",
        "    sd:standing \"ALIVE\" ;",
        "070_standing_evidence",
    );
}

#[test]
fn uninterpreted_extension_cannot_claim_runtime_standing() {
    assert_mutation_refused(
        "    sd:standing \"UNSUPPORTED\" .",
        "    sd:standing \"PARTIAL_ALIVE\" .",
        "080_extension_claims",
    );
}

#[test]
fn free_form_capability_category_is_refused() {
    assert_mutation_refused(
        "    sd:category ex:DocumentationCategory ;",
        "    sd:category \"documentation\" ;",
        "090_controlled_values",
    );
}

#[test]
fn untyped_capability_link_is_refused() {
    assert_mutation_refused(
        "    sd:relatedCapability ex:GenerateSemanticDocs ;\n    sd:learningGoal",
        "    sd:relatedCapability ex:UnknownCapability ;\n    sd:learningGoal",
        "090_controlled_values",
    );
}

#[test]
fn duplicate_document_output_path_is_refused() {
    assert_mutation_refused(
        "    sd:docPath \"docs/semantic-diataxis/reference.md\" ;",
        "    sd:docPath \"docs/semantic-diataxis/tutorial.md\" ;",
        "090_controlled_values",
    );
}

#[test]
fn human_and_machine_capability_bindings_cannot_diverge() {
    assert_mutation_refused(
        "    sd:relatedCapability ex:GenerateSemanticDocs ;\n    sd:achieves \"Construct all documentation views",
        "    sd:relatedCapability ex:PublishSemanticDocs ;\n    sd:achieves \"Construct all documentation views",
        "090_controlled_values",
    );
}
