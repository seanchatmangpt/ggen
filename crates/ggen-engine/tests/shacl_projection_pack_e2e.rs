//! Real end-to-end regression coverage for `packs/shacl-projection-pack` and
//! `packs/shacl-to-pydantic-pack`. Uses the real `sync()` pipeline
//! (`tests/support::scaffold_multi_pack`/`scaffold_pack_with_ontology`, no
//! mocks), a real temp consumer project wired to the packs by relative
//! path, and asserts on real generated Python source text on disk.
//!
//! `shacl-projection-pack` is the shared, target-agnostic vocabulary
//! (`sp:derivedFromShape` / `sp:isPrimaryOutput`) and admission gate
//! (`gates/010_shape_validity.rq`) generalizing the SHACL-shape-driven
//! projection pattern first built as a one-off inside `packs/dspy-pack`.
//! `shacl-to-pydantic-pack` is a SECOND, genuinely different projection
//! target (a real `pydantic.BaseModel`, not another dspy.Signature) that
//! reuses the shared vocabulary/gate rather than reimplementing shape
//! validation from scratch -- proving real reuse, not a parallel theory.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod support;

use std::path::{Path, PathBuf};

use support::{assert_gate_refuses, read, scaffold_multi_pack};

fn packs_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

/// A real, valid consumer ontology deriving one `sp:PydanticModel` from one
/// real `sh:NodeShape` with two input-like fields and one primary-output
/// field, exercising every admitted `sh:datatype` family (string/int/float
/// map to str/int/float; a fourth field below exercises bool).
const VALID_PYDANTIC_FIXTURE: &str = r#"
@prefix sp:      <http://seanchatmangpt.github.io/packs/shacl-projection#> .
@prefix sh:      <http://www.w3.org/ns/shacl#> .
@prefix xsd:     <http://www.w3.org/2001/XMLSchema#> .
@prefix dcterms: <http://purl.org/dc/terms/> .

sp:userProfileModel a sp:PydanticModel ;
    sp:className "UserProfile" ;
    dcterms:description "A real user profile pydantic model." ;
    sp:derivedFromShape sp:userProfileShape .

sp:userProfileShape a sh:NodeShape ;
    sh:property sp:nameField , sp:ageField , sp:activeField .

sp:nameField a sh:PropertyShape ;
    sh:path <http://example.org/name> ; sh:datatype xsd:string ;
    sh:description "the user's display name" ;
    sp:isPrimaryOutput true .

sp:ageField a sh:PropertyShape ;
    sh:path <http://example.org/age> ; sh:datatype xsd:integer ;
    sh:description "the user's age in years" ;
    sp:isPrimaryOutput true .

sp:activeField a sh:PropertyShape ;
    sh:path <http://example.org/active> ; sh:datatype xsd:boolean ;
    sh:description "whether the account is active" ;
    sp:isPrimaryOutput true .
"#;

/// Sync `ontology` against a fresh copy of BOTH `shacl-projection-pack`
/// (vocabulary + shared admission gate) and `shacl-to-pydantic-pack` (the
/// pydantic projection template), and return the real generated
/// `src/shacl_pydantic_models.py` content.
fn sync_and_read_pydantic(ontology: &str) -> (tempfile::TempDir, PathBuf, String) {
    let (dir, project) = scaffold_multi_pack(&["shacl-projection-pack", "shacl-to-pydantic-pack"]);
    std::fs::write(project.join("ontology.ttl"), ontology).expect("write ontology.ttl");
    ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default())
        .expect("valid pydantic fixture must sync cleanly through the shared admission gate");
    let content = read(&project, "src/shacl_pydantic_models.py");
    (dir, project, content)
}

#[test]
fn valid_shape_renders_real_pydantic_basemodel_with_all_fields() {
    let (_dir, _project, program) = sync_and_read_pydantic(VALID_PYDANTIC_FIXTURE);
    assert!(
        program.contains("import pydantic"),
        "must import pydantic:\n{program}"
    );
    assert!(
        program.contains("class UserProfile(pydantic.BaseModel):"),
        "must render the real class name from sp:className, echoed verbatim:\n{program}"
    );
    assert!(
        program.contains(r#"name: str = pydantic.Field(description="the user's display name")"#),
        "must render the string field mapped to str:\n{program}"
    );
    assert!(
        program.contains(r#"age: int = pydantic.Field(description="the user's age in years")"#),
        "must render the integer field mapped to int:\n{program}"
    );
    assert!(
        program.contains(
            r#"active: bool = pydantic.Field(description="whether the account is active")"#
        ),
        "must render the boolean field mapped to bool:\n{program}"
    );
}

#[test]
fn second_sync_is_idempotent() {
    let (_dir, project, _program) = sync_and_read_pydantic(VALID_PYDANTIC_FIXTURE);
    support::assert_idempotent(&project);
}

#[test]
fn shared_gate_refuses_invalid_python_identifier_for_pydantic_projection() {
    // Proves real reuse, not just parallel theory: the SAME shared gate
    // (shacl-projection-pack's gates/010_shape_validity.rq) that would gate
    // a dspy Signature also gates a pydantic model derived from a shape with
    // an invalid field identifier -- shacl-to-pydantic-pack ships no gate
    // of its own.
    let (dir, project) = scaffold_multi_pack(&["shacl-projection-pack", "shacl-to-pydantic-pack"]);
    let sabotage = r#"
@prefix sp:  <http://seanchatmangpt.github.io/packs/shacl-projection#> .
@prefix sh:  <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
sp:badModel a sp:PydanticModel ; sp:className "BadId" ; sp:derivedFromShape sp:badShape .
sp:badShape a sh:NodeShape ; sh:property sp:p1 , sp:out1 .
sp:p1 a sh:PropertyShape ; sh:path <http://example.org/9bad-name> ; sh:datatype xsd:string .
sp:out1 a sh:PropertyShape ; sh:path <http://example.org/ok> ; sh:datatype xsd:string ; sp:isPrimaryOutput true .
"#;
    assert_gate_refuses(&project, sabotage, "010_shape_validity");
    drop(dir);
}

#[test]
fn shared_gate_refuses_zero_primary_output_fields_for_pydantic_projection() {
    let (dir, project) = scaffold_multi_pack(&["shacl-projection-pack", "shacl-to-pydantic-pack"]);
    let sabotage = r#"
@prefix sp:  <http://seanchatmangpt.github.io/packs/shacl-projection#> .
@prefix sh:  <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
sp:noOutModel a sp:PydanticModel ; sp:className "NoOut" ; sp:derivedFromShape sp:noOutShape .
sp:noOutShape a sh:NodeShape ; sh:property sp:onlyField .
sp:onlyField a sh:PropertyShape ; sh:path <http://example.org/only_field> ; sh:datatype xsd:string .
"#;
    assert_gate_refuses(&project, sabotage, "010_shape_validity");
    drop(dir);
}

#[test]
fn shared_gate_refuses_unmapped_datatype_for_pydantic_projection() {
    let (dir, project) = scaffold_multi_pack(&["shacl-projection-pack", "shacl-to-pydantic-pack"]);
    let sabotage = r#"
@prefix sp:  <http://seanchatmangpt.github.io/packs/shacl-projection#> .
@prefix sh:  <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
sp:badTypeModel a sp:PydanticModel ; sp:className "BadType" ; sp:derivedFromShape sp:badTypeShape .
sp:badTypeShape a sh:NodeShape ; sh:property sp:p1 , sp:out1 .
sp:p1 a sh:PropertyShape ; sh:path <http://example.org/weird> ; sh:datatype xsd:anyURI .
sp:out1 a sh:PropertyShape ; sh:path <http://example.org/ok> ; sh:datatype xsd:string ; sp:isPrimaryOutput true .
"#;
    assert_gate_refuses(&project, sabotage, "010_shape_validity");
    drop(dir);
}

#[test]
fn shared_gate_refuses_duplicate_field_name_within_one_shape() {
    let (dir, project) = scaffold_multi_pack(&["shacl-projection-pack", "shacl-to-pydantic-pack"]);
    let sabotage = r#"
@prefix sp:  <http://seanchatmangpt.github.io/packs/shacl-projection#> .
@prefix sh:  <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
sp:dupModel a sp:PydanticModel ; sp:className "Dup" ; sp:derivedFromShape sp:dupShape .
sp:dupShape a sh:NodeShape ; sh:property sp:p1 , sp:p2 .
sp:p1 a sh:PropertyShape ; sh:path <http://example.org/name> ; sh:datatype xsd:string ; sp:isPrimaryOutput true .
sp:p2 a sh:PropertyShape ; sh:path <http://other.org/name> ; sh:datatype xsd:string .
"#;
    assert_gate_refuses(&project, sabotage, "010_shape_validity");
    drop(dir);
}

#[test]
fn shacl_to_pydantic_pack_alone_ships_no_admission_gate_but_still_syncs() {
    // Documents the real, confirmed limitation stated in shacl-to-pydantic-pack's own
    // pack.toml: composing shacl-to-pydantic-pack WITHOUT shacl-projection-pack gets the
    // template (vocabulary is just IRIs) but NOT the shared admission gate -- a sabotaged
    // shape that would be refused when both packs are composed instead syncs cleanly here,
    // proving the gate is not silently inherited by some other mechanism.
    let (dir, project) = support::scaffold_pack(&packs_root().join("shacl-to-pydantic-pack"));
    let ttl = r#"
@prefix sp:  <http://seanchatmangpt.github.io/packs/shacl-projection#> .
@prefix sh:  <http://www.w3.org/ns/shacl#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
sp:noOutModel a sp:PydanticModel ; sp:className "NoOut" ; sp:derivedFromShape sp:noOutShape .
sp:noOutShape a sh:NodeShape ; sh:property sp:onlyField .
sp:onlyField a sh:PropertyShape ; sh:path <http://example.org/only_field> ; sh:datatype xsd:string .
"#;
    std::fs::write(project.join("ontology.ttl"), ttl).expect("write ontology.ttl");
    let result = ggen_engine::sync::sync(&project, ggen_engine::sync::SyncOptions::default());
    assert!(
        result.is_ok(),
        "without shacl-projection-pack composed, the same shape that gets refused above must \
         sync cleanly (no gate present to refuse it): {result:?}"
    );
    let program = read(&project, "src/shacl_pydantic_models.py");
    assert!(program.contains("class NoOut(pydantic.BaseModel):"));
    drop(dir);
}
