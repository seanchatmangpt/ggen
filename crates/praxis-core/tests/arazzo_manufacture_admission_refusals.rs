//! PROJ-783 (PRD.md v26.7.11 sec.7.5/18/19.3): end-to-end proof that
//! `praxis_core::arazzo::admit_manufactured_arazzo` refuses a presented
//! Arazzo document with `ARAZZO_UNMANUFACTURED`,
//! `ARAZZO_SOURCE_RECEIPT_MISSING`, or `ARAZZO_PROJECTION_DIGEST_MISMATCH`
//! -- run against a genuine manufactured artifact produced by the real
//! Rail A/B pipeline (`ArazzoProjectionReceipt::project_and_compile`), not
//! a hand-built fixture standing in for one.
//!
//! Follows the capstone-test pattern established this session
//! (`crates/wasm4pm-arazzo/tests/end_to_end_lowering.rs`,
//! `crates/praxis-core/tests/rail_ab_external_cut_wiring.rs`), exercised
//! from outside the crate through `praxis_core::arazzo`'s public API.

use std::collections::BTreeSet;

use powl2_decompose::Powl;
use praxis_core::arazzo::{admit_manufactured_arazzo, ArazzoProjectionReceipt};
use praxis_core::error::CoreError;

/// Mirrors the fixture already established in
/// `praxis_core::arazzo::tests::model_with_external_cut` /
/// `praxis_graphlaw::chatman::powl_projection::tests::model_with_external_cut`.
fn model_with_external_cut() -> Powl {
    Powl::PartialOrder {
        children: vec![
            Powl::Leaf(Some("intake".to_string())),
            Powl::ExternalCut {
                region: Box::new(Powl::Leaf(Some("remote_settle".to_string()))),
                projection: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                renderer: "arazzo_projection.tera".to_string(),
            },
        ],
        order: BTreeSet::from([(0usize, 1usize)]),
    }
}

#[test]
fn a_genuinely_manufactured_document_is_admitted() -> Result<(), Box<dyn std::error::Error>> {
    let artifact = ArazzoProjectionReceipt::project_and_compile(
        &model_with_external_cut(),
        "urn:e2e:proj783:manufacture-ok",
        Some("urn:e2e:proj783:manufacture-ok"),
        "e2e-manufacture-admission-ok",
        "PROJ-783 e2e: real manufacture is admitted",
        "26.7.11",
    )?;
    admit_manufactured_arazzo(&artifact.arazzo_document, Some(&artifact.receipt))?;
    Ok(())
}

#[test]
fn arazzo_unmanufactured_fires_on_a_real_manufactured_document_presented_without_its_receipt(
) -> Result<(), Box<dyn std::error::Error>> {
    let artifact = ArazzoProjectionReceipt::project_and_compile(
        &model_with_external_cut(),
        "urn:e2e:proj783:unmanufactured",
        Some("urn:e2e:proj783:unmanufactured"),
        "e2e-manufacture-admission-no-receipt",
        "PROJ-783 e2e: missing receipt refuses ARAZZO_UNMANUFACTURED",
        "26.7.11",
    )?;
    let result = admit_manufactured_arazzo(&artifact.arazzo_document, None);
    assert!(
        matches!(result, Err(CoreError::ArazzoUnmanufactured(_))),
        "got: {result:?}"
    );
    Ok(())
}

#[test]
fn arazzo_source_receipt_missing_fires_on_a_receipt_with_no_source_powl_binding(
) -> Result<(), Box<dyn std::error::Error>> {
    let artifact = ArazzoProjectionReceipt::project_and_compile(
        &model_with_external_cut(),
        "urn:e2e:proj783:source-missing",
        Some("urn:e2e:proj783:source-missing"),
        "e2e-manufacture-admission-no-source",
        "PROJ-783 e2e: unbound receipt refuses ARAZZO_SOURCE_RECEIPT_MISSING",
        "26.7.11",
    )?;
    let mut unbound_receipt = artifact.receipt.clone();
    unbound_receipt.source_powl_digest_hex = String::new();
    let result = admit_manufactured_arazzo(&artifact.arazzo_document, Some(&unbound_receipt));
    assert!(
        matches!(result, Err(CoreError::ArazzoSourceReceiptMissing(_))),
        "got: {result:?}"
    );
    Ok(())
}

#[test]
fn arazzo_projection_digest_mismatch_fires_on_a_document_tampered_after_manufacture(
) -> Result<(), Box<dyn std::error::Error>> {
    let artifact = ArazzoProjectionReceipt::project_and_compile(
        &model_with_external_cut(),
        "urn:e2e:proj783:digest-mismatch",
        Some("urn:e2e:proj783:digest-mismatch"),
        "e2e-manufacture-admission-tampered",
        "PROJ-783 e2e: tampered document refuses ARAZZO_PROJECTION_DIGEST_MISMATCH",
        "26.7.11",
    )?;
    let tampered_document = format!(
        "{}\n// tampered after manufacture\n",
        artifact.arazzo_document
    );
    let result = admit_manufactured_arazzo(&tampered_document, Some(&artifact.receipt));
    assert!(
        matches!(result, Err(CoreError::ArazzoProjectionDigestMismatch(_))),
        "got: {result:?}"
    );
    Ok(())
}
