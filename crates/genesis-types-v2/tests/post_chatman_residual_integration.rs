//! Integration tests for residual-vector repair types (post-Chatman Feature 4).
//!
//! Tests the full repair admission flow:
//!   build residual vector → check passing/failing → run repair → verify admission.
//!
//! Chicago TDD: real data, real computation, state-based assertions.
//! No mocks. Panics in test context are intentional failure signals.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use genesis_types::{
    BoundedRepairOperator, EvidenceTier, RepairAdmissionReport, RepairBand, ResidualDimension,
    ResidualVector, VisualGapReport,
};

// ─── full repair admission flow ───────────────────────────────────────────────

#[test]
fn test_residual_vector_repair_admission_flow() {
    // Arrange: simulate a visual gap report from a fresh render (two failing dims)
    let before_dims = vec![
        ResidualDimension::new("foreground_component_count", 22.0, (1.0, 5.0)),
        ResidualDimension::new("silhouette_iou", 0.15, (0.25, 1.0)),
    ];
    let before = ResidualVector::new(before_dims);

    // Assert: dominant dimension is the largest absolute residual
    // foreground residual = 22 - 3 = +19, silhouette residual = 0.15 - 0.625 = -0.475
    assert_eq!(
        before.dominant.as_deref(),
        Some("foreground_component_count"),
        "dominant must be the dimension with largest |residual|"
    );
    assert!(!before.all_passing(), "both dimensions fail before repair");

    // Act: apply repair — component count reduced, IoU improved
    let after_dims = vec![
        ResidualDimension::new("foreground_component_count", 4.0, (1.0, 5.0)),
        ResidualDimension::new("silhouette_iou", 0.30, (0.25, 1.0)),
    ];
    let after = ResidualVector::new(after_dims);
    assert!(after.all_passing(), "both dims should pass after repair");

    // Act: compute admission report
    let admission = RepairAdmissionReport::compute("merge-components-op", before, after);

    // Assert: repair that fixes all dims is admitted
    assert!(
        admission.admitted,
        "repair that moves all dims into passing range must be admitted"
    );
    assert!(!admission.detail.is_empty(), "detail must be populated");
}

// ─── repair that doesn't improve → not admitted ───────────────────────────────

#[test]
fn test_repair_no_improvement_is_not_admitted() {
    // Both before and after: same number of failing dims (1 each)
    let before = ResidualVector::new(vec![
        ResidualDimension::new("count", 22.0, (1.0, 5.0)), // failing
    ]);
    let after = ResidualVector::new(vec![
        ResidualDimension::new("count", 20.0, (1.0, 5.0)), // still failing
    ]);

    let admission = RepairAdmissionReport::compute("no-op-repair", before, after);
    assert!(
        !admission.admitted,
        "repair that leaves failing count unchanged must not be admitted"
    );
    assert!(
        admission.detail.contains("Repair did not improve"),
        "detail should explain non-improvement; got: {}",
        admission.detail
    );
}

// ─── partial repair: fewer failing dims → admitted ───────────────────────────

#[test]
fn test_repair_reduces_failing_count_is_admitted() {
    // Two failing dims before, one fixed after
    let before = ResidualVector::new(vec![
        ResidualDimension::new("count", 22.0, (1.0, 5.0)),       // failing
        ResidualDimension::new("silhouette", 0.10, (0.25, 1.0)), // failing
    ]);
    let after = ResidualVector::new(vec![
        ResidualDimension::new("count", 4.0, (1.0, 5.0)),        // now passing
        ResidualDimension::new("silhouette", 0.10, (0.25, 1.0)), // still failing
    ]);

    let admission = RepairAdmissionReport::compute("partial-repair-op", before, after);
    assert!(
        admission.admitted,
        "repair that reduces failing count should be admitted"
    );
    assert!(
        admission.detail.contains("reduced"),
        "detail should mention reduction; got: {}",
        admission.detail
    );
}

// ─── RepairBand enforcement ───────────────────────────────────────────────────

#[test]
fn test_repair_band_enforcement() {
    let band = RepairBand {
        default_band: (1.0, 20.0),
        preferred_band: (3.0, 10.0),
        forbidden_band: (0.0, 0.5),
        tier: EvidenceTier::Known,
        unit: "count".to_string(),
    };

    // Forbidden zone (0 < value < 0.5)
    assert!(band.is_forbidden(0.25), "0.25 is inside the forbidden band");
    assert!(band.is_forbidden(0.1), "0.1 is inside the forbidden band");
    assert!(!band.is_forbidden(0.5), "0.5 is at the exclusive upper bound — not forbidden");
    assert!(!band.is_forbidden(5.0), "5.0 is well above the forbidden band");

    // Preferred zone (3.0 ≤ value ≤ 10.0)
    assert!(band.is_preferred(5.0), "5.0 is inside the preferred band");
    assert!(band.is_preferred(3.0), "3.0 is at the inclusive lower bound of preferred");
    assert!(band.is_preferred(10.0), "10.0 is at the inclusive upper bound of preferred");
    assert!(!band.is_preferred(15.0), "15.0 is outside preferred (but not forbidden)");
    assert!(!band.is_preferred(0.3), "0.3 is in the forbidden band, not preferred");
}

// ─── stale VisualGapReport is rejected ────────────────────────────────────────

#[test]
fn test_stale_visual_gap_report_is_rejected() {
    let stale = VisualGapReport {
        render_hash: "stale-hash-abc123".to_string(),
        timestamp: chrono::Utc::now(),
        residuals: ResidualVector::new(vec![]),
        is_fresh_render: false,
    };

    let result = stale.assert_fresh();
    assert!(result.is_err(), "stale report must be rejected by assert_fresh()");
}

// ─── fresh VisualGapReport is accepted ───────────────────────────────────────

#[test]
fn test_fresh_visual_gap_report_is_accepted() {
    let fresh = VisualGapReport {
        render_hash: "fresh-hash-deadbeef".to_string(),
        timestamp: chrono::Utc::now(),
        residuals: ResidualVector::new(vec![
            ResidualDimension::new("silhouette_iou", 0.45, (0.25, 1.0)),
        ]),
        is_fresh_render: true,
    };

    let result = fresh.assert_fresh();
    assert!(result.is_ok(), "fresh report must be accepted by assert_fresh()");
}

// ─── dominant dimension computation ───────────────────────────────────────────

#[test]
fn test_dominant_dimension_is_largest_absolute_residual() {
    // residual(a) = 10 - 7.5 = 2.5  (midpoint of 5..10)
    // residual(b) = 0.1 - 0.625 = -0.525  (midpoint of 0.25..1.0)
    // residual(c) = 22 - 3 = 19   (midpoint of 1..5)
    // dominant = c (|19| > |2.5| > |-0.525|)
    let dims = vec![
        ResidualDimension::new("coverage_ratio", 10.0, (5.0, 10.0)),
        ResidualDimension::new("silhouette_iou", 0.10, (0.25, 1.0)),
        ResidualDimension::new("component_count", 22.0, (1.0, 5.0)),
    ];
    let vec = ResidualVector::new(dims);
    assert_eq!(
        vec.dominant.as_deref(),
        Some("component_count"),
        "component_count has the largest absolute residual"
    );
}

// ─── empty residual vector ────────────────────────────────────────────────────

#[test]
fn test_empty_residual_vector_has_no_dominant_and_all_passing() {
    let vec = ResidualVector::new(vec![]);
    assert!(vec.dominant.is_none(), "empty vector has no dominant");
    assert!(vec.all_passing(), "empty vector trivially passes (vacuous truth)");
}

// ─── BoundedRepairOperator field integrity ────────────────────────────────────

#[test]
fn test_bounded_repair_operator_serializes_and_has_correct_fields() {
    let op = BoundedRepairOperator {
        id: "merge-components-v1".to_string(),
        targets_dimension: "foreground_component_count".to_string(),
        band: RepairBand {
            default_band: (1.0, 20.0),
            preferred_band: (3.0, 10.0),
            forbidden_band: (0.0, 0.5),
            tier: EvidenceTier::Known,
            unit: "count".to_string(),
        },
        description: "Merges nearby foreground segments to reduce component fragmentation."
            .to_string(),
        modifies_source_law: false,
    };

    // Assert field values are preserved (state-based assertion, not mock interaction)
    assert_eq!(op.id, "merge-components-v1");
    assert_eq!(op.targets_dimension, "foreground_component_count");
    assert!(!op.modifies_source_law, "this operator must not modify source law");
    assert!(op.band.is_preferred(5.0));
    assert!(!op.band.is_forbidden(5.0));

    // Assert JSON serialization roundtrip
    let json = serde_json::to_string(&op).unwrap();
    let restored: BoundedRepairOperator = serde_json::from_str(&json).unwrap();
    assert_eq!(restored.id, op.id);
    assert_eq!(restored.targets_dimension, op.targets_dimension);
    assert_eq!(restored.modifies_source_law, op.modifies_source_law);
}

// ─── EvidenceTier serialisation ───────────────────────────────────────────────

#[test]
fn test_evidence_tier_all_variants_roundtrip() {
    let tiers = [
        EvidenceTier::Known,
        EvidenceTier::Inferred,
        EvidenceTier::Estimated,
        EvidenceTier::Forbidden,
        EvidenceTier::ExceptionClass,
    ];

    for tier in &tiers {
        let json = serde_json::to_string(tier).unwrap();
        let back: EvidenceTier = serde_json::from_str(&json).unwrap();
        assert_eq!(*tier, back, "EvidenceTier::{tier:?} must roundtrip through JSON");
    }
}

// ─── multi-dim repair: detail message reflects count ─────────────────────────

#[test]
fn test_repair_admission_detail_message_reflects_failure_reduction() {
    // Three failing dims before, two after
    let before = ResidualVector::new(vec![
        ResidualDimension::new("a", 100.0, (0.0, 10.0)), // failing
        ResidualDimension::new("b", 100.0, (0.0, 10.0)), // failing
        ResidualDimension::new("c", 100.0, (0.0, 10.0)), // failing
    ]);
    let after = ResidualVector::new(vec![
        ResidualDimension::new("a", 5.0, (0.0, 10.0)),   // passing
        ResidualDimension::new("b", 100.0, (0.0, 10.0)), // still failing
        ResidualDimension::new("c", 100.0, (0.0, 10.0)), // still failing
    ]);

    let report = RepairAdmissionReport::compute("reduce-a-op", before, after);
    assert!(report.admitted, "fewer failures after repair → admitted");
    // The detail must reference the before/after failing counts (3 → 2)
    assert!(
        report.detail.contains('3') && report.detail.contains('2'),
        "detail must reference before (3) and after (2) failing counts; got: {}",
        report.detail
    );
}
