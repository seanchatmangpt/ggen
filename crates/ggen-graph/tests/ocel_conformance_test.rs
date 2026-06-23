//! Chicago TDD integration tests for OCEL conformance and discovery.
//!
//! Real OCEL event fixtures from pack lifecycle (install, remove, verify, publish)
//! with real SPARQL conformance checks and process discovery.
//! No mocks, no test doubles — real OCEL events, real graph operations.

use chrono::{TimeZone, Utc};
use ggen_graph::{
    ocel::{
        emit_lockfile_write, emit_pack_install, emit_pack_publish, emit_pack_remove,
        emit_pack_verify, lockfile_entry_object, pack_object, receipt_object,
        ConformanceReport, OcelConformanceChecker, OcelEvent, OcelLog, OcelObjectRef,
        OcelProcessDiscovery, ProcessModel,
    },
    GraphError,
};
use std::collections::HashMap;

fn ts(secs: i64) -> chrono::DateTime<chrono::Utc> {
    Utc.timestamp_opt(secs, 0)
        .single()
        .unwrap_or_else(Utc::now)
}

// Real pack lifecycle constants
const PACK_ID_BASE: &str = "acme/base";
const PACK_ID_EXTENDED: &str = "acme/extended";
const VERSION_1_0: &str = "1.0.0";
const VERSION_1_1: &str = "1.1.0";
const DIGEST_BASE: &str = "3a7bd3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4f1b";
const DIGEST_EXTENDED: &str = "b1f7f3e2360a3d29eea436fcfb7e44c735d117c42d1c1835420b6b9942dd4abc";
const OPERATION_ID_PUBLISH: &str = "11111111-1111-4111-8111-111111111111";
const OPERATION_ID_REMOVE: &str = "22222222-2222-4222-8222-222222222222";

/// Helper: Create a real OCEL log for a lawful pack lifecycle (install → verify → publish).
fn create_lawful_pack_log() -> OcelLog {
    let mut log = OcelLog::new();

    // Objects: pack, lockfile-entry, receipt
    log.objects
        .push(pack_object(PACK_ID_BASE, VERSION_1_0, DIGEST_BASE));
    log.objects
        .push(lockfile_entry_object(PACK_ID_BASE, VERSION_1_0, DIGEST_BASE));
    log.objects
        .push(receipt_object(OPERATION_ID_PUBLISH, "c2lnbmF0dXJl"));

    // Events: install → verify → publish (lawful ordering)
    log.events
        .push(emit_pack_install("ev_install_1", ts(10), PACK_ID_BASE, VERSION_1_0));
    log.events
        .push(emit_pack_verify("ev_verify_1", ts(20), PACK_ID_BASE, VERSION_1_0));
    log.events.push(emit_pack_publish(
        "ev_publish_1",
        ts(30),
        PACK_ID_BASE,
        VERSION_1_0,
        OPERATION_ID_PUBLISH,
    ));

    log
}

/// Helper: Create a real OCEL log with a variant path (install → remove).
fn create_variant_pack_log() -> OcelLog {
    let mut log = OcelLog::new();

    log.objects
        .push(pack_object(PACK_ID_BASE, VERSION_1_1, DIGEST_BASE));
    log.objects
        .push(lockfile_entry_object(PACK_ID_BASE, VERSION_1_1, DIGEST_BASE));
    log.objects
        .push(receipt_object(OPERATION_ID_REMOVE, "c2lnbmF0dXJlMg=="));

    // Events: install → remove (variant path, no verify/publish)
    log.events
        .push(emit_pack_install("ev_install_2", ts(40), PACK_ID_BASE, VERSION_1_1));
    log.events
        .push(emit_pack_remove("ev_remove_2", ts(50), PACK_ID_BASE, VERSION_1_1));

    log
}

/// Helper: Create a real OCEL log with mixed object types (pack + lockfile-entry + receipt).
fn create_mixed_object_log() -> OcelLog {
    let mut log = OcelLog::new();

    // Multiple objects
    log.objects
        .push(pack_object(PACK_ID_BASE, VERSION_1_0, DIGEST_BASE));
    log.objects
        .push(pack_object(PACK_ID_EXTENDED, VERSION_1_0, DIGEST_EXTENDED));
    log.objects
        .push(lockfile_entry_object(PACK_ID_BASE, VERSION_1_0, DIGEST_BASE));
    log.objects
        .push(lockfile_entry_object(
            PACK_ID_EXTENDED,
            VERSION_1_0,
            DIGEST_EXTENDED,
        ));
    log.objects
        .push(receipt_object(OPERATION_ID_PUBLISH, "c2lnbmF0dXJl"));

    // Pack 1: install → verify → publish
    log.events
        .push(emit_pack_install("ev_install_1", ts(10), PACK_ID_BASE, VERSION_1_0));
    log.events
        .push(emit_lockfile_write("ev_lock_1", ts(15), PACK_ID_BASE, VERSION_1_0));
    log.events
        .push(emit_pack_verify("ev_verify_1", ts(20), PACK_ID_BASE, VERSION_1_0));
    log.events.push(emit_pack_publish(
        "ev_publish_1",
        ts(30),
        PACK_ID_BASE,
        VERSION_1_0,
        OPERATION_ID_PUBLISH,
    ));

    // Pack 2: install → verify (incomplete)
    log.events.push(emit_pack_install(
        "ev_install_2",
        ts(40),
        PACK_ID_EXTENDED,
        VERSION_1_0,
    ));
    log.events.push(emit_lockfile_write(
        "ev_lock_2",
        ts(45),
        PACK_ID_EXTENDED,
        VERSION_1_0,
    ));
    log.events.push(emit_pack_verify(
        "ev_verify_2",
        ts(50),
        PACK_ID_EXTENDED,
        VERSION_1_0,
    ));

    log
}

#[test]
fn conformance_lawful_pack_lifecycle_against_valid_model() -> Result<(), GraphError> {
    // Arrange: real OCEL log from lawful pack lifecycle
    let log = create_lawful_pack_log();
    let expected_model = "pack.install -> pack.verify -> pack.publish";

    // Act: check conformance
    let report = OcelConformanceChecker::check(&log, expected_model)?;

    // Assert: high fitness, no deviations
    assert!(
        report.fitness >= 0.9,
        "lawful pack lifecycle should conform with high fitness (got {:.2})",
        report.fitness
    );
    assert!(
        report.deviations.is_empty(),
        "lawful lifecycle should have no deviations, got: {:?}",
        report.deviations
    );
    assert_eq!(log.events.len(), 3, "pack lifecycle should have 3 events");
    Ok(())
}

#[test]
fn conformance_pack_lifecycle_against_invalid_model_detects_deviations() -> Result<(), GraphError> {
    // Arrange: pack lifecycle log
    let log = create_lawful_pack_log();
    // Invalid model that doesn't include the expected activities
    let invalid_model = "some.other.activity -> another.activity";

    // Act: check conformance against invalid model
    let report = OcelConformanceChecker::check(&log, invalid_model)?;

    // Assert: low fitness, deviations detected
    assert!(
        report.fitness < 1.0,
        "conformance against invalid model should have low fitness (got {:.2})",
        report.fitness
    );
    assert!(
        !report.deviations.is_empty(),
        "invalid model should produce deviations, got empty list"
    );
    Ok(())
}

#[test]
fn discovery_canonical_path_for_lawful_pack_lifecycle() -> Result<(), GraphError> {
    // Arrange: real OCEL log with lawful pack lifecycle
    let log = create_lawful_pack_log();

    // Act: discover process models
    let models = OcelProcessDiscovery::discover(&log)?;

    // Assert: one model (pack type), canonical path is install → verify → publish
    assert_eq!(models.len(), 1, "should discover one process model");

    let pack_model = &models[0];
    assert_eq!(pack_model.object_type, "pack", "model should be for pack type");
    assert_eq!(
        pack_model.canonical_path,
        vec!["pack.install", "pack.verify", "pack.publish"],
        "canonical path should be the complete lifecycle"
    );
    assert_eq!(pack_model.variant_count, 1, "only one execution path");
    assert_eq!(pack_model.variant_ratio, 0.0, "all instances follow canonical path");
    Ok(())
}

#[test]
fn discovery_detects_variant_paths() -> Result<(), GraphError> {
    // Arrange: combine lawful and variant pack logs
    let mut combined = create_lawful_pack_log();
    let variant = create_variant_pack_log();

    // Merge events
    combined.events.extend(variant.events);
    combined.objects.extend(variant.objects);

    // Act: discover process models
    let models = OcelProcessDiscovery::discover(&combined)?;

    // Assert: one pack model with two variants
    assert_eq!(models.len(), 1, "should discover one process model (pack)");

    let pack_model = &models[0];
    assert_eq!(pack_model.variant_count, 2, "two distinct execution paths detected");
    assert!(
        pack_model.variant_ratio > 0.0 && pack_model.variant_ratio < 1.0,
        "variant ratio should be between 0 and 1 (got {:.2})",
        pack_model.variant_ratio
    );
    // Canonical path should be the longer one (more common: install → verify → publish)
    assert_eq!(pack_model.canonical_path.len(), 3);
    Ok(())
}

#[test]
fn discovery_multiple_object_types() -> Result<(), GraphError> {
    // Arrange: real OCEL log with pack, lockfile-entry, and receipt objects
    let log = create_mixed_object_log();

    // Act: discover process models
    let models = OcelProcessDiscovery::discover(&log)?;

    // Assert: discover models for pack and lockfile-entry
    assert!(
        models.len() >= 2,
        "should discover at least two process models, got {}",
        models.len()
    );

    let pack_models: Vec<_> = models.iter().filter(|m| m.object_type == "pack").collect();
    let lock_models: Vec<_> = models
        .iter()
        .filter(|m| m.object_type == "lockfile-entry")
        .collect();

    assert_eq!(pack_models.len(), 1, "should have one pack model");
    assert_eq!(lock_models.len(), 1, "should have one lockfile-entry model");

    // Pack model should show variant paths
    let pack_model = pack_models[0];
    assert!(
        pack_model.variant_count >= 1,
        "pack model should have at least one variant"
    );

    // Lockfile-entry model should show lockfile.write activity
    let lock_model = lock_models[0];
    assert!(
        lock_model
            .canonical_path
            .iter()
            .any(|a| a.contains("lockfile")),
        "lockfile model should contain lockfile activities"
    );
    Ok(())
}

#[test]
fn conformance_with_all_pack_lifecycle_activities() -> Result<(), GraphError> {
    // Arrange: comprehensive log with install, verify, publish, remove, lockfile.write
    let mut log = OcelLog::new();

    log.objects
        .push(pack_object(PACK_ID_BASE, VERSION_1_0, DIGEST_BASE));
    log.objects
        .push(lockfile_entry_object(PACK_ID_BASE, VERSION_1_0, DIGEST_BASE));
    log.objects
        .push(receipt_object(OPERATION_ID_PUBLISH, "c2lnbmF0dXJl"));
    log.objects
        .push(receipt_object(OPERATION_ID_REMOVE, "c2lnbmF0dXJlMg=="));

    // Real events in complete lifecycle: install → lockfile.write → verify → publish → remove
    log.events
        .push(emit_pack_install("ev_install", ts(10), PACK_ID_BASE, VERSION_1_0));
    log.events
        .push(emit_lockfile_write("ev_lock_write", ts(15), PACK_ID_BASE, VERSION_1_0));
    log.events
        .push(emit_pack_verify("ev_verify", ts(20), PACK_ID_BASE, VERSION_1_0));
    log.events.push(emit_pack_publish(
        "ev_publish",
        ts(30),
        PACK_ID_BASE,
        VERSION_1_0,
        OPERATION_ID_PUBLISH,
    ));
    log.events
        .push(emit_pack_remove("ev_remove", ts(40), PACK_ID_BASE, VERSION_1_0));

    // Act: check conformance with a model that covers the full lifecycle
    let model = "pack.install -> pack.verify -> pack.publish -> pack.remove";
    let report = OcelConformanceChecker::check(&log, model)?;

    // Assert: fitness should be reasonable (may not be 1.0 because of lockfile.write)
    assert!(
        report.fitness >= 0.6,
        "comprehensive lifecycle should have decent fitness (got {:.2})",
        report.fitness
    );
    Ok(())
}

#[test]
fn discovery_captures_variant_ratio_accurately() -> Result<(), GraphError> {
    // Arrange: log with 3 pack instances: 2 canonical, 1 variant
    let mut log = OcelLog::new();

    // Three pack versions
    log.objects.push(pack_object(PACK_ID_BASE, "1.0.0", DIGEST_BASE));
    log.objects.push(pack_object(PACK_ID_BASE, "1.1.0", DIGEST_BASE));
    log.objects
        .push(pack_object(PACK_ID_EXTENDED, "1.0.0", DIGEST_EXTENDED));

    // Pack 1.0.0: canonical (install → verify → publish)
    log.events
        .push(emit_pack_install("ev1_install", ts(10), PACK_ID_BASE, "1.0.0"));
    log.events
        .push(emit_pack_verify("ev1_verify", ts(20), PACK_ID_BASE, "1.0.0"));
    log.events.push(emit_pack_publish(
        "ev1_publish",
        ts(30),
        PACK_ID_BASE,
        "1.0.0",
        OPERATION_ID_PUBLISH,
    ));

    // Pack 1.1.0: canonical (install → verify → publish)
    log.events
        .push(emit_pack_install("ev2_install", ts(40), PACK_ID_BASE, "1.1.0"));
    log.events
        .push(emit_pack_verify("ev2_verify", ts(50), PACK_ID_BASE, "1.1.0"));
    log.events.push(emit_pack_publish(
        "ev2_publish",
        ts(60),
        PACK_ID_BASE,
        "1.1.0",
        OPERATION_ID_PUBLISH,
    ));

    // Pack extended 1.0.0: variant (install → remove)
    log.events.push(emit_pack_install(
        "ev3_install",
        ts(70),
        PACK_ID_EXTENDED,
        "1.0.0",
    ));
    log.events.push(emit_pack_remove(
        "ev3_remove",
        ts(80),
        PACK_ID_EXTENDED,
        "1.0.0",
    ));

    // Act: discover process models
    let models = OcelProcessDiscovery::discover(&log)?;

    // Assert: variant_ratio should be approximately 1/3 (one variant out of three instances)
    let pack_models: Vec<_> = models.iter().filter(|m| m.object_type == "pack").collect();
    assert_eq!(pack_models.len(), 1);

    let pack_model = pack_models[0];
    // 1 out of 3 instances follows variant: expected ratio ≈ 0.333
    let expected_ratio = 1.0 / 3.0;
    assert!(
        (pack_model.variant_ratio - expected_ratio).abs() < 0.01,
        "variant_ratio should be approximately {:.3} (got {:.3})",
        expected_ratio,
        pack_model.variant_ratio
    );
    Ok(())
}

#[test]
fn conformance_empty_log_returns_perfect_fitness() -> Result<(), GraphError> {
    // Arrange: empty OCEL log
    let log = OcelLog::new();
    let model = "pack.install -> pack.verify";

    // Act: check conformance on empty log
    let report = OcelConformanceChecker::check(&log, model)?;

    // Assert: empty log should have perfect fitness (no violations)
    assert_eq!(
        report.fitness, 1.0,
        "empty log should have perfect fitness"
    );
    assert!(
        report.deviations.is_empty(),
        "empty log should have no deviations"
    );
    Ok(())
}

#[test]
fn discovery_empty_log_returns_empty_models() -> Result<(), GraphError> {
    // Arrange: empty OCEL log
    let log = OcelLog::new();

    // Act: discover process models
    let models = OcelProcessDiscovery::discover(&log)?;

    // Assert: no models discovered
    assert!(
        models.is_empty(),
        "empty log should produce no models, got {:?}",
        models
    );
    Ok(())
}

#[test]
fn conformance_report_structure_is_valid() -> Result<(), GraphError> {
    // Arrange: real pack log
    let log = create_lawful_pack_log();
    let model = "pack.install -> pack.verify -> pack.publish";

    // Act: generate conformance report
    let report = OcelConformanceChecker::check(&log, model)?;

    // Assert: report structure is valid and values are in expected ranges
    assert!(
        (0.0..=1.0).contains(&report.fitness),
        "fitness should be between 0.0 and 1.0 (got {})",
        report.fitness
    );
    assert!(
        (0.0..=1.0).contains(&report.precision),
        "precision should be between 0.0 and 1.0 (got {})",
        report.precision
    );
    // Deviations list is okay to be empty for conforming logs
    Ok(())
}

#[test]
fn process_model_structure_is_valid() -> Result<(), GraphError> {
    // Arrange: real pack log
    let log = create_lawful_pack_log();

    // Act: discover process models
    let models = OcelProcessDiscovery::discover(&log)?;

    // Assert: all models have valid structure
    for model in models {
        assert!(
            !model.object_type.is_empty(),
            "object_type must be non-empty"
        );
        // canonical_path can be empty if no objects of that type
        assert!(
            model.variant_count >= 1,
            "variant_count should be at least 1"
        );
        assert!(
            (0.0..=1.0).contains(&model.variant_ratio),
            "variant_ratio should be between 0.0 and 1.0 (got {})",
            model.variant_ratio
        );
    }
    Ok(())
}
