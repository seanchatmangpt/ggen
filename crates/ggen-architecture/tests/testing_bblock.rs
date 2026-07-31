use ggen_architecture::{
    seven_day_standards_profile, testing_bblock_protocol, TestingBblockStanding,
    TestingSuiteKind, TestingSuiteStatus, TESTING_BBLOCK_PROTOCOL_ID,
};

#[test]
fn testing_bblock_has_ten_distinct_suites_and_partial_standing() {
    let protocol = testing_bblock_protocol();
    assert!(protocol.validate().is_ok());
    assert_eq!(protocol.id, TESTING_BBLOCK_PROTOCOL_ID);
    assert_eq!(protocol.suites.len(), 10);
    assert_eq!(protocol.alive_count(), 6);
    assert_eq!(protocol.pending_count(), 4);
    assert_eq!(protocol.standing(), TestingBblockStanding::PartialAlive);
}

#[test]
fn testing_bblock_preserves_every_required_suite_boundary() {
    let protocol = testing_bblock_protocol();
    let kinds = protocol
        .suites
        .iter()
        .map(|suite| suite.kind)
        .collect::<std::collections::BTreeSet<_>>();
    assert_eq!(
        kinds,
        std::collections::BTreeSet::from([
            TestingSuiteKind::ProtocolUnit,
            TestingSuiteKind::PropertyFuzz,
            TestingSuiteKind::StdioHttpIntegration,
            TestingSuiteKind::BlackBoxCliE2e,
            TestingSuiteKind::Security,
            TestingSuiteKind::Chaos,
            TestingSuiteKind::Stress,
            TestingSuiteKind::Benchmark,
            TestingSuiteKind::Replay,
            TestingSuiteKind::VerifierReport,
        ])
    );
}

#[test]
fn incomplete_testing_bblock_is_refused() {
    let mut protocol = testing_bblock_protocol();
    protocol.suites.pop();
    assert!(protocol.validate().is_err());
}

#[test]
fn seven_day_profile_binds_the_same_testing_protocol() {
    let profile = seven_day_standards_profile();
    assert!(profile.validate().is_ok());
    assert_eq!(profile.testing_bblock, testing_bblock_protocol());
    assert!(profile.testing_bblock.suites.iter().any(|suite| {
        suite.kind == TestingSuiteKind::PropertyFuzz
            && suite.status == TestingSuiteStatus::PendingCheckpoint
    }));
}

#[test]
fn testing_protocol_changes_the_standards_profile_digest() {
    let original = seven_day_standards_profile();
    let mut changed = original.clone();
    changed.testing_bblock.suites[0]
        .acceptance
        .push_str(" changed");
    assert_ne!(
        original.digest().expect("original standards digest"),
        changed.digest().expect("changed testing protocol digest")
    );
}
