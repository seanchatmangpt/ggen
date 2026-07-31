use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};
use thiserror::Error;

pub const TESTING_BBLOCK_PROTOCOL_ID: &str = "GBB-TESTING-PROTOCOL-001";
pub const TESTING_BBLOCK_PROTOCOL_VERSION: &str = "26.7.31";
const EXPECTED_SUITES: usize = 10;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TestingSuiteKind {
    ProtocolUnit,
    PropertyFuzz,
    StdioHttpIntegration,
    BlackBoxCliE2e,
    Security,
    Chaos,
    Stress,
    Benchmark,
    Replay,
    VerifierReport,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum TestingSuiteStatus {
    Alive,
    PendingCheckpoint,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum TestingBblockStanding {
    PartialAlive,
    Alive,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TestingSuite {
    pub id: String,
    pub kind: TestingSuiteKind,
    pub status: TestingSuiteStatus,
    pub acceptance: String,
    pub falsifier: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TestingBblockProtocol {
    pub id: String,
    pub version: String,
    pub suites: Vec<TestingSuite>,
}

impl TestingBblockProtocol {
    pub fn validate(&self) -> Result<(), TestingBblockRefusal> {
        if self.id != TESTING_BBLOCK_PROTOCOL_ID {
            return Err(TestingBblockRefusal::IdentityMismatch(self.id.clone()));
        }
        if self.version != TESTING_BBLOCK_PROTOCOL_VERSION {
            return Err(TestingBblockRefusal::VersionMismatch(self.version.clone()));
        }
        if self.suites.len() != EXPECTED_SUITES {
            return Err(TestingBblockRefusal::CardinalityMismatch {
                expected: EXPECTED_SUITES,
                observed: self.suites.len(),
            });
        }
        let mut identities = BTreeSet::new();
        let mut kinds = BTreeSet::new();
        for suite in &self.suites {
            if suite.id.trim().is_empty()
                || suite.acceptance.trim().is_empty()
                || suite.falsifier.trim().is_empty()
            {
                return Err(TestingBblockRefusal::IncompleteSuite(suite.id.clone()));
            }
            if !identities.insert(suite.id.clone()) {
                return Err(TestingBblockRefusal::DuplicateSuite(suite.id.clone()));
            }
            if !kinds.insert(suite.kind) {
                return Err(TestingBblockRefusal::DuplicateKind(suite.kind));
            }
        }
        let required = BTreeSet::from([
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
        ]);
        if kinds != required {
            return Err(TestingBblockRefusal::SuiteKindClosureMismatch);
        }
        Ok(())
    }

    #[must_use]
    pub fn alive_count(&self) -> usize {
        self.suites
            .iter()
            .filter(|suite| suite.status == TestingSuiteStatus::Alive)
            .count()
    }

    #[must_use]
    pub fn pending_count(&self) -> usize {
        self.suites
            .iter()
            .filter(|suite| suite.status == TestingSuiteStatus::PendingCheckpoint)
            .count()
    }

    #[must_use]
    pub fn standing(&self) -> TestingBblockStanding {
        if self.pending_count() == 0 {
            TestingBblockStanding::Alive
        } else {
            TestingBblockStanding::PartialAlive
        }
    }
}

#[must_use]
pub fn testing_bblock_protocol() -> TestingBblockProtocol {
    let rows = [
        (
            "TEST-PROTOCOL-UNIT",
            TestingSuiteKind::ProtocolUnit,
            TestingSuiteStatus::Alive,
            "cargo test --manifest-path crates/ggen-architecture/Cargo.toml --workspace --all-targets certification::tests",
            "break an identity, lifecycle, evidence, standards, or receipt law and require a deterministic failure",
        ),
        (
            "TEST-PROPERTY-FUZZ",
            TestingSuiteKind::PropertyFuzz,
            TestingSuiteStatus::PendingCheckpoint,
            "execute generated property and fuzz corpora over GBB composition, standards digests, and receipt replay",
            "find one admitted input whose deterministic replay, ceiling, identity, or refusal invariant is violated",
        ),
        (
            "TEST-STDIO-HTTP-INTEGRATION",
            TestingSuiteKind::StdioHttpIntegration,
            TestingSuiteStatus::PendingCheckpoint,
            "execute equivalent admitted requests through real stdio and HTTP boundaries and compare receipts",
            "observe protocol-dependent semantic, authority, output, or receipt divergence",
        ),
        (
            "TEST-BLACKBOX-CLI-E2E",
            TestingSuiteKind::BlackBoxCliE2e,
            TestingSuiteStatus::Alive,
            "build the real ggen CLI, manufacture TAI twice, execute seven scenarios, and verify receipts",
            "remove one generated artifact, scenario, receipt, or replay edge and require the black-box workflow to fail",
        ),
        (
            "TEST-SECURITY",
            TestingSuiteKind::Security,
            TestingSuiteStatus::Alive,
            "verify BRCE-only authority, direct-actuation exclusion, tamper refusal, and bounded resource law",
            "introduce direct process/network/cloud authority, a stale digest, or a passport/resource expansion",
        ),
        (
            "TEST-CHAOS",
            TestingSuiteKind::Chaos,
            TestingSuiteStatus::Alive,
            "execute delayed-contract, unavailable-certification, failed-inspection, founder-unavailable, unknown-scenario, and tamper paths",
            "allow one injected failure to disappear, self-heal without a receipt, or collapse into successful standing",
        ),
        (
            "TEST-STRESS",
            TestingSuiteKind::Stress,
            TestingSuiteStatus::PendingCheckpoint,
            "execute bounded maximum-cardinality registries, dependency closures, evidence ledgers, and projection outputs",
            "exceed a declared resource/cardinality ceiling without typed refusal or deterministic degradation receipt",
        ),
        (
            "TEST-BENCHMARK",
            TestingSuiteKind::Benchmark,
            TestingSuiteStatus::PendingCheckpoint,
            "measure manufacture, composition, simulation, verification, and replay against declared baselines",
            "accept a regression without an exact environment, input digest, baseline, and machine-readable benchmark receipt",
        ),
        (
            "TEST-REPLAY",
            TestingSuiteKind::Replay,
            TestingSuiteStatus::Alive,
            "recompute standards, composition, artifact, scenario, and certification receipts from exact admitted inputs",
            "change ambient paths, ordering, timestamps, or mutable state and observe a different semantic result",
        ),
        (
            "TEST-VERIFIER-REPORT",
            TestingSuiteKind::VerifierReport,
            TestingSuiteStatus::Alive,
            "emit machine-readable TAI and seven-day standards verifier reports with typed standing and exclusions",
            "omit the exact head, profile digest, suite standing, broker boundary, or unresolved checkpoint count",
        ),
    ];
    TestingBblockProtocol {
        id: TESTING_BBLOCK_PROTOCOL_ID.to_string(),
        version: TESTING_BBLOCK_PROTOCOL_VERSION.to_string(),
        suites: rows
            .into_iter()
            .map(|(id, kind, status, acceptance, falsifier)| TestingSuite {
                id: id.to_string(),
                kind,
                status,
                acceptance: acceptance.to_string(),
                falsifier: falsifier.to_string(),
            })
            .collect(),
    }
}

#[derive(Debug, Error)]
pub enum TestingBblockRefusal {
    #[error("testing Building Block identity mismatch: `{0}`")]
    IdentityMismatch(String),
    #[error("testing Building Block version mismatch: `{0}`")]
    VersionMismatch(String),
    #[error("testing suite cardinality mismatch: expected {expected}, observed {observed}")]
    CardinalityMismatch { expected: usize, observed: usize },
    #[error("testing suite is incomplete: `{0}`")]
    IncompleteSuite(String),
    #[error("testing suite is duplicated: `{0}`")]
    DuplicateSuite(String),
    #[error("testing suite kind is duplicated: `{0:?}`")]
    DuplicateKind(TestingSuiteKind),
    #[error("testing suite kind closure does not contain the ten canonical suites")]
    SuiteKindClosureMismatch,
}
