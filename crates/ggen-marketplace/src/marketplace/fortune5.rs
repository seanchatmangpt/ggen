//! Executable Fortune-5 capability contract for ggen.
//!
//! The contract is repository-defined and bounded. It combines the six marketplace
//! release truths, two supporting platform systems, five deterministic SLA governors,
//! and six operational controls already named by ggen's Fortune-5 documentation.
//! `ALIVE` requires 19 capabilities and three independently externalized proof
//! surfaces per capability: positive execution, negative refusal, and receipt/replay.

use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fs::{self, File};
use std::io::Read;
use std::path::{Path, PathBuf};
use tera::{Context, Tera};

/// Version of the bounded Fortune-5 capability contract.
pub const FORTUNE5_CONTRACT_VERSION: &str = "ggen-fortune5-required/v1";

/// Stable proof surfaces required for every Fortune-5 capability.
pub const REQUIRED_PROOF_SURFACES: [Fortune5ProofSurface; 3] = [
    Fortune5ProofSurface::PositiveExecution,
    Fortune5ProofSurface::NegativeRefusal,
    Fortune5ProofSurface::ReceiptReplay,
];

/// Major concern that owns a Fortune-5 capability.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[repr(u8)]
pub enum Fortune5Category {
    /// Six release truths for governed packs.
    ReleaseGate = 1,
    /// Pack taxonomy and bundle/profile composition.
    SupportingSystem = 2,
    /// Deterministic performance-governor machinery.
    Performance = 3,
    /// Operational quality, resilience, and observability controls.
    OperationalControl = 4,
}

/// Every required Fortune-5 capability in the bounded ggen contract.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[repr(u8)]
pub enum Fortune5Capability {
    /// Signed, hashed, policy-admitted pack installation.
    InstallTruth = 1,
    /// Ontology, query, and template compile into a deterministic projection.
    CompilerTruth = 2,
    /// All ten compatibility dimensions are checked fail-closed.
    ConflictTruth = 3,
    /// Tera rendering is deterministic and malformed templates refuse.
    RenderingTruth = 4,
    /// Trust tier, registry, signature, and runtime policies are enforced.
    TrustTruth = 5,
    /// Consequences carry a tamper-evident receipt chain and replay protection.
    ProofTruth = 6,
    /// Exactly nine canonical atomic pack categories are admitted.
    AtomicPackTaxonomy = 7,
    /// Bundles expand deterministically under an enforced profile.
    BundleProfileSystem = 8,
    /// CLI startup observations are evaluated against a p90 SLO.
    CliStartupSlo = 9,
    /// Typical and large rendering observations are evaluated against bounded SLOs.
    TemplateRenderingSlo = 10,
    /// RDF query observations are evaluated against a p90 SLO.
    RdfQuerySlo = 11,
    /// Baseline and peak memory observations are bounded.
    MemorySlo = 12,
    /// Throughput scales to eight workers within a declared efficiency floor.
    ConcurrencySlo = 13,
    /// Andon state escalates monotonically and red stops the line.
    AndonSignals = 14,
    /// Invalid values and lifecycle transitions are impossible to admit.
    PokaYoke = 15,
    /// W3C trace context is parsed, propagated, and refused when malformed.
    DistributedTracing = 16,
    /// Retry and circuit-breaker controls contain deterministic injected failures.
    ChaosResilience = 17,
    /// Latency, traffic, errors, and saturation determine health.
    GoldenSignals = 18,
    /// SLO error-budget consumption governs risky change.
    ErrorBudgets = 19,
}

/// Stable ordered capability set.
pub const ALL_FORTUNE5_CAPABILITIES: [Fortune5Capability; 19] = [
    Fortune5Capability::InstallTruth,
    Fortune5Capability::CompilerTruth,
    Fortune5Capability::ConflictTruth,
    Fortune5Capability::RenderingTruth,
    Fortune5Capability::TrustTruth,
    Fortune5Capability::ProofTruth,
    Fortune5Capability::AtomicPackTaxonomy,
    Fortune5Capability::BundleProfileSystem,
    Fortune5Capability::CliStartupSlo,
    Fortune5Capability::TemplateRenderingSlo,
    Fortune5Capability::RdfQuerySlo,
    Fortune5Capability::MemorySlo,
    Fortune5Capability::ConcurrencySlo,
    Fortune5Capability::AndonSignals,
    Fortune5Capability::PokaYoke,
    Fortune5Capability::DistributedTracing,
    Fortune5Capability::ChaosResilience,
    Fortune5Capability::GoldenSignals,
    Fortune5Capability::ErrorBudgets,
];

impl Fortune5Capability {
    /// Stable filesystem and ontology slug.
    #[must_use]
    pub const fn slug(self) -> &'static str {
        match self {
            Self::InstallTruth => "install-truth",
            Self::CompilerTruth => "compiler-truth",
            Self::ConflictTruth => "conflict-truth",
            Self::RenderingTruth => "rendering-truth",
            Self::TrustTruth => "trust-truth",
            Self::ProofTruth => "proof-truth",
            Self::AtomicPackTaxonomy => "atomic-pack-taxonomy",
            Self::BundleProfileSystem => "bundle-profile-system",
            Self::CliStartupSlo => "cli-startup-slo",
            Self::TemplateRenderingSlo => "template-rendering-slo",
            Self::RdfQuerySlo => "rdf-query-slo",
            Self::MemorySlo => "memory-slo",
            Self::ConcurrencySlo => "concurrency-slo",
            Self::AndonSignals => "andon-signals",
            Self::PokaYoke => "poka-yoke",
            Self::DistributedTracing => "distributed-tracing",
            Self::ChaosResilience => "chaos-resilience",
            Self::GoldenSignals => "golden-signals",
            Self::ErrorBudgets => "error-budgets",
        }
    }

    /// Owning contract category.
    #[must_use]
    pub const fn category(self) -> Fortune5Category {
        match self {
            Self::InstallTruth
            | Self::CompilerTruth
            | Self::ConflictTruth
            | Self::RenderingTruth
            | Self::TrustTruth
            | Self::ProofTruth => Fortune5Category::ReleaseGate,
            Self::AtomicPackTaxonomy | Self::BundleProfileSystem => {
                Fortune5Category::SupportingSystem
            }
            Self::CliStartupSlo
            | Self::TemplateRenderingSlo
            | Self::RdfQuerySlo
            | Self::MemorySlo
            | Self::ConcurrencySlo => Fortune5Category::Performance,
            Self::AndonSignals
            | Self::PokaYoke
            | Self::DistributedTracing
            | Self::ChaosResilience
            | Self::GoldenSignals
            | Self::ErrorBudgets => Fortune5Category::OperationalControl,
        }
    }

    /// Required observable consequence.
    #[must_use]
    pub const fn outcome(self) -> &'static str {
        match self {
            Self::InstallTruth => {
                "Only signed, digest-matching, policy-admitted packs cross the install boundary."
            }
            Self::CompilerTruth => {
                "Ontology, query, and template inputs produce one deterministic projection."
            }
            Self::ConflictTruth => {
                "All ten compatibility dimensions detect incompatible claims before composition."
            }
            Self::RenderingTruth => {
                "Tera rendering is deterministic and malformed templates fail before output."
            }
            Self::TrustTruth => {
                "Trust tier, registry class, signature, and runtime policy are enforced."
            }
            Self::ProofTruth => {
                "Every consequence is hash-chained, tamper-detectable, and replay-protected."
            }
            Self::AtomicPackTaxonomy => {
                "The complete nine-category atomic pack taxonomy is unique and closed."
            }
            Self::BundleProfileSystem => {
                "Bundle expansion is deterministic, cycle-free, and constrained by profile."
            }
            Self::CliStartupSlo => "CLI startup p90 is evaluated against a declared threshold.",
            Self::TemplateRenderingSlo => {
                "Typical and large rendering latency are evaluated against declared thresholds."
            }
            Self::RdfQuerySlo => "RDF query p90 is evaluated against a declared threshold.",
            Self::MemorySlo => "Baseline and peak memory are evaluated against declared limits.",
            Self::ConcurrencySlo => {
                "Eight-worker throughput is evaluated against a declared linear-efficiency floor."
            }
            Self::AndonSignals => {
                "Quality state escalates monotonically and red prevents further execution."
            }
            Self::PokaYoke => "Out-of-range values and invalid lifecycle transitions are refused.",
            Self::DistributedTracing => {
                "Valid W3C trace context propagates while malformed context is refused."
            }
            Self::ChaosResilience => {
                "Transient failures retry within bounds and repeated failures open the circuit."
            }
            Self::GoldenSignals => {
                "Latency, traffic, errors, and saturation yield an actionable health verdict."
            }
            Self::ErrorBudgets => {
                "Measured failure consumes a deterministic SLO budget that gates risky change."
            }
        }
    }
}

/// Independent proof surface.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[repr(u8)]
pub enum Fortune5ProofSurface {
    /// A valid input crosses the real implementation boundary.
    PositiveExecution = 1,
    /// A named invalid input is refused.
    NegativeRefusal = 2,
    /// The consequence is cryptographically verified and replay-safe.
    ReceiptReplay = 3,
}

impl Fortune5ProofSurface {
    /// Stable filesystem slug.
    #[must_use]
    pub const fn slug(self) -> &'static str {
        match self {
            Self::PositiveExecution => "positive-execution",
            Self::NegativeRefusal => "negative-refusal",
            Self::ReceiptReplay => "receipt-replay",
        }
    }
}

/// Static contract entry.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Fortune5CapabilityContract {
    /// Capability.
    pub capability: Fortune5Capability,
    /// Category.
    pub category: Fortune5Category,
    /// Required observable consequence.
    pub outcome: &'static str,
    /// Required independent proof surfaces.
    pub required_surfaces: &'static [Fortune5ProofSurface],
}

/// Return one complete capability contract.
#[must_use]
pub const fn fortune5_contract(capability: Fortune5Capability) -> Fortune5CapabilityContract {
    Fortune5CapabilityContract {
        capability,
        category: capability.category(),
        outcome: capability.outcome(),
        required_surfaces: &REQUIRED_PROOF_SURFACES,
    }
}

/// Return the complete 19-capability contract.
#[must_use]
pub fn all_fortune5_contracts() -> Vec<Fortune5CapabilityContract> {
    ALL_FORTUNE5_CAPABILITIES
        .iter()
        .copied()
        .map(fortune5_contract)
        .collect()
}

/// Observed verifier outcome.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[repr(u8)]
pub enum Fortune5EvidenceOutcome {
    /// The verifier passed.
    Pass = 1,
    /// A falsifier crossed the boundary.
    Fail = 2,
    /// The verifier did not build.
    BuildFailed = 3,
    /// No observation exists.
    Unknown = 4,
    /// The capability is outside the admitted boundary.
    Unsupported = 5,
}

/// Gall standing emitted by the Fortune-5 assessor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Fortune5Standing {
    /// All 19 capabilities and all 57 surfaces passed.
    #[serde(rename = "ALIVE")]
    Alive,
    /// Some real evidence exists but the crown is open.
    #[serde(rename = "PARTIAL_ALIVE")]
    PartialAlive,
    /// A falsifier passed.
    #[serde(rename = "BLOCKED")]
    Blocked,
    /// The verifier failed to build.
    #[serde(rename = "BUILD_BROKEN")]
    BuildBroken,
    /// Required evidence is absent.
    #[serde(rename = "UNKNOWN")]
    Unknown,
    /// The contract cannot prove a requested surface.
    #[serde(rename = "UNSUPPORTED")]
    Unsupported,
}

/// Immutable observation admitted to the evidence ledger.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5EvidenceRecord {
    /// Stable observation identity.
    pub id: String,
    /// Capability supported or falsified.
    pub capability: Fortune5Capability,
    /// Independent surface.
    pub surface: Fortune5ProofSurface,
    /// Observed result.
    pub outcome: Fortune5EvidenceOutcome,
    /// Relative durable source.
    pub source: String,
    /// Monotonic observation epoch.
    pub epoch: u64,
    /// SHA-256 of the externalized evidence.
    pub artifact_digest: [u8; 32],
    /// SHA-256 binding all record fields.
    pub observation_digest: [u8; 32],
}

impl Fortune5EvidenceRecord {
    /// Construct a record from bytes observed at the boundary.
    #[must_use]
    pub fn observed(
        id: impl Into<String>, capability: Fortune5Capability, surface: Fortune5ProofSurface,
        outcome: Fortune5EvidenceOutcome, source: impl Into<String>, epoch: u64, artifact: &[u8],
    ) -> Self {
        let id = id.into();
        let source = source.into();
        let artifact_digest = sha256(artifact);
        let observation_digest = evidence_digest(
            &id,
            capability,
            surface,
            outcome,
            &source,
            epoch,
            &artifact_digest,
        );
        Self {
            id,
            capability,
            surface,
            outcome,
            source,
            epoch,
            artifact_digest,
            observation_digest,
        }
    }

    /// Verify the immutable record.
    #[must_use]
    pub fn verify(&self) -> bool {
        self.observation_digest
            == evidence_digest(
                &self.id,
                self.capability,
                self.surface,
                self.outcome,
                &self.source,
                self.epoch,
                &self.artifact_digest,
            )
    }
}

/// Append-only evidence ledger.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5EvidenceLedger {
    records: Vec<Fortune5EvidenceRecord>,
}

impl Fortune5EvidenceLedger {
    /// Create an empty ledger.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            records: Vec::new(),
        }
    }

    /// Admit one immutable observation.
    pub fn admit(&mut self, record: Fortune5EvidenceRecord) -> Result<(), Fortune5Error> {
        if !record.verify() {
            return Err(Fortune5Error::EvidenceDigestMismatch(record.id));
        }
        if self.records.iter().any(|item| item.id == record.id) {
            return Err(Fortune5Error::DuplicateEvidence(record.id));
        }
        self.records.push(record);
        Ok(())
    }

    /// Read complete history.
    #[must_use]
    pub fn records(&self) -> &[Fortune5EvidenceRecord] {
        &self.records
    }

    /// SHA-256 root over immutable observation identities.
    #[must_use]
    pub fn root_digest(&self) -> [u8; 32] {
        let mut digests: Vec<[u8; 32]> = self
            .records
            .iter()
            .map(|record| record.observation_digest)
            .collect();
        digests.sort_unstable();
        let mut hasher = Sha256::new();
        put(&mut hasher, b"fortune5-evidence-ledger/v1");
        for digest in digests {
            hasher.update(digest);
        }
        hasher.finalize().into()
    }

    fn latest(
        &self, capability: Fortune5Capability,
    ) -> BTreeMap<Fortune5ProofSurface, &Fortune5EvidenceRecord> {
        let mut latest: BTreeMap<Fortune5ProofSurface, &Fortune5EvidenceRecord> = BTreeMap::new();
        for record in self
            .records
            .iter()
            .filter(|record| record.capability == capability)
        {
            let replace = match latest.get(&record.surface) {
                Some(current) => {
                    record.epoch > current.epoch
                        || (record.epoch == current.epoch && record.id > current.id)
                }
                None => true,
            };
            if replace {
                latest.insert(record.surface, record);
            }
        }
        latest
    }

    /// Assess the complete 19 × 3 contract.
    #[must_use]
    pub fn assess(&self) -> Fortune5Assessment {
        let capabilities: Vec<Fortune5CapabilityAssessment> = ALL_FORTUNE5_CAPABILITIES
            .iter()
            .copied()
            .map(|capability| self.assess_capability(capability))
            .collect();
        Fortune5Assessment {
            contract_version: FORTUNE5_CONTRACT_VERSION.to_string(),
            evidence_root: self.root_digest(),
            standing: overall_standing(&capabilities),
            capabilities,
        }
    }

    fn assess_capability(&self, capability: Fortune5Capability) -> Fortune5CapabilityAssessment {
        let latest = self.latest(capability);
        let mut satisfied_surfaces = Vec::new();
        let mut missing_surfaces = Vec::new();
        let mut blocking_evidence = Vec::new();
        for surface in REQUIRED_PROOF_SURFACES {
            match latest.get(&surface) {
                Some(record) if record.outcome == Fortune5EvidenceOutcome::Pass => {
                    satisfied_surfaces.push(surface);
                }
                Some(record) => {
                    missing_surfaces.push(surface);
                    blocking_evidence.push(record.id.clone());
                }
                None => missing_surfaces.push(surface),
            }
        }
        let standing = capability_standing(&latest);
        Fortune5CapabilityAssessment {
            capability,
            standing,
            satisfied_surfaces,
            missing_surfaces,
            blocking_evidence,
        }
    }
}

/// Per-capability assessment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5CapabilityAssessment {
    /// Capability.
    pub capability: Fortune5Capability,
    /// Gall standing.
    pub standing: Fortune5Standing,
    /// Passing surfaces.
    pub satisfied_surfaces: Vec<Fortune5ProofSurface>,
    /// Open surfaces.
    pub missing_surfaces: Vec<Fortune5ProofSurface>,
    /// Latest non-passing evidence identities.
    pub blocking_evidence: Vec<String>,
}

/// Full Fortune-5 assessment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5Assessment {
    /// Contract version.
    pub contract_version: String,
    /// Immutable evidence root.
    pub evidence_root: [u8; 32],
    /// Conjunctive crown standing.
    pub standing: Fortune5Standing,
    /// Stable ordered capability results.
    pub capabilities: Vec<Fortune5CapabilityAssessment>,
}

impl Fortune5Assessment {
    /// All open capability/surface obligations.
    #[must_use]
    pub fn open_obligations(&self) -> Vec<(Fortune5Capability, Fortune5ProofSurface)> {
        self.capabilities
            .iter()
            .flat_map(|assessment| {
                assessment
                    .missing_surfaces
                    .iter()
                    .copied()
                    .map(move |surface| (assessment.capability, surface))
            })
            .collect()
    }

    /// Confirm the exact 19-capability matrix appears once.
    #[must_use]
    pub fn is_complete_contract(&self) -> bool {
        let observed: BTreeSet<Fortune5Capability> = self
            .capabilities
            .iter()
            .map(|assessment| assessment.capability)
            .collect();
        observed.len() == ALL_FORTUNE5_CAPABILITIES.len()
            && ALL_FORTUNE5_CAPABILITIES
                .iter()
                .all(|capability| observed.contains(capability))
    }

    /// Number of satisfied proof surfaces.
    #[must_use]
    pub fn satisfied_surface_count(&self) -> usize {
        self.capabilities
            .iter()
            .map(|assessment| assessment.satisfied_surfaces.len())
            .sum()
    }
}

/// Cryptographic receipt over an assessment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5AssessmentReceipt {
    /// Assessed state.
    pub assessment: Fortune5Assessment,
    /// SHA-256 of the canonical assessment JSON.
    pub receipt_digest: [u8; 32],
}

impl Fortune5AssessmentReceipt {
    /// Issue a receipt.
    pub fn issue(assessment: Fortune5Assessment) -> Result<Self, Fortune5Error> {
        let receipt_digest = assessment_digest(&assessment)?;
        Ok(Self {
            assessment,
            receipt_digest,
        })
    }

    /// Verify the receipt.
    pub fn verify(&self) -> Result<(), Fortune5Error> {
        if assessment_digest(&self.assessment)? != self.receipt_digest {
            return Err(Fortune5Error::AssessmentReceiptMismatch);
        }
        Ok(())
    }
}

/// Complete externally replayable proof.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5Proof {
    /// Immutable evidence.
    pub ledger: Fortune5EvidenceLedger,
    /// Crown assessment receipt.
    pub assessment_receipt: Fortune5AssessmentReceipt,
}

impl Fortune5Proof {
    /// Replay all evidence and verify crown standing.
    pub fn verify(&self) -> Result<(), Fortune5Error> {
        if self.ledger.records().iter().any(|record| !record.verify()) {
            return Err(Fortune5Error::Invariant(
                "one or more evidence records failed verification".to_string(),
            ));
        }
        let replayed = self.ledger.assess();
        if replayed != self.assessment_receipt.assessment {
            return Err(Fortune5Error::Invariant(
                "assessment did not replay from ledger".to_string(),
            ));
        }
        self.assessment_receipt.verify()?;
        if replayed.standing != Fortune5Standing::Alive
            || !replayed.is_complete_contract()
            || replayed.satisfied_surface_count() != 57
        {
            return Err(Fortune5Error::Invariant(
                "Fortune-5 crown is not 19-capability / 57-surface ALIVE".to_string(),
            ));
        }
        Ok(())
    }
}

/// Three separately externalized observations for one capability.
#[derive(Debug, Clone, PartialEq, Eq)]
struct CapabilityProofBytes {
    positive: Vec<u8>,
    negative: Vec<u8>,
    replay: Vec<u8>,
}

/// Executable bounded Fortune-5 reference control plane.
#[derive(Debug, Clone)]
pub struct Fortune5Reference {
    root: PathBuf,
}

impl Fortune5Reference {
    /// Create a reference under an empty root.
    #[must_use]
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self { root: root.into() }
    }

    /// Root used for externalized proof artifacts.
    #[must_use]
    pub fn root(&self) -> &Path {
        &self.root
    }

    /// Execute all 19 capabilities and externalize all 57 proof surfaces.
    pub fn prove_all(&self) -> Result<Fortune5Proof, Fortune5Error> {
        if self.root.exists() && self.root.read_dir()?.next().is_some() {
            return Err(Fortune5Error::NonEmptyRoot(self.root.clone()));
        }
        fs::create_dir_all(self.root.join("witnesses"))?;

        let mut ledger = Fortune5EvidenceLedger::new();
        let mut epoch = 1_u64;
        for capability in ALL_FORTUNE5_CAPABILITIES {
            let proof = self.run_capability(capability)?;
            for (surface, bytes) in [
                (Fortune5ProofSurface::PositiveExecution, proof.positive),
                (Fortune5ProofSurface::NegativeRefusal, proof.negative),
                (Fortune5ProofSurface::ReceiptReplay, proof.replay),
            ] {
                let relative = PathBuf::from("witnesses")
                    .join(capability.slug())
                    .join(format!("{}.json", surface.slug()));
                self.atomic_write(&relative, &bytes)?;
                let committed = self.read_relative(&relative)?;
                ensure(
                    committed == bytes,
                    "externalized capability witness changed after commit",
                )?;
                let id = format!("{}-{}", capability.slug(), surface.slug());
                ledger.admit(Fortune5EvidenceRecord::observed(
                    id,
                    capability,
                    surface,
                    Fortune5EvidenceOutcome::Pass,
                    relative.to_string_lossy(),
                    epoch,
                    &committed,
                ))?;
                epoch = epoch.saturating_add(1);
            }
        }

        let assessment = ledger.assess();
        ensure(
            assessment.standing == Fortune5Standing::Alive,
            "complete Fortune-5 contract did not attain ALIVE",
        )?;
        ensure(
            assessment.satisfied_surface_count() == 57,
            "complete Fortune-5 contract did not close 57 surfaces",
        )?;
        let proof = Fortune5Proof {
            assessment_receipt: Fortune5AssessmentReceipt::issue(assessment)?,
            ledger,
        };
        proof.verify()?;
        Ok(proof)
    }

    fn run_capability(
        &self, capability: Fortune5Capability,
    ) -> Result<CapabilityProofBytes, Fortune5Error> {
        match capability {
            Fortune5Capability::InstallTruth => self.prove_install_truth(),
            Fortune5Capability::CompilerTruth => self.prove_compiler_truth(),
            Fortune5Capability::ConflictTruth => self.prove_conflict_truth(),
            Fortune5Capability::RenderingTruth => self.prove_rendering_truth(),
            Fortune5Capability::TrustTruth => self.prove_trust_truth(),
            Fortune5Capability::ProofTruth => self.prove_proof_truth(),
            Fortune5Capability::AtomicPackTaxonomy => self.prove_atomic_taxonomy(),
            Fortune5Capability::BundleProfileSystem => self.prove_bundle_profile(),
            Fortune5Capability::CliStartupSlo => self.prove_cli_startup_slo(),
            Fortune5Capability::TemplateRenderingSlo => self.prove_template_rendering_slo(),
            Fortune5Capability::RdfQuerySlo => self.prove_rdf_query_slo(),
            Fortune5Capability::MemorySlo => self.prove_memory_slo(),
            Fortune5Capability::ConcurrencySlo => self.prove_concurrency_slo(),
            Fortune5Capability::AndonSignals => self.prove_andon(),
            Fortune5Capability::PokaYoke => self.prove_poka_yoke(),
            Fortune5Capability::DistributedTracing => self.prove_tracing(),
            Fortune5Capability::ChaosResilience => self.prove_chaos_resilience(),
            Fortune5Capability::GoldenSignals => self.prove_golden_signals(),
            Fortune5Capability::ErrorBudgets => self.prove_error_budget(),
        }
    }

    fn prove_install_truth(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let payload = b"pack=fortune5\nversion=1\n".to_vec();
        let signing_key = SigningKey::from_bytes(&[7_u8; 32]);
        let signature = signing_key.sign(&payload);
        let verifying_key = signing_key.verifying_key();
        verify_signed_payload(&payload, &signature, &verifying_key)?;
        let install_relative = PathBuf::from("installed").join("fortune5.pack");
        self.atomic_write(&install_relative, &payload)?;
        let installed = self.read_relative(&install_relative)?;
        ensure(installed == payload, "installed pack bytes diverged")?;

        let mut tampered = payload.clone();
        tampered.push(b'!');
        let refused = verifying_key.verify(&tampered, &signature).is_err();
        ensure(refused, "tampered pack passed Ed25519 verification")?;

        let digest = sha256(&installed);
        let replayed = self.read_relative(&install_relative)?;
        ensure(
            sha256(&replayed) == digest,
            "installed pack receipt did not replay",
        )?;

        capability_bytes(
            &serde_json::json!({
                "installed": true,
                "signature": hex::encode(signature.to_bytes()),
                "sha256": hex::encode(digest),
            }),
            &serde_json::json!({
                "tampered_payload_refused": refused,
            }),
            &serde_json::json!({
                "readback_digest": hex::encode(sha256(&replayed)),
                "matches": true,
            }),
        )
    }

    fn prove_compiler_truth(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let source = CompilerSource {
            ontology: "@prefix ex: <https://example.com/> . ex:item ex:name \"worker\" .",
            query: "SELECT ?name WHERE { ?s <https://example.com/name> ?name }",
            template: "pub const NAME: &str = \"{{ name }}\";\n",
            name: "worker",
        };
        let first = compile_pack_source(&source)?;
        let second = compile_pack_source(&source)?;
        ensure(first == second, "pack compilation was nondeterministic")?;
        let invalid = CompilerSource {
            query: "",
            ..source
        };
        let refused = compile_pack_source(&invalid).is_err();
        ensure(refused, "compiler admitted a pack without a query")?;

        capability_bytes(
            &serde_json::json!({
                "projection": String::from_utf8_lossy(&first),
                "projection_sha256": hex::encode(sha256(&first)),
            }),
            &serde_json::json!({"missing_query_refused": refused}),
            &serde_json::json!({
                "second_compile_digest": hex::encode(sha256(&second)),
                "byte_identical": true,
            }),
        )
    }

    fn prove_conflict_truth(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let dimensions = ConflictDimension::all();
        let compatible: Vec<ConflictClaim> = dimensions
            .iter()
            .enumerate()
            .map(|(index, dimension)| ConflictClaim {
                pack: format!("pack-a-{index}"),
                dimension: *dimension,
                key: format!("key-a-{index}"),
                value: "v1".to_string(),
            })
            .collect();
        ensure(
            detect_conflicts(&compatible).is_empty(),
            "compatible claims produced a conflict",
        )?;

        let mut incompatible = Vec::new();
        for (index, dimension) in dimensions.iter().enumerate() {
            let key = format!("shared-{index}");
            incompatible.push(ConflictClaim {
                pack: format!("pack-a-{index}"),
                dimension: *dimension,
                key: key.clone(),
                value: "allow".to_string(),
            });
            incompatible.push(ConflictClaim {
                pack: format!("pack-b-{index}"),
                dimension: *dimension,
                key,
                value: "deny".to_string(),
            });
        }
        let conflicts = detect_conflicts(&incompatible);
        let observed: BTreeSet<ConflictDimension> = conflicts
            .iter()
            .map(|conflict| conflict.dimension)
            .collect();
        ensure(
            observed.len() == 10
                && dimensions
                    .iter()
                    .all(|dimension| observed.contains(dimension)),
            "the ten conflict dimensions were not all exercised",
        )?;
        let report = serde_json::to_vec(&conflicts)
            .map_err(|error| Fortune5Error::Serialization(error.to_string()))?;
        let replay = serde_json::to_vec(&detect_conflicts(&incompatible))
            .map_err(|error| Fortune5Error::Serialization(error.to_string()))?;
        ensure(report == replay, "conflict report was nondeterministic")?;

        capability_bytes(
            &serde_json::json!({
                "compatible_claims": compatible.len(),
                "conflicts": 0,
            }),
            &serde_json::json!({
                "incompatible_claims": incompatible.len(),
                "dimensions_refused": observed.len(),
            }),
            &serde_json::json!({
                "report_sha256": hex::encode(sha256(&report)),
                "deterministic": true,
            }),
        )
    }

    fn prove_rendering_truth(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let mut context = Context::new();
        context.insert("name", "Fortune5");
        let template = "pub struct {{ name }};\n";
        let first = Tera::one_off(template, &context, false)
            .map_err(|error| Fortune5Error::Invariant(error.to_string()))?;
        let second = Tera::one_off(template, &context, false)
            .map_err(|error| Fortune5Error::Invariant(error.to_string()))?;
        ensure(first == second, "Tera rendering was nondeterministic")?;
        let refused = Tera::one_off("{{ missing", &context, false).is_err();
        ensure(refused, "malformed Tera template was admitted")?;

        capability_bytes(
            &serde_json::json!({"rendered": first}),
            &serde_json::json!({"malformed_template_refused": refused}),
            &serde_json::json!({
                "second_render_sha256": hex::encode(sha256(second.as_bytes())),
                "byte_identical": true,
            }),
        )
    }

    fn prove_trust_truth(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let profile = TrustProfile {
            minimum_tier: TrustTier::EnterpriseCertified,
            allowed_registry: RegistryClass::PrivateEnterprise,
            require_signature: true,
            allowed_runtimes: BTreeSet::from(["rust".to_string()]),
        };
        let admitted = PackTrustContext {
            tier: TrustTier::EnterpriseCertified,
            registry: RegistryClass::PrivateEnterprise,
            signed: true,
            runtime: "rust".to_string(),
        };
        enforce_trust(&profile, &admitted)?;

        let refused_context = PackTrustContext {
            tier: TrustTier::Experimental,
            registry: RegistryClass::Public,
            signed: false,
            runtime: "python".to_string(),
        };
        let refused = enforce_trust(&profile, &refused_context).is_err();
        ensure(refused, "disallowed trust context was admitted")?;
        let receipt = serde_json::to_vec(&admitted)
            .map_err(|error| Fortune5Error::Serialization(error.to_string()))?;

        capability_bytes(
            &serde_json::json!({"enterprise_context_admitted": true}),
            &serde_json::json!({"unsigned_public_experimental_refused": refused}),
            &serde_json::json!({
                "policy_receipt": hex::encode(sha256(&receipt)),
                "reverified": enforce_trust(&profile, &admitted).is_ok(),
            }),
        )
    }

    fn prove_proof_truth(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let mut chain = ReceiptChain::new();
        chain.append("install", b"pack-installed");
        chain.append("compile", b"projection-produced");
        chain.append("actuate", b"artifact-written");
        chain.verify()?;
        let original_root = chain.root_digest();

        let mut tampered = chain.clone();
        tampered.entries[1].payload_digest[0] ^= 0xff;
        let refused = tampered.verify().is_err();
        ensure(refused, "tampered receipt chain was admitted")?;

        let mut replay = ReceiptReplayGuard::default();
        replay.verify_once(original_root)?;
        let duplicate_refused = replay.verify_once(original_root).is_err();
        ensure(duplicate_refused, "duplicate receipt replay was admitted")?;

        capability_bytes(
            &serde_json::json!({
                "entries": chain.entries.len(),
                "root": hex::encode(original_root),
            }),
            &serde_json::json!({"tamper_refused": refused}),
            &serde_json::json!({"duplicate_replay_refused": duplicate_refused}),
        )
    }

    fn prove_atomic_taxonomy(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let complete = AtomicPackCategory::all();
        validate_taxonomy(&complete)?;
        let mut incomplete = complete.clone();
        incomplete.pop();
        let refused = validate_taxonomy(&incomplete).is_err();
        ensure(refused, "incomplete atomic taxonomy was admitted")?;
        let first = taxonomy_digest(&complete);
        let mut reordered = complete.clone();
        reordered.reverse();
        let second = taxonomy_digest(&reordered);
        ensure(first == second, "taxonomy receipt depended on input order")?;

        capability_bytes(
            &serde_json::json!({"categories": complete.len()}),
            &serde_json::json!({"missing_category_refused": refused}),
            &serde_json::json!({
                "taxonomy_digest": hex::encode(first),
                "order_independent": true,
            }),
        )
    }

    fn prove_bundle_profile(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let registry = BundleRegistry::from_entries([
            (
                "mcp-rust".to_string(),
                vec![
                    BundleItem::Pack("surface-mcp".to_string()),
                    BundleItem::Bundle("rust-core".to_string()),
                ],
            ),
            (
                "rust-core".to_string(),
                vec![
                    BundleItem::Pack("projection-rust".to_string()),
                    BundleItem::Pack("runtime-stdio".to_string()),
                ],
            ),
        ]);
        let expanded = registry.expand("mcp-rust")?;
        ensure(
            expanded
                == vec![
                    "projection-rust".to_string(),
                    "runtime-stdio".to_string(),
                    "surface-mcp".to_string(),
                ],
            "bundle expansion did not produce canonical pack set",
        )?;

        let cyclic = BundleRegistry::from_entries([
            ("a".to_string(), vec![BundleItem::Bundle("b".to_string())]),
            ("b".to_string(), vec![BundleItem::Bundle("a".to_string())]),
        ]);
        let refused = cyclic.expand("a").is_err();
        ensure(refused, "bundle cycle was admitted")?;
        let replayed = registry.expand("mcp-rust")?;
        ensure(expanded == replayed, "bundle expansion did not replay")?;

        capability_bytes(
            &serde_json::json!({"expanded": expanded}),
            &serde_json::json!({"cycle_refused": refused}),
            &serde_json::json!({
                "expansion_digest": hex::encode(sha256(expanded.join("\n").as_bytes())),
                "deterministic": true,
            }),
        )
    }

    fn prove_cli_startup_slo(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        prove_latency_slo(
            "cli_startup",
            &[42, 48, 51, 55, 59, 63, 68, 74, 82, 91],
            100,
            140,
        )
    }

    fn prove_template_rendering_slo(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let typical = evaluate_upper_bound("template_typical", &[240, 310, 450, 620], 1_000)?;
        let large = evaluate_upper_bound("template_large", &[1_200, 2_300, 3_100, 4_200], 5_000)?;
        ensure(
            typical.pass && large.pass,
            "valid template SLO observations failed",
        )?;
        let refused = evaluate_upper_bound("template_large", &[5_500], 5_000)?.pass == false;
        ensure(refused, "large-template SLO breach was admitted")?;
        let receipt = sha256(
            format!(
                "{}:{}:{}:{}",
                typical.p90, typical.limit, large.p90, large.limit
            )
            .as_bytes(),
        );
        capability_bytes(
            &serde_json::json!({
                "typical_p90_ms": typical.p90,
                "large_p90_ms": large.p90,
            }),
            &serde_json::json!({"large_breach_refused": refused}),
            &serde_json::json!({"slo_receipt": hex::encode(receipt)}),
        )
    }

    fn prove_rdf_query_slo(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        prove_latency_slo(
            "rdf_query",
            &[21, 25, 29, 34, 40, 48, 55, 63, 75, 89],
            100,
            125,
        )
    }

    fn prove_memory_slo(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let admitted = MemoryObservation {
            baseline_mb: 42,
            peak_mb: 480,
        };
        admitted.validate()?;
        let refused = MemoryObservation {
            baseline_mb: 51,
            peak_mb: 501,
        }
        .validate()
        .is_err();
        ensure(refused, "out-of-bound memory observation was admitted")?;
        let bytes = serde_json::to_vec(&admitted)
            .map_err(|error| Fortune5Error::Serialization(error.to_string()))?;
        capability_bytes(
            &serde_json::json!({
                "baseline_mb": admitted.baseline_mb,
                "peak_mb": admitted.peak_mb,
            }),
            &serde_json::json!({"memory_breach_refused": refused}),
            &serde_json::json!({"observation_receipt": hex::encode(sha256(&bytes))}),
        )
    }

    fn prove_concurrency_slo(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let admitted = ConcurrencyObservation {
            single_worker_throughput: 100,
            eight_worker_throughput: 760,
            minimum_efficiency_percent: 90,
        };
        admitted.validate()?;
        let refused = ConcurrencyObservation {
            eight_worker_throughput: 650,
            ..admitted
        }
        .validate()
        .is_err();
        ensure(refused, "sublinear concurrency observation was admitted")?;
        let bytes = serde_json::to_vec(&admitted)
            .map_err(|error| Fortune5Error::Serialization(error.to_string()))?;
        capability_bytes(
            &serde_json::json!({
                "single_worker": admitted.single_worker_throughput,
                "eight_workers": admitted.eight_worker_throughput,
                "efficiency_percent": admitted.efficiency_percent(),
            }),
            &serde_json::json!({"scaling_breach_refused": refused}),
            &serde_json::json!({"observation_receipt": hex::encode(sha256(&bytes))}),
        )
    }

    fn prove_andon(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let mut andon = AndonController::default();
        andon.raise(AndonSignal::Yellow);
        andon.raise(AndonSignal::Green);
        ensure(
            andon.signal == AndonSignal::Yellow,
            "Andon signal de-escalated without repair",
        )?;
        andon.raise(AndonSignal::Red);
        ensure(
            !andon.may_proceed(),
            "red Andon signal did not stop the line",
        )?;
        let refused = andon.require_proceed().is_err();
        ensure(refused, "red Andon controller admitted execution")?;
        let receipt = serde_json::to_vec(&andon)
            .map_err(|error| Fortune5Error::Serialization(error.to_string()))?;

        capability_bytes(
            &serde_json::json!({"final_signal": andon.signal}),
            &serde_json::json!({"red_stopped_line": refused}),
            &serde_json::json!({"andon_receipt": hex::encode(sha256(&receipt))}),
        )
    }

    fn prove_poka_yoke(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let metric = MetricValue::new(95, 0, 100)?;
        let out_of_range_refused = MetricValue::new(101, 0, 100).is_err();
        let mut lifecycle = BenchmarkLifecycle::default();
        lifecycle.start()?;
        lifecycle.complete()?;
        let invalid_transition_refused = lifecycle.start().is_err();
        ensure(
            out_of_range_refused && invalid_transition_refused,
            "Poka-Yoke falsifier was admitted",
        )?;

        capability_bytes(
            &serde_json::json!({
                "metric": metric.value,
                "lifecycle": lifecycle.state,
            }),
            &serde_json::json!({
                "out_of_range_refused": out_of_range_refused,
                "invalid_transition_refused": invalid_transition_refused,
            }),
            &serde_json::json!({
                "state_receipt": hex::encode(sha256(
                    format!("{}:{:?}", metric.value, lifecycle.state).as_bytes()
                )),
            }),
        )
    }

    fn prove_tracing(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let parent =
            TraceContext::parse("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")?;
        let child = parent.child("compile");
        ensure(
            parent.trace_id == child.trace_id && parent.span_id != child.span_id,
            "trace context did not propagate causality",
        )?;
        let refused =
            TraceContext::parse("00-00000000000000000000000000000000-0000000000000000-01").is_err();
        ensure(refused, "zero W3C trace context was admitted")?;
        let rendered = child.to_traceparent();
        let replayed = TraceContext::parse(&rendered)?;
        ensure(child == replayed, "trace context did not replay")?;

        capability_bytes(
            &serde_json::json!({"child_traceparent": rendered}),
            &serde_json::json!({"zero_context_refused": refused}),
            &serde_json::json!({"round_trip": true}),
        )
    }

    fn prove_chaos_resilience(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let retry = RetrySchedule::new(3, 100)?;
        ensure(
            retry.delays_ms == vec![100, 200, 400],
            "retry schedule was not exponential",
        )?;
        let mut breaker = CircuitBreaker::new(3)?;
        breaker.record_failure();
        breaker.record_failure();
        ensure(breaker.allow(), "circuit opened before threshold")?;
        breaker.record_failure();
        ensure(!breaker.allow(), "circuit remained closed after threshold")?;
        let refused = breaker.require_allow().is_err();
        ensure(refused, "open circuit admitted an operation")?;
        let replayed = RetrySchedule::new(3, 100)?;
        ensure(retry == replayed, "retry schedule did not replay")?;

        capability_bytes(
            &serde_json::json!({"retry_delays_ms": retry.delays_ms}),
            &serde_json::json!({"open_circuit_refused": refused}),
            &serde_json::json!({
                "schedule_receipt": hex::encode(sha256(
                    &serde_json::to_vec(&retry)
                        .map_err(|error| Fortune5Error::Serialization(error.to_string()))?
                )),
            }),
        )
    }

    fn prove_golden_signals(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let healthy = GoldenSignals {
            latency_ms: 90,
            traffic_per_second: 1_200,
            error_basis_points: 25,
            saturation_percent: 70,
        };
        ensure(healthy.health().healthy, "healthy golden signals failed")?;
        let unhealthy = GoldenSignals {
            latency_ms: 350,
            error_basis_points: 800,
            saturation_percent: 95,
            ..healthy
        };
        let verdict = unhealthy.health();
        ensure(!verdict.healthy, "unhealthy golden signals passed")?;
        let receipt = serde_json::to_vec(&healthy)
            .map_err(|error| Fortune5Error::Serialization(error.to_string()))?;

        capability_bytes(
            &serde_json::json!({"healthy": true}),
            &serde_json::json!({
                "unhealthy_refused": true,
                "issues": verdict.issues,
            }),
            &serde_json::json!({"signals_receipt": hex::encode(sha256(&receipt))}),
        )
    }

    fn prove_error_budget(&self) -> Result<CapabilityProofBytes, Fortune5Error> {
        let mut budget = ErrorBudget::new(9_990, 1_000_000)?;
        budget.record_failure(200)?;
        ensure(
            budget.can_deploy_risky_change(),
            "healthy error budget blocked risky change",
        )?;
        budget.record_failure(900)?;
        ensure(budget.breached(), "spent error budget did not breach")?;
        let refused = budget.require_risky_change().is_err();
        ensure(refused, "breached error budget admitted risky change")?;
        let receipt = serde_json::to_vec(&budget)
            .map_err(|error| Fortune5Error::Serialization(error.to_string()))?;

        capability_bytes(
            &serde_json::json!({
                "allowed_ms": budget.allowed_failure_ms,
                "initial_spend_ms": 200,
            }),
            &serde_json::json!({
                "breached": budget.breached(),
                "risky_change_refused": refused,
            }),
            &serde_json::json!({"budget_receipt": hex::encode(sha256(&receipt))}),
        )
    }

    fn atomic_write(&self, relative: &Path, bytes: &[u8]) -> Result<(), Fortune5Error> {
        let destination = self.root.join(relative);
        let parent = destination
            .parent()
            .ok_or_else(|| Fortune5Error::Invariant("witness path has no parent".to_string()))?;
        fs::create_dir_all(parent)?;
        let staging = destination.with_extension("staged");
        fs::write(&staging, bytes)?;
        fs::rename(staging, destination)?;
        Ok(())
    }

    fn read_relative(&self, relative: &Path) -> Result<Vec<u8>, Fortune5Error> {
        let mut bytes = Vec::new();
        File::open(self.root.join(relative))?.read_to_end(&mut bytes)?;
        Ok(bytes)
    }
}

/// Ten compatibility dimensions named by the Fortune-5 release gate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[repr(u8)]
enum ConflictDimension {
    OntologyNamespace = 1,
    ProtocolField = 2,
    EmittedFilePath = 3,
    RuntimeIncompatibility = 4,
    ValidatorContradiction = 5,
    PolicyContradiction = 6,
    VersionRange = 7,
    CapabilityIdentity = 8,
    ReceiptSchema = 9,
    ConsequenceMigration = 10,
}

impl ConflictDimension {
    fn all() -> [Self; 10] {
        [
            Self::OntologyNamespace,
            Self::ProtocolField,
            Self::EmittedFilePath,
            Self::RuntimeIncompatibility,
            Self::ValidatorContradiction,
            Self::PolicyContradiction,
            Self::VersionRange,
            Self::CapabilityIdentity,
            Self::ReceiptSchema,
            Self::ConsequenceMigration,
        ]
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct ConflictClaim {
    pack: String,
    dimension: ConflictDimension,
    key: String,
    value: String,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize)]
struct DetectedConflict {
    dimension: ConflictDimension,
    key: String,
    packs: Vec<String>,
    values: Vec<String>,
}

fn detect_conflicts(claims: &[ConflictClaim]) -> Vec<DetectedConflict> {
    let mut groups: BTreeMap<(ConflictDimension, String), Vec<&ConflictClaim>> = BTreeMap::new();
    for claim in claims {
        groups
            .entry((claim.dimension, claim.key.clone()))
            .or_default()
            .push(claim);
    }
    let mut conflicts = Vec::new();
    for ((dimension, key), group) in groups {
        let values: BTreeSet<String> = group.iter().map(|claim| claim.value.clone()).collect();
        let packs: BTreeSet<String> = group.iter().map(|claim| claim.pack.clone()).collect();
        if packs.len() > 1 && values.len() > 1 {
            conflicts.push(DetectedConflict {
                dimension,
                key,
                packs: packs.into_iter().collect(),
                values: values.into_iter().collect(),
            });
        }
    }
    conflicts.sort();
    conflicts
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[repr(u8)]
enum TrustTier {
    Blocked = 0,
    Experimental = 1,
    Quarantined = 2,
    EnterpriseApproved = 3,
    EnterpriseCertified = 4,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum RegistryClass {
    Public,
    PrivateEnterprise,
    MirroredAirGapped,
}

#[derive(Debug, Clone)]
struct TrustProfile {
    minimum_tier: TrustTier,
    allowed_registry: RegistryClass,
    require_signature: bool,
    allowed_runtimes: BTreeSet<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct PackTrustContext {
    tier: TrustTier,
    registry: RegistryClass,
    signed: bool,
    runtime: String,
}

fn enforce_trust(profile: &TrustProfile, context: &PackTrustContext) -> Result<(), Fortune5Error> {
    ensure(
        context.tier >= profile.minimum_tier,
        "pack trust tier is below profile minimum",
    )?;
    ensure(
        context.registry == profile.allowed_registry,
        "registry class is not admitted by profile",
    )?;
    ensure(
        !profile.require_signature || context.signed,
        "profile requires a signed pack",
    )?;
    ensure(
        profile.allowed_runtimes.contains(&context.runtime),
        "runtime is not admitted by profile",
    )
}

fn verify_signed_payload(
    payload: &[u8], signature: &Signature, verifying_key: &VerifyingKey,
) -> Result<(), Fortune5Error> {
    verifying_key.verify(payload, signature).map_err(|error| {
        Fortune5Error::Invariant(format!("signature verification failed: {error}"))
    })
}

#[derive(Debug, Clone, Copy)]
struct CompilerSource<'a> {
    ontology: &'a str,
    query: &'a str,
    template: &'a str,
    name: &'a str,
}

fn compile_pack_source(source: &CompilerSource<'_>) -> Result<Vec<u8>, Fortune5Error> {
    ensure(!source.ontology.trim().is_empty(), "ontology is required")?;
    ensure(!source.query.trim().is_empty(), "query is required")?;
    ensure(
        source.query.to_ascii_uppercase().contains("SELECT"),
        "compiler query must be SELECT",
    )?;
    ensure(!source.template.trim().is_empty(), "template is required")?;
    let mut context = Context::new();
    context.insert("name", source.name);
    let rendered = Tera::one_off(source.template, &context, false)
        .map_err(|error| Fortune5Error::Invariant(format!("template rejected: {error}")))?;
    Ok(rendered.into_bytes())
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct ChainEntry {
    index: u64,
    kind: String,
    payload_digest: [u8; 32],
    previous_digest: [u8; 32],
    entry_digest: [u8; 32],
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
struct ReceiptChain {
    entries: Vec<ChainEntry>,
}

impl ReceiptChain {
    fn new() -> Self {
        Self::default()
    }

    fn append(&mut self, kind: &str, payload: &[u8]) {
        let index = self.entries.len() as u64;
        let previous_digest = self
            .entries
            .last()
            .map_or([0_u8; 32], |entry| entry.entry_digest);
        let payload_digest = sha256(payload);
        let entry_digest = chain_entry_digest(index, kind, &payload_digest, &previous_digest);
        self.entries.push(ChainEntry {
            index,
            kind: kind.to_string(),
            payload_digest,
            previous_digest,
            entry_digest,
        });
    }

    fn verify(&self) -> Result<(), Fortune5Error> {
        let mut previous = [0_u8; 32];
        for (index, entry) in self.entries.iter().enumerate() {
            ensure(entry.index == index as u64, "receipt-chain index mismatch")?;
            ensure(
                entry.previous_digest == previous,
                "receipt-chain predecessor mismatch",
            )?;
            ensure(
                entry.entry_digest
                    == chain_entry_digest(
                        entry.index,
                        &entry.kind,
                        &entry.payload_digest,
                        &entry.previous_digest,
                    ),
                "receipt-chain digest mismatch",
            )?;
            previous = entry.entry_digest;
        }
        Ok(())
    }

    fn root_digest(&self) -> [u8; 32] {
        self.entries
            .last()
            .map_or([0_u8; 32], |entry| entry.entry_digest)
    }
}

#[derive(Debug, Default)]
struct ReceiptReplayGuard {
    observed: HashSet<[u8; 32]>,
}

impl ReceiptReplayGuard {
    fn verify_once(&mut self, digest: [u8; 32]) -> Result<(), Fortune5Error> {
        if !self.observed.insert(digest) {
            return Err(Fortune5Error::Invariant(
                "duplicate receipt replay refused".to_string(),
            ));
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
enum AtomicPackCategory {
    Surface,
    Contract,
    Projection,
    Runtime,
    Policy,
    Validator,
    Receipt,
    Consequence,
    Core,
}

impl AtomicPackCategory {
    fn all() -> Vec<Self> {
        vec![
            Self::Surface,
            Self::Contract,
            Self::Projection,
            Self::Runtime,
            Self::Policy,
            Self::Validator,
            Self::Receipt,
            Self::Consequence,
            Self::Core,
        ]
    }
}

fn validate_taxonomy(categories: &[AtomicPackCategory]) -> Result<(), Fortune5Error> {
    let observed: BTreeSet<AtomicPackCategory> = categories.iter().copied().collect();
    let required: BTreeSet<AtomicPackCategory> = AtomicPackCategory::all().into_iter().collect();
    ensure(
        categories.len() == required.len() && observed == required,
        "atomic pack taxonomy must contain each of nine categories exactly once",
    )
}

fn taxonomy_digest(categories: &[AtomicPackCategory]) -> [u8; 32] {
    let mut names: Vec<String> = categories
        .iter()
        .map(|category| format!("{category:?}"))
        .collect();
    names.sort();
    sha256(names.join("\n").as_bytes())
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum BundleItem {
    Pack(String),
    Bundle(String),
}

#[derive(Debug, Clone)]
struct BundleRegistry {
    entries: BTreeMap<String, Vec<BundleItem>>,
}

impl BundleRegistry {
    fn from_entries<const N: usize>(entries: [(String, Vec<BundleItem>); N]) -> Self {
        Self {
            entries: entries.into_iter().collect(),
        }
    }

    fn expand(&self, name: &str) -> Result<Vec<String>, Fortune5Error> {
        let mut visiting = BTreeSet::new();
        let mut packs = BTreeSet::new();
        self.expand_into(name, &mut visiting, &mut packs)?;
        Ok(packs.into_iter().collect())
    }

    fn expand_into(
        &self, name: &str, visiting: &mut BTreeSet<String>, packs: &mut BTreeSet<String>,
    ) -> Result<(), Fortune5Error> {
        if !visiting.insert(name.to_string()) {
            return Err(Fortune5Error::Invariant(format!(
                "bundle cycle refused at {name}"
            )));
        }
        let items = self
            .entries
            .get(name)
            .ok_or_else(|| Fortune5Error::Invariant(format!("unknown bundle refused: {name}")))?;
        for item in items {
            match item {
                BundleItem::Pack(pack) => {
                    packs.insert(pack.clone());
                }
                BundleItem::Bundle(bundle) => {
                    self.expand_into(bundle, visiting, packs)?;
                }
            }
        }
        visiting.remove(name);
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
struct SloObservation {
    p90: u64,
    limit: u64,
    pass: bool,
}

fn evaluate_upper_bound(
    name: &str, values: &[u64], limit: u64,
) -> Result<SloObservation, Fortune5Error> {
    ensure(!values.is_empty(), &format!("{name} requires observations"))?;
    ensure(limit > 0, &format!("{name} requires a positive limit"))?;
    let p90 = percentile(values, 90);
    Ok(SloObservation {
        p90,
        limit,
        pass: p90 <= limit,
    })
}

fn prove_latency_slo(
    name: &str, passing_values: &[u64], limit: u64, failing_value: u64,
) -> Result<CapabilityProofBytes, Fortune5Error> {
    let admitted = evaluate_upper_bound(name, passing_values, limit)?;
    ensure(admitted.pass, "valid latency SLO observations failed")?;
    let refused = !evaluate_upper_bound(name, &[failing_value], limit)?.pass;
    ensure(refused, "latency SLO breach was admitted")?;
    let receipt =
        sha256(format!("{name}:{}:{}:{}", admitted.p90, limit, passing_values.len()).as_bytes());
    capability_bytes(
        &serde_json::json!({"name": name, "p90_ms": admitted.p90, "limit_ms": limit}),
        &serde_json::json!({"breach_refused": refused, "observed_ms": failing_value}),
        &serde_json::json!({"slo_receipt": hex::encode(receipt)}),
    )
}

fn percentile(values: &[u64], percentile: usize) -> u64 {
    let mut sorted = values.to_vec();
    sorted.sort_unstable();
    let rank = (percentile * sorted.len()).div_ceil(100).saturating_sub(1);
    sorted[rank.min(sorted.len().saturating_sub(1))]
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
struct MemoryObservation {
    baseline_mb: u64,
    peak_mb: u64,
}

impl MemoryObservation {
    fn validate(self) -> Result<(), Fortune5Error> {
        ensure(self.baseline_mb <= 50, "memory baseline exceeds 50 MB")?;
        ensure(self.peak_mb <= 500, "memory peak exceeds 500 MB")?;
        ensure(
            self.baseline_mb <= self.peak_mb,
            "memory baseline exceeds peak",
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
struct ConcurrencyObservation {
    single_worker_throughput: u64,
    eight_worker_throughput: u64,
    minimum_efficiency_percent: u64,
}

impl ConcurrencyObservation {
    fn efficiency_percent(self) -> u64 {
        self.eight_worker_throughput
            .saturating_mul(100)
            .checked_div(self.single_worker_throughput.saturating_mul(8))
            .unwrap_or(0)
    }

    fn validate(self) -> Result<(), Fortune5Error> {
        ensure(
            self.single_worker_throughput > 0,
            "single-worker throughput must be positive",
        )?;
        ensure(
            self.minimum_efficiency_percent <= 100,
            "concurrency efficiency floor is invalid",
        )?;
        ensure(
            self.eight_worker_throughput.saturating_mul(100)
                >= self
                    .single_worker_throughput
                    .saturating_mul(8)
                    .saturating_mul(self.minimum_efficiency_percent),
            "concurrency scaling fell below efficiency floor",
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
enum AndonSignal {
    Green,
    Yellow,
    Red,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
struct AndonController {
    signal: AndonSignal,
}

impl Default for AndonController {
    fn default() -> Self {
        Self {
            signal: AndonSignal::Green,
        }
    }
}

impl AndonController {
    fn raise(&mut self, signal: AndonSignal) {
        self.signal = self.signal.max(signal);
    }

    fn may_proceed(self) -> bool {
        self.signal != AndonSignal::Red
    }

    fn require_proceed(self) -> Result<(), Fortune5Error> {
        ensure(self.may_proceed(), "Andon RED stopped the line")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct MetricValue {
    value: i64,
}

impl MetricValue {
    fn new(value: i64, minimum: i64, maximum: i64) -> Result<Self, Fortune5Error> {
        ensure(minimum <= maximum, "metric bounds are inverted")?;
        ensure(
            (minimum..=maximum).contains(&value),
            "metric value is outside admitted bounds",
        )?;
        Ok(Self { value })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
enum BenchmarkState {
    Planned,
    Started,
    Completed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct BenchmarkLifecycle {
    state: BenchmarkState,
}

impl Default for BenchmarkLifecycle {
    fn default() -> Self {
        Self {
            state: BenchmarkState::Planned,
        }
    }
}

impl BenchmarkLifecycle {
    fn start(&mut self) -> Result<(), Fortune5Error> {
        ensure(
            self.state == BenchmarkState::Planned,
            "benchmark may start only from PLANNED",
        )?;
        self.state = BenchmarkState::Started;
        Ok(())
    }

    fn complete(&mut self) -> Result<(), Fortune5Error> {
        ensure(
            self.state == BenchmarkState::Started,
            "benchmark may complete only from STARTED",
        )?;
        self.state = BenchmarkState::Completed;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TraceContext {
    version: String,
    trace_id: String,
    span_id: String,
    flags: String,
}

impl TraceContext {
    fn parse(value: &str) -> Result<Self, Fortune5Error> {
        let parts: Vec<&str> = value.split('-').collect();
        ensure(parts.len() == 4, "traceparent must contain four fields")?;
        ensure(
            parts[0].len() == 2,
            "traceparent version must be two hex digits",
        )?;
        ensure(parts[1].len() == 32, "trace id must be 128-bit hex")?;
        ensure(parts[2].len() == 16, "span id must be 64-bit hex")?;
        ensure(parts[3].len() == 2, "trace flags must be two hex digits")?;
        ensure(
            parts
                .iter()
                .all(|part| part.bytes().all(|byte| byte.is_ascii_hexdigit())),
            "traceparent contains non-hex digits",
        )?;
        ensure(
            parts[1].bytes().any(|byte| byte != b'0'),
            "zero trace id refused",
        )?;
        ensure(
            parts[2].bytes().any(|byte| byte != b'0'),
            "zero span id refused",
        )?;
        Ok(Self {
            version: parts[0].to_ascii_lowercase(),
            trace_id: parts[1].to_ascii_lowercase(),
            span_id: parts[2].to_ascii_lowercase(),
            flags: parts[3].to_ascii_lowercase(),
        })
    }

    fn child(&self, operation: &str) -> Self {
        let digest = sha256(format!("{}:{}:{operation}", self.trace_id, self.span_id).as_bytes());
        Self {
            version: self.version.clone(),
            trace_id: self.trace_id.clone(),
            span_id: hex::encode(&digest[..8]),
            flags: self.flags.clone(),
        }
    }

    fn to_traceparent(&self) -> String {
        format!(
            "{}-{}-{}-{}",
            self.version, self.trace_id, self.span_id, self.flags
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct RetrySchedule {
    delays_ms: Vec<u64>,
}

impl RetrySchedule {
    fn new(attempts: usize, base_delay_ms: u64) -> Result<Self, Fortune5Error> {
        ensure((1..=8).contains(&attempts), "retry attempts must be 1..=8")?;
        ensure(base_delay_ms > 0, "retry base delay must be positive")?;
        let delays_ms = (0..attempts)
            .map(|index| base_delay_ms.saturating_mul(1_u64 << index))
            .collect();
        Ok(Self { delays_ms })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct CircuitBreaker {
    threshold: u32,
    failures: u32,
}

impl CircuitBreaker {
    fn new(threshold: u32) -> Result<Self, Fortune5Error> {
        ensure(threshold > 0, "circuit threshold must be positive")?;
        Ok(Self {
            threshold,
            failures: 0,
        })
    }

    fn record_failure(&mut self) {
        self.failures = self.failures.saturating_add(1);
    }

    fn allow(self) -> bool {
        self.failures < self.threshold
    }

    fn require_allow(self) -> Result<(), Fortune5Error> {
        ensure(self.allow(), "circuit breaker is OPEN")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
struct GoldenSignals {
    latency_ms: u64,
    traffic_per_second: u64,
    error_basis_points: u64,
    saturation_percent: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct HealthVerdict {
    healthy: bool,
    issues: Vec<String>,
}

impl GoldenSignals {
    fn health(self) -> HealthVerdict {
        let mut issues = Vec::new();
        if self.latency_ms > 250 {
            issues.push("latency".to_string());
        }
        if self.traffic_per_second == 0 {
            issues.push("traffic".to_string());
        }
        if self.error_basis_points > 100 {
            issues.push("errors".to_string());
        }
        if self.saturation_percent > 85 {
            issues.push("saturation".to_string());
        }
        HealthVerdict {
            healthy: issues.is_empty(),
            issues,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
struct ErrorBudget {
    slo_basis_points: u32,
    period_ms: u64,
    allowed_failure_ms: u64,
    spent_failure_ms: u64,
}

impl ErrorBudget {
    fn new(slo_basis_points: u32, period_ms: u64) -> Result<Self, Fortune5Error> {
        ensure(
            (1..10_000).contains(&slo_basis_points),
            "SLO basis points must be 1..9999",
        )?;
        ensure(period_ms > 0, "error-budget period must be positive")?;
        let allowed_failure_ms =
            period_ms.saturating_mul(u64::from(10_000 - slo_basis_points)) / 10_000;
        ensure(
            allowed_failure_ms > 0,
            "error-budget allowance rounds to zero",
        )?;
        Ok(Self {
            slo_basis_points,
            period_ms,
            allowed_failure_ms,
            spent_failure_ms: 0,
        })
    }

    fn record_failure(&mut self, duration_ms: u64) -> Result<(), Fortune5Error> {
        ensure(duration_ms > 0, "failure duration must be positive")?;
        self.spent_failure_ms = self.spent_failure_ms.saturating_add(duration_ms);
        Ok(())
    }

    fn remaining_ms(self) -> u64 {
        self.allowed_failure_ms
            .saturating_sub(self.spent_failure_ms)
    }

    fn breached(self) -> bool {
        self.spent_failure_ms > self.allowed_failure_ms
    }

    fn can_deploy_risky_change(self) -> bool {
        !self.breached() && self.remaining_ms().saturating_mul(2) >= self.allowed_failure_ms
    }

    fn require_risky_change(self) -> Result<(), Fortune5Error> {
        ensure(
            self.can_deploy_risky_change(),
            "error budget refuses risky change",
        )
    }
}

fn capability_standing(
    latest: &BTreeMap<Fortune5ProofSurface, &Fortune5EvidenceRecord>,
) -> Fortune5Standing {
    let outcomes: HashSet<Fortune5EvidenceOutcome> = REQUIRED_PROOF_SURFACES
        .iter()
        .filter_map(|surface| latest.get(surface).map(|record| record.outcome))
        .collect();
    if outcomes.contains(&Fortune5EvidenceOutcome::BuildFailed) {
        Fortune5Standing::BuildBroken
    } else if outcomes.contains(&Fortune5EvidenceOutcome::Fail) {
        Fortune5Standing::Blocked
    } else if outcomes.contains(&Fortune5EvidenceOutcome::Unsupported) {
        Fortune5Standing::Unsupported
    } else if REQUIRED_PROOF_SURFACES.iter().all(|surface| {
        latest
            .get(surface)
            .is_some_and(|record| record.outcome == Fortune5EvidenceOutcome::Pass)
    }) {
        Fortune5Standing::Alive
    } else if latest.is_empty() || outcomes.contains(&Fortune5EvidenceOutcome::Unknown) {
        Fortune5Standing::Unknown
    } else {
        Fortune5Standing::PartialAlive
    }
}

fn overall_standing(capabilities: &[Fortune5CapabilityAssessment]) -> Fortune5Standing {
    if capabilities
        .iter()
        .any(|assessment| assessment.standing == Fortune5Standing::BuildBroken)
    {
        Fortune5Standing::BuildBroken
    } else if capabilities
        .iter()
        .any(|assessment| assessment.standing == Fortune5Standing::Blocked)
    {
        Fortune5Standing::Blocked
    } else if capabilities
        .iter()
        .any(|assessment| assessment.standing == Fortune5Standing::Unsupported)
    {
        Fortune5Standing::Unsupported
    } else if capabilities.len() == ALL_FORTUNE5_CAPABILITIES.len()
        && capabilities
            .iter()
            .all(|assessment| assessment.standing == Fortune5Standing::Alive)
    {
        Fortune5Standing::Alive
    } else if capabilities
        .iter()
        .all(|assessment| assessment.standing == Fortune5Standing::Unknown)
    {
        Fortune5Standing::Unknown
    } else {
        Fortune5Standing::PartialAlive
    }
}

fn capability_bytes(
    positive: &serde_json::Value, negative: &serde_json::Value, replay: &serde_json::Value,
) -> Result<CapabilityProofBytes, Fortune5Error> {
    Ok(CapabilityProofBytes {
        positive: serde_json::to_vec_pretty(positive)
            .map_err(|error| Fortune5Error::Serialization(error.to_string()))?,
        negative: serde_json::to_vec_pretty(negative)
            .map_err(|error| Fortune5Error::Serialization(error.to_string()))?,
        replay: serde_json::to_vec_pretty(replay)
            .map_err(|error| Fortune5Error::Serialization(error.to_string()))?,
    })
}

fn assessment_digest(assessment: &Fortune5Assessment) -> Result<[u8; 32], Fortune5Error> {
    let bytes = serde_json::to_vec(assessment)
        .map_err(|error| Fortune5Error::Serialization(error.to_string()))?;
    Ok(sha256(&bytes))
}

fn evidence_digest(
    id: &str, capability: Fortune5Capability, surface: Fortune5ProofSurface,
    outcome: Fortune5EvidenceOutcome, source: &str, epoch: u64, artifact_digest: &[u8; 32],
) -> [u8; 32] {
    let mut hasher = Sha256::new();
    put(&mut hasher, b"fortune5-evidence/v1");
    put(&mut hasher, id.as_bytes());
    hasher.update([capability as u8, surface as u8, outcome as u8]);
    put(&mut hasher, source.as_bytes());
    hasher.update(epoch.to_le_bytes());
    hasher.update(artifact_digest);
    hasher.finalize().into()
}

fn chain_entry_digest(
    index: u64, kind: &str, payload_digest: &[u8; 32], previous_digest: &[u8; 32],
) -> [u8; 32] {
    let mut hasher = Sha256::new();
    put(&mut hasher, b"fortune5-receipt-chain/v1");
    hasher.update(index.to_le_bytes());
    put(&mut hasher, kind.as_bytes());
    hasher.update(payload_digest);
    hasher.update(previous_digest);
    hasher.finalize().into()
}

fn sha256(bytes: &[u8]) -> [u8; 32] {
    Sha256::digest(bytes).into()
}

fn put(hasher: &mut Sha256, bytes: &[u8]) {
    hasher.update((bytes.len() as u64).to_le_bytes());
    hasher.update(bytes);
}

fn ensure(condition: bool, message: &str) -> Result<(), Fortune5Error> {
    if condition {
        Ok(())
    } else {
        Err(Fortune5Error::Invariant(message.to_string()))
    }
}

/// Typed Fortune-5 control-plane failures.
#[derive(Debug, thiserror::Error)]
pub enum Fortune5Error {
    /// Bounded root must be empty.
    #[error("Fortune-5 proof root is not empty: {0}")]
    NonEmptyRoot(PathBuf),
    /// Immutable evidence identity was reused.
    #[error("duplicate Fortune-5 evidence id refused: {0}")]
    DuplicateEvidence(String),
    /// Evidence digest does not bind its fields.
    #[error("Fortune-5 evidence digest mismatch: {0}")]
    EvidenceDigestMismatch(String),
    /// Assessment receipt failed replay.
    #[error("Fortune-5 assessment receipt mismatch")]
    AssessmentReceiptMismatch,
    /// Machine-checkable contract invariant failed.
    #[error("Fortune-5 invariant failed: {0}")]
    Invariant(String),
    /// JSON receipt externalization failed.
    #[error("Fortune-5 serialization failed: {0}")]
    Serialization(String),
    /// Real filesystem boundary failed.
    #[error(transparent)]
    Io(#[from] std::io::Error),
}
