# `crates/ggen-marketplace/src/marketplace/fortune5.rs`

Source SHA-256: `f4fc55c6d29324b1ed5bd64aa6ef740d0e432ab131fe779953f21db88748b4a1`

```mermaid
classDiagram
    class enum_Fortune5Category {
      <<enum>>
    }
    class enum_Fortune5Capability {
      <<enum>>
    }
    class enum_Fortune5ProofSurface {
      <<enum>>
    }
    class struct_Fortune5CapabilityContract {
      <<struct>>
      +"capability: Fortune5Capability"
      +"category: Fortune5Category"
      +"outcome: &'static str"
      +"required_surfaces: &'static [Fortune5ProofSurface]"
    }
    class fn_all_fortune5_contracts {
      <<fn>>
    }
    class enum_Fortune5EvidenceOutcome {
      <<enum>>
    }
    class enum_Fortune5Standing {
      <<enum>>
    }
    class struct_Fortune5EvidenceRecord {
      <<struct>>
      +"id: String"
      +"capability: Fortune5Capability"
      +"surface: Fortune5ProofSurface"
      +"outcome: Fortune5EvidenceOutcome"
      +"source: String"
      +"epoch: u64"
      +"artifact_digest: [u8; 32]"
      +"observation_digest: [u8; 32]"
    }
    class struct_Fortune5EvidenceLedger {
      <<struct>>
      +"records: Vec~Fortune5EvidenceRecord~"
    }
    class struct_Fortune5CapabilityAssessment {
      <<struct>>
      +"capability: Fortune5Capability"
      +"standing: Fortune5Standing"
      +"satisfied_surfaces: Vec~Fortune5ProofSurface~"
      +"missing_surfaces: Vec~Fortune5ProofSurface~"
      +"blocking_evidence: Vec~String~"
    }
    class struct_Fortune5Assessment {
      <<struct>>
      +"contract_version: String"
      +"evidence_root: [u8; 32]"
      +"standing: Fortune5Standing"
      +"capabilities: Vec~Fortune5CapabilityAssessment~"
    }
    class struct_Fortune5AssessmentReceipt {
      <<struct>>
      +"assessment: Fortune5Assessment"
      +"receipt_digest: [u8; 32]"
    }
    class struct_Fortune5Proof {
      <<struct>>
      +"ledger: Fortune5EvidenceLedger"
      +"assessment_receipt: Fortune5AssessmentReceipt"
    }
    class struct_CapabilityProofBytes {
      <<struct>>
      +"positive: Vec~u8~"
      +"negative: Vec~u8~"
      +"replay: Vec~u8~"
    }
    class struct_Fortune5Reference {
      <<struct>>
      +"root: PathBuf"
    }
    class enum_ConflictDimension {
      <<enum>>
    }
    class struct_ConflictClaim {
      <<struct>>
      +"pack: String"
      +"dimension: ConflictDimension"
      +"key: String"
      +"value: String"
    }
    class struct_DetectedConflict {
      <<struct>>
      +"dimension: ConflictDimension"
      +"key: String"
      +"packs: Vec~String~"
      +"values: Vec~String~"
    }
    class fn_detect_conflicts {
      <<fn>>
    }
    class enum_TrustTier {
      <<enum>>
    }
    class enum_RegistryClass {
      <<enum>>
    }
    class struct_TrustProfile {
      <<struct>>
      +"minimum_tier: TrustTier"
      +"allowed_registry: RegistryClass"
      +"require_signature: bool"
      +"allowed_runtimes: BTreeSet~String~"
    }
    class struct_PackTrustContext {
      <<struct>>
      +"tier: TrustTier"
      +"registry: RegistryClass"
      +"signed: bool"
      +"runtime: String"
    }
    class fn_enforce_trust {
      <<fn>>
    }
    class fn_verify_signed_payload {
      <<fn>>
    }
    class struct_CompilerSource {
      <<struct>>
      +"ontology: &'a str"
      +"query: &'a str"
      +"template: &'a str"
      +"name: &'a str"
    }
    class fn_compile_pack_source {
      <<fn>>
    }
    class struct_ChainEntry {
      <<struct>>
      +"index: u64"
      +"kind: String"
      +"payload_digest: [u8; 32]"
      +"previous_digest: [u8; 32]"
      +"entry_digest: [u8; 32]"
    }
    class struct_ReceiptChain {
      <<struct>>
      +"entries: Vec~ChainEntry~"
    }
    class struct_ReceiptReplayGuard {
      <<struct>>
      +"observed: HashSet~[u8; 32]~"
    }
    class enum_AtomicPackCategory {
      <<enum>>
    }
    class fn_validate_taxonomy {
      <<fn>>
    }
    class fn_taxonomy_digest {
      <<fn>>
    }
    class enum_BundleItem {
      <<enum>>
    }
    class struct_BundleRegistry {
      <<struct>>
      +"entries: BTreeMap~String"
    }
    class struct_SloObservation {
      <<struct>>
      +"p90: u64"
      +"limit: u64"
      +"pass: bool"
    }
    class fn_evaluate_upper_bound {
      <<fn>>
    }
    class fn_prove_latency_slo {
      <<fn>>
    }
    class fn_percentile {
      <<fn>>
    }
    class struct_MemoryObservation {
      <<struct>>
      +"baseline_mb: u64"
      +"peak_mb: u64"
    }
    class struct_ConcurrencyObservation {
      <<struct>>
      +"single_worker_throughput: u64"
      +"eight_worker_throughput: u64"
      +"minimum_efficiency_percent: u64"
    }
    class enum_AndonSignal {
      <<enum>>
    }
    class struct_AndonController {
      <<struct>>
      +"signal: AndonSignal"
    }
    class struct_MetricValue {
      <<struct>>
      +"value: i64"
    }
    class enum_BenchmarkState {
      <<enum>>
    }
    class struct_BenchmarkLifecycle {
      <<struct>>
      +"state: BenchmarkState"
    }
    class struct_TraceContext {
      <<struct>>
      +"version: String"
      +"trace_id: String"
      +"span_id: String"
      +"flags: String"
    }
    class struct_RetrySchedule {
      <<struct>>
      +"delays_ms: Vec~u64~"
    }
    class struct_CircuitBreaker {
      <<struct>>
      +"threshold: u32"
      +"failures: u32"
    }
    class struct_GoldenSignals {
      <<struct>>
      +"latency_ms: u64"
      +"traffic_per_second: u64"
      +"error_basis_points: u64"
      +"saturation_percent: u64"
    }
    class struct_HealthVerdict {
      <<struct>>
      +"healthy: bool"
      +"issues: Vec~String~"
    }
    class struct_ErrorBudget {
      <<struct>>
      +"slo_basis_points: u32"
      +"period_ms: u64"
      +"allowed_failure_ms: u64"
      +"spent_failure_ms: u64"
    }
    class fn_capability_standing {
      <<fn>>
    }
    class fn_overall_standing {
      <<fn>>
    }
    class fn_capability_bytes {
      <<fn>>
    }
    class fn_assessment_digest {
      <<fn>>
    }
    class fn_evidence_digest {
      <<fn>>
    }
    class fn_chain_entry_digest {
      <<fn>>
    }
    class fn_sha256 {
      <<fn>>
    }
    class fn_put {
      <<fn>>
    }
    class fn_ensure {
      <<fn>>
    }
    class enum_Fortune5Error {
      <<enum>>
    }
    note "AndonController"
    note "AtomicPackCategory"
    note "BenchmarkLifecycle"
    note "BundleRegistry"
    note "CircuitBreaker"
    note "ConcurrencyObservation"
    note "ConflictDimension"
    note "Default for AndonController"
    note "Default for BenchmarkLifecycle"
    note "ErrorBudget"
    note "Fortune5Assessment"
    note "Fortune5AssessmentReceipt"
    note "Fortune5Capability"
    note "Fortune5EvidenceLedger"
    note "Fortune5EvidenceRecord"
    note "Fortune5Proof"
    note "Fortune5ProofSurface"
    note "Fortune5Reference"
    note "GoldenSignals"
    note "MemoryObservation"
    note "MetricValue"
    note "ReceiptChain"
    note "ReceiptReplayGuard"
    note "RetrySchedule"
    note "TraceContext"
```

## Dependencies

- `ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey}`
- `serde::{Deserialize, Serialize}`
- `sha2::{Digest, Sha256}`
- `std::collections::{BTreeMap, BTreeSet, HashSet}`
- `std::fs::{self, File}`
- `std::io::Read`
- `std::path::{Path, PathBuf}`
- `tera::{Context, Tera}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
