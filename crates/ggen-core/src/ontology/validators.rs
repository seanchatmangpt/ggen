/// Multi-layer Validators: Static, Dynamic, Performance
///
/// Ensures all ΔΣ proposals preserve hard invariants (Q) through:
/// - Static validation (SHACL, OWL constraints, Σ² rules)
/// - Dynamic validation (shadow projections, test execution)
/// - Performance validation (operator latency SLOs, memory bounds)
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::Instant;
use tokio::task::JoinHandle;

use crate::ontology::delta_proposer::DeltaSigmaProposal;
use crate::ontology::sigma_runtime::{PerformanceMetrics, SigmaReceipt, SigmaSnapshot, TestResult};

// Local ValidationResult type - different from sigma_runtime's ValidationResult
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    pub passed: bool,
    pub evidence: Vec<ValidationEvidence>,
}

/// Validation evidence for each layer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationEvidence {
    pub validator_name: String,
    pub passed: bool,
    pub checks_performed: usize,
    pub checks_passed: usize,
    pub duration_ms: u64,
    pub details: String,
}

/// Hard invariants (Q): The immutable constitution
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum Invariant {
    /// No retrocausation: past snapshots unaffected
    NoRetrocausation,

    /// Type soundness: all values conform to declared ranges
    TypeSoundness,

    /// Guard soundness: guards are satisfiable
    GuardSoundness,

    /// Projection determinism: same snapshot → same output
    ProjectionDeterminism,

    /// SLO preservation: operator latencies within bounds
    SLOPreservation,

    /// Snapshot immutability: IDs never change
    ImmutabilityOfSnapshots,

    /// Atomic promotion: pointer swap is indivisible
    AtomicPromotion,
}

/// Validation context
#[derive(Debug, Clone)]
pub struct ValidationContext {
    /// Proposal being validated
    pub proposal: DeltaSigmaProposal,

    /// Current snapshot
    pub current_snapshot: Arc<SigmaSnapshot>,

    /// Expected new snapshot (after applying proposal)
    pub expected_new_snapshot: Arc<SigmaSnapshot>,

    /// Sector being validated
    pub sector: String,

    /// Hard invariants to check
    pub invariants: Vec<Invariant>,
}

/// Static validator: SHACL, OWL, Σ² rules
#[async_trait]
pub trait StaticValidator: Send + Sync {
    async fn validate(&self, ctx: &ValidationContext) -> Result<ValidationEvidence, String>;
}

/// Dynamic validator: shadow execution, tests
#[async_trait]
pub trait DynamicValidator: Send + Sync {
    async fn validate(&self, ctx: &ValidationContext) -> Result<ValidationEvidence, String>;
}

/// Performance validator: latency SLOs, memory bounds
#[async_trait]
pub trait PerformanceValidator: Send + Sync {
    async fn validate(&self, ctx: &ValidationContext) -> Result<ValidationEvidence, String>;
}

/// Composite validator: runs all three in parallel
pub struct CompositeValidator {
    static_validator: Arc<dyn StaticValidator>,
    dynamic_validator: Arc<dyn DynamicValidator>,
    performance_validator: Arc<dyn PerformanceValidator>,
}

impl CompositeValidator {
    pub fn new(
        static_validator: Arc<dyn StaticValidator>, dynamic_validator: Arc<dyn DynamicValidator>,
        performance_validator: Arc<dyn PerformanceValidator>,
    ) -> Self {
        Self {
            static_validator,
            dynamic_validator,
            performance_validator,
        }
    }

    /// Run all three validators in parallel
    pub async fn validate_all(
        &self, ctx: &ValidationContext,
    ) -> Result<(ValidationEvidence, ValidationEvidence, ValidationEvidence), String> {
        let ctx1 = ctx.clone();
        let ctx2 = ctx.clone();
        let ctx3 = ctx.clone();

        let sv = self.static_validator.clone();
        let dv = self.dynamic_validator.clone();
        let pv = self.performance_validator.clone();

        let static_handle: JoinHandle<Result<ValidationEvidence, String>> =
            tokio::spawn(async move { sv.validate(&ctx1).await });

        let dynamic_handle: JoinHandle<Result<ValidationEvidence, String>> =
            tokio::spawn(async move { dv.validate(&ctx2).await });

        let perf_handle: JoinHandle<Result<ValidationEvidence, String>> =
            tokio::spawn(async move { pv.validate(&ctx3).await });

        let static_result = static_handle
            .await
            .map_err(|e| format!("Static validation task failed: {}", e))??;

        let dynamic_result = dynamic_handle
            .await
            .map_err(|e| format!("Dynamic validation task failed: {}", e))??;

        let perf_result = perf_handle
            .await
            .map_err(|e| format!("Performance validation task failed: {}", e))??;

        Ok((static_result, dynamic_result, perf_result))
    }

    /// Check all invariants
    pub async fn check_invariants(&self, ctx: &ValidationContext) -> Result<bool, String> {
        for invariant in &ctx.invariants {
            match invariant {
                Invariant::NoRetrocausation => {
                    // Verify no modification to past snapshots
                    if ctx.current_snapshot.id == ctx.expected_new_snapshot.id {
                        return Err("Snapshot ID should change after applying proposal".to_string());
                    }
                }
                Invariant::TypeSoundness => {
                    // Would validate types in RDF triples
                    // For now: mock implementation
                }
                Invariant::GuardSoundness => {
                    // Would validate guard definitions are satisfiable
                }
                Invariant::ProjectionDeterminism => {
                    // Would verify projections are deterministic
                }
                Invariant::SLOPreservation => {
                    // Would check operator latencies
                }
                Invariant::ImmutabilityOfSnapshots => {
                    // Snapshot IDs are immutable by design (content-addressed)
                }
                Invariant::AtomicPromotion => {
                    // Runtime guarantee via atomic operations
                }
            }
        }

        Ok(true)
    }
}

/// Mock static validator
pub struct MockStaticValidator;

#[async_trait]
impl StaticValidator for MockStaticValidator {
    async fn validate(&self, ctx: &ValidationContext) -> Result<ValidationEvidence, String> {
        let start = Instant::now();

        // Mock checks: SHACL, OWL, Σ² rules
        let checks = 5;
        let passed = 5; // All pass in mock

        Ok(ValidationEvidence {
            validator_name: "MockStaticValidator".to_string(),
            passed: true,
            checks_performed: checks,
            checks_passed: passed,
            duration_ms: start.elapsed().as_millis() as u64,
            details: format!("SHACL validation passed for proposal: {}", ctx.proposal.id),
        })
    }
}

/// Mock dynamic validator
pub struct MockDynamicValidator;

#[async_trait]
impl DynamicValidator for MockDynamicValidator {
    async fn validate(&self, ctx: &ValidationContext) -> Result<ValidationEvidence, String> {
        let start = Instant::now();

        // Mock test execution
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

        Ok(ValidationEvidence {
            validator_name: "MockDynamicValidator".to_string(),
            passed: true,
            checks_performed: 3,
            checks_passed: 3,
            duration_ms: start.elapsed().as_millis() as u64,
            details: format!("Shadow projection tests passed for sector: {}", ctx.sector),
        })
    }
}

/// Mock performance validator
pub struct MockPerformanceValidator {
    slo_latency_us: u64,
    slo_memory_bytes: u64,
}

impl MockPerformanceValidator {
    pub fn new(slo_latency_us: u64, slo_memory_bytes: u64) -> Self {
        Self {
            slo_latency_us,
            slo_memory_bytes,
        }
    }
}

#[async_trait]
impl PerformanceValidator for MockPerformanceValidator {
    async fn validate(&self, _ctx: &ValidationContext) -> Result<ValidationEvidence, String> {
        let start = Instant::now();

        // Mock microbenchmark
        tokio::time::sleep(tokio::time::Duration::from_millis(5)).await;

        let mock_latency_us = 500u64;
        let mock_memory_bytes = 1024u64 * 100; // 100 KB

        let latency_ok = mock_latency_us <= self.slo_latency_us;
        let memory_ok = mock_memory_bytes <= self.slo_memory_bytes;
        let passed = latency_ok && memory_ok;

        Ok(ValidationEvidence {
            validator_name: "MockPerformanceValidator".to_string(),
            passed,
            checks_performed: 2,
            checks_passed: if passed { 2 } else { 0 },
            duration_ms: start.elapsed().as_millis() as u64,
            details: format!(
                "Latency: {:.0}μs (SLO: {:.0}μs), Memory: {:.0}KB (SLO: {:.0}KB)",
                mock_latency_us,
                self.slo_latency_us,
                mock_memory_bytes as f64 / 1024.0,
                self.slo_memory_bytes as f64 / 1024.0
            ),
        })
    }
}

/// Validator result: combines all evidence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidatorResult {
    pub overall_passed: bool,
    pub static_evidence: ValidationEvidence,
    pub dynamic_evidence: ValidationEvidence,
    pub performance_evidence: ValidationEvidence,
    pub invariants_checked: Vec<Invariant>,
    pub invariants_passed: bool,
}

impl ValidatorResult {
    pub fn to_receipt(&self, proposal: &DeltaSigmaProposal) -> SigmaReceipt {
        let mut receipt = SigmaReceipt::new(
            Default::default(), // Would be actual snapshot ID
            None,
            format!("Validation for proposal: {}", proposal.id),
        );

        if self.overall_passed && self.invariants_passed {
            receipt = receipt.mark_valid();
            receipt.invariants_preserved = true;
        } else {
            let reason = format!(
                "Static: {}, Dynamic: {}, Performance: {}",
                self.static_evidence.passed,
                self.dynamic_evidence.passed,
                self.performance_evidence.passed
            );
            receipt = receipt.mark_invalid(reason);
        }

        receipt.invariants_checked = self
            .invariants_checked
            .iter()
            .map(|i| format!("{:?}", i))
            .collect();

        receipt.performance_metrics = PerformanceMetrics {
            memory_bytes: 0,
            operator_latency_us: 0,
            slos_met: self.performance_evidence.passed,
            custom: Default::default(),
        };

        receipt.test_results.insert(
            "static_validation".to_string(),
            TestResult {
                name: "Static Validation".to_string(),
                passed: self.static_evidence.passed,
                duration_ms: self.static_evidence.duration_ms,
                error: if self.static_evidence.passed {
                    None
                } else {
                    Some(self.static_evidence.details.clone())
                },
            },
        );

        receipt.test_results.insert(
            "dynamic_validation".to_string(),
            TestResult {
                name: "Dynamic Validation".to_string(),
                passed: self.dynamic_evidence.passed,
                duration_ms: self.dynamic_evidence.duration_ms,
                error: if self.dynamic_evidence.passed {
                    None
                } else {
                    Some(self.dynamic_evidence.details.clone())
                },
            },
        );

        receipt.test_results.insert(
            "performance_validation".to_string(),
            TestResult {
                name: "Performance Validation".to_string(),
                passed: self.performance_evidence.passed,
                duration_ms: self.performance_evidence.duration_ms,
                error: if self.performance_evidence.passed {
                    None
                } else {
                    Some(self.performance_evidence.details.clone())
                },
            },
        );

        receipt
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_context() -> ValidationContext {
        let proposal = DeltaSigmaProposal {
            id: "test_proposal".to_string(),
            change_type: "AddClass".to_string(),
            target_element: "TestClass".to_string(),
            source_patterns: vec!["TestPattern".to_string()],
            confidence: 0.9,
            triples_to_add: vec!["<test> rdf:type owl:Class .".to_string()],
            triples_to_remove: vec![],
            sector: "test".to_string(),
            justification: "Test".to_string(),
            estimated_impact_bytes: 100,
            compatibility: "compatible".to_string(),
        };

        let snapshot = SigmaSnapshot::new(
            None,
            vec![],
            "1.0.0".to_string(),
            "sig".to_string(),
            Default::default(),
        );

        ValidationContext {
            proposal,
            current_snapshot: Arc::new(snapshot.clone()),
            expected_new_snapshot: Arc::new(snapshot),
            sector: "test".to_string(),
            invariants: vec![
                Invariant::NoRetrocausation,
                Invariant::TypeSoundness,
                Invariant::SLOPreservation,
            ],
        }
    }

    #[tokio::test]
    async fn test_static_validator() {
        let validator = MockStaticValidator;
        let ctx = create_test_context();
        let result = validator.validate(&ctx).await.unwrap();

        assert!(result.passed);
        assert_eq!(result.checks_performed, 5);
    }

    #[tokio::test]
    async fn test_dynamic_validator() {
        let validator = MockDynamicValidator;
        let ctx = create_test_context();
        let result = validator.validate(&ctx).await.unwrap();

        assert!(result.passed);
        assert_eq!(result.checks_performed, 3);
    }

    #[tokio::test]
    async fn test_performance_validator() {
        let validator = MockPerformanceValidator::new(1000, 1024 * 100);
        let ctx = create_test_context();
        let result = validator.validate(&ctx).await.unwrap();

        assert!(result.passed);
    }

    #[tokio::test]
    async fn test_composite_validator() {
        let static_v: Arc<dyn StaticValidator> = Arc::new(MockStaticValidator);
        let dynamic_v: Arc<dyn DynamicValidator> = Arc::new(MockDynamicValidator);
        let perf_v: Arc<dyn PerformanceValidator> =
            Arc::new(MockPerformanceValidator::new(1000, 1024 * 100));

        let composite = CompositeValidator::new(static_v, dynamic_v, perf_v);
        let ctx = create_test_context();

        let (static_ev, dynamic_ev, perf_ev) = composite.validate_all(&ctx).await.unwrap();

        assert!(static_ev.passed);
        assert!(dynamic_ev.passed);
        assert!(perf_ev.passed);
    }

    #[tokio::test]
    async fn test_invariant_checking() {
        let static_v: Arc<dyn StaticValidator> = Arc::new(MockStaticValidator);
        let dynamic_v: Arc<dyn DynamicValidator> = Arc::new(MockDynamicValidator);
        let perf_v: Arc<dyn PerformanceValidator> =
            Arc::new(MockPerformanceValidator::new(1000, 1024 * 100));

        let composite = CompositeValidator::new(static_v, dynamic_v, perf_v);
        let ctx = create_test_context();

        let passed = composite.check_invariants(&ctx).await.unwrap();
        assert!(passed);
    }
}
