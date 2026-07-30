//! Deterministic, non-LLM self-play for SAFe and Enterprise Architecture as Strategy.
//!
//! Self-play means bounded role policies choosing among legal architecture moves
//! against the same admitted state. The engine contains no model calls, hidden
//! prompts, network access, filesystem access, process execution, or external
//! actuation. It manufactures broker-addressed intents, move receipts, and replay
//! evidence for downstream ggen projections and independent boundary tests.

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Canonical receipt schema for one deterministic self-play move.
pub const SELF_PLAY_RECEIPT_SCHEMA: &str = "ggen.architecture.self-play.move.v1";
/// Canonical report schema for one complete self-play scenario.
pub const SELF_PLAY_REPORT_SCHEMA: &str = "ggen.architecture.self-play.report.v1";
/// Self-play can manufacture only broker-addressed intent.
pub const SELF_PLAY_REQUIRED_BROKER: &str = "BRCE";

/// SAFe and Enterprise Architecture as Strategy use cases covered by the framework.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum UseCaseKind {
    /// Lean Portfolio Management evaluates and sequences portfolio epics.
    SafePortfolioEpicFunnel,
    /// ART participants negotiate one Program Increment plan.
    SafePiPlanning,
    /// Cross-team and cross-ART dependencies are exposed and reduced.
    SafeDependencyResolution,
    /// Architecture runway is funded before feature pressure consumes capacity.
    SafeArchitectureRunway,
    /// Integrated system-demo evidence gates release readiness.
    SafeSystemDemoReleaseGate,
    /// Inspect-and-adapt converts observed problems into bounded improvement work.
    SafeInspectAndAdapt,
    /// Integration and standardization define the operating model.
    EaOperatingModel,
    /// Core processes, shared data, linking automation, and channels form the core diagram.
    EaCoreDiagram,
    /// Shared infrastructure, data, transaction, and reusable platforms form the digitized platform.
    EaDigitizedPlatform,
    /// Governance, project management, and linking mechanisms form the engagement model.
    EaEngagementModel,
    /// Reliability, agility, customer fit, and economics establish value realization.
    EaValueRealization,
    /// Current, transition, and target architectures are sequenced without direct actuation.
    EaTransitionArchitecture,
}

/// Deterministic actors used by the canonical scenario profiles.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ActorRole {
    /// Accountable for business outcomes and investment guardrails.
    BusinessOwner,
    /// Accountable for sequencing customer and product value.
    ProductManagement,
    /// Accountable for ART flow, dependency visibility, and PI coordination.
    ReleaseTrainEngineer,
    /// Accountable for solution architecture and architecture runway.
    SystemArchitect,
    /// Accountable for enterprise operating-model and core-diagram coherence.
    EnterpriseArchitect,
    /// Accountable for reusable platform capability and platform economics.
    PlatformOwner,
    /// Accountable for risk, compliance, and policy refusal.
    RiskOfficer,
    /// Independent actor that verifies evidence, receipts, and replay closure.
    EvidenceVerifier,
}

/// Integer-valued architecture state dimensions.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Metric {
    /// Expected or observed business value.
    BusinessValue,
    /// Flow efficiency through the delivery system.
    FlowEfficiency,
    /// Cross-process and cross-system integration.
    Integration,
    /// Process and technology standardization.
    Standardization,
    /// Reuse of shared platform capability.
    PlatformReuse,
    /// Available architecture runway.
    ArchitectureRunway,
    /// Independently verifiable evidence coverage.
    EvidenceCoverage,
    /// Residual enterprise risk; lower is better.
    Risk,
    /// Relative cost or investment consumption; lower is normally better.
    Cost,
    /// Work in process; lower is normally better after value is admitted.
    Wip,
    /// Unresolved dependency pressure; lower is better.
    DependencyLoad,
    /// Integrated release readiness.
    ReleaseReadiness,
    /// Explicit and enforceable decision rights.
    DecisionRights,
    /// Consistency of authoritative shared information.
    DataConsistency,
    /// Customer or stakeholder fit.
    CustomerFit,
}

/// Comparison used by guards, goals, and invariants.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Comparison {
    /// Observed value must be at least the threshold.
    AtLeast,
    /// Observed value must be at most the threshold.
    AtMost,
    /// Observed value must equal the threshold.
    Equal,
}

impl Comparison {
    fn holds(self, observed: i64, threshold: i64) -> bool {
        match self {
            Self::AtLeast => observed >= threshold,
            Self::AtMost => observed <= threshold,
            Self::Equal => observed == threshold,
        }
    }
}

/// One condition over a metric.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MetricConstraint {
    /// Metric evaluated by this condition.
    pub metric: Metric,
    /// Comparison law.
    pub comparison: Comparison,
    /// Required threshold.
    pub threshold: i64,
}

impl MetricConstraint {
    fn holds(&self, metrics: &BTreeMap<Metric, i64>) -> bool {
        self.comparison.holds(
            metrics.get(&self.metric).copied().unwrap_or_default(),
            self.threshold,
        )
    }
}

/// One additive effect of an action.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MetricEffect {
    /// Metric changed by the action.
    pub metric: Metric,
    /// Signed deterministic delta.
    pub delta: i64,
}

/// Deterministic policy for one actor.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ActorPolicy {
    /// Actor governed by this policy.
    pub actor: ActorRole,
    /// Utility weight for every relevant metric.
    pub weights: BTreeMap<Metric, i64>,
    /// Capability tokens admitted for this role in the scenario.
    pub authorities: BTreeSet<String>,
    /// Minimum utility required to choose a move instead of passing.
    pub minimum_utility: i64,
}

/// One legal candidate move in a self-play scenario.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ActionSpec {
    /// Stable action identity.
    pub id: String,
    /// Actor allowed to choose the action.
    pub actor: ActorRole,
    /// Optional exclusive decision group. Only one action in the group may execute.
    pub choice_group: Option<String>,
    /// Guards evaluated against the current state.
    pub guards: Vec<MetricConstraint>,
    /// Additive state transition.
    pub effects: Vec<MetricEffect>,
    /// Capability tokens required by the action.
    pub required_authorities: BTreeSet<String>,
    /// Named evidence produced by execution of the model transition.
    pub evidence: BTreeSet<String>,
    /// Whether the real-world consequence would require broker submission.
    pub broker_intent: bool,
}

/// Complete admitted scenario.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SelfPlayScenario {
    /// Stable scenario identity.
    pub id: String,
    /// Use cases exercised by the scenario.
    pub use_cases: BTreeSet<UseCaseKind>,
    /// Bounded round count.
    pub max_rounds: u32,
    /// Initial architecture state.
    pub initial_metrics: BTreeMap<Metric, i64>,
    /// Role policies participating in self-play.
    pub policies: Vec<ActorPolicy>,
    /// Candidate moves.
    pub actions: Vec<ActionSpec>,
    /// Conditions that must remain true after every applied move.
    pub invariants: Vec<MetricConstraint>,
    /// Conditions required for an ALIVE report.
    pub goals: Vec<MetricConstraint>,
}

/// Flattened ggen projection manifest. RDF templates can emit this shape without
/// relying on implicit grouping or order-sensitive map construction.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SelfPlayManifest {
    /// Stable program identity.
    pub program_id: String,
    /// Scenario declarations.
    pub scenarios: Vec<ManifestScenario>,
    /// Scenario-to-use-case rows.
    pub scenario_use_cases: Vec<ManifestScenarioUseCase>,
    /// Initial metric rows.
    pub initial_metrics: Vec<ManifestMetricValue>,
    /// Actor declarations.
    pub policies: Vec<ManifestPolicy>,
    /// Policy utility-weight rows.
    pub policy_weights: Vec<ManifestPolicyWeight>,
    /// Policy authority rows.
    pub policy_authorities: Vec<ManifestPolicyAuthority>,
    /// Action declarations.
    pub actions: Vec<ManifestAction>,
    /// Action guard rows.
    pub guards: Vec<ManifestGuard>,
    /// Action effect rows.
    pub effects: Vec<ManifestEffect>,
    /// Action authority rows.
    pub action_authorities: Vec<ManifestActionAuthority>,
    /// Action evidence rows.
    pub action_evidence: Vec<ManifestActionEvidence>,
    /// Scenario invariant rows.
    pub invariants: Vec<ManifestScenarioConstraint>,
    /// Scenario goal rows.
    pub goals: Vec<ManifestScenarioConstraint>,
}

/// Flattened scenario declaration.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestScenario {
    /// Scenario identity.
    pub id: String,
    /// Round ceiling.
    pub max_rounds: u32,
}

/// Flattened scenario-to-use-case row.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestScenarioUseCase {
    /// Owning scenario.
    pub scenario_id: String,
    /// Covered use case.
    pub use_case: UseCaseKind,
}

/// Flattened initial metric row.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestMetricValue {
    /// Owning scenario.
    pub scenario_id: String,
    /// Metric identity.
    pub metric: Metric,
    /// Initial value.
    pub value: i64,
}

/// Flattened policy declaration.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestPolicy {
    /// Owning scenario.
    pub scenario_id: String,
    /// Actor identity.
    pub actor: ActorRole,
    /// Minimum accepted utility.
    pub minimum_utility: i64,
}

/// Flattened policy utility weight.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestPolicyWeight {
    /// Owning scenario.
    pub scenario_id: String,
    /// Actor identity.
    pub actor: ActorRole,
    /// Weighted metric.
    pub metric: Metric,
    /// Signed utility weight.
    pub weight: i64,
}

/// Flattened policy authority token.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestPolicyAuthority {
    /// Owning scenario.
    pub scenario_id: String,
    /// Actor identity.
    pub actor: ActorRole,
    /// Admitted capability token.
    pub authority: String,
}

/// Flattened action declaration.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestAction {
    /// Owning scenario.
    pub scenario_id: String,
    /// Stable action identity.
    pub id: String,
    /// Actor identity.
    pub actor: ActorRole,
    /// Optional exclusive decision group.
    pub choice_group: Option<String>,
    /// Whether a real consequence requires broker submission.
    pub broker_intent: bool,
}

/// Flattened guard row.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestGuard {
    /// Owning scenario.
    pub scenario_id: String,
    /// Owning action.
    pub action_id: String,
    /// Guard metric.
    pub metric: Metric,
    /// Guard comparison.
    pub comparison: Comparison,
    /// Guard threshold.
    pub threshold: i64,
}

/// Flattened effect row.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestEffect {
    /// Owning scenario.
    pub scenario_id: String,
    /// Owning action.
    pub action_id: String,
    /// Effect metric.
    pub metric: Metric,
    /// Signed deterministic delta.
    pub delta: i64,
}

/// Flattened action authority row.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestActionAuthority {
    /// Owning scenario.
    pub scenario_id: String,
    /// Owning action.
    pub action_id: String,
    /// Required capability token.
    pub authority: String,
}

/// Flattened action evidence row.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestActionEvidence {
    /// Owning scenario.
    pub scenario_id: String,
    /// Owning action.
    pub action_id: String,
    /// Evidence identity.
    pub evidence: String,
}

/// Flattened scenario invariant or goal.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManifestScenarioConstraint {
    /// Owning scenario.
    pub scenario_id: String,
    /// Constrained metric.
    pub metric: Metric,
    /// Comparison law.
    pub comparison: Comparison,
    /// Required threshold.
    pub threshold: i64,
}

/// In-memory game state.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct GameState {
    /// Current round, starting at zero before any move.
    pub round: u32,
    /// Current metric values.
    pub metrics: BTreeMap<Metric, i64>,
    /// Actions already executed.
    pub executed_actions: BTreeSet<String>,
    /// Exclusive choice groups already closed.
    pub chosen_groups: BTreeSet<String>,
}

/// One cryptographically chained move receipt.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MoveReceipt {
    /// Receipt schema.
    pub schema: String,
    /// Scenario identity.
    pub scenario_id: String,
    /// Round coordinate.
    pub round: u32,
    /// Actor that selected the move.
    pub actor: ActorRole,
    /// Action identity.
    pub action_id: String,
    /// Utility observed by the selecting policy.
    pub utility: i64,
    /// State digest before the move.
    pub pre_state_hash: String,
    /// State digest after the move.
    pub post_state_hash: String,
    /// Prior receipt-chain head.
    pub predecessor: String,
    /// Digest of this receipt envelope.
    pub receipt_hash: String,
    /// Evidence identities produced by the model transition.
    pub evidence: BTreeSet<String>,
    /// Whether a BRCE intent was manufactured.
    pub broker_intent_emitted: bool,
    /// External actuation is always false in this pure kernel.
    pub actuation_performed: bool,
}

/// Final scenario report.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SelfPlayReport {
    /// Report schema.
    pub schema: String,
    /// Scenario identity.
    pub scenario_id: String,
    /// Covered use cases.
    pub use_cases: BTreeSet<UseCaseKind>,
    /// Bounded evidentiary standing.
    pub standing: SelfPlayStanding,
    /// Whether execution reached a move fixed point.
    pub fixed_point: bool,
    /// Number of completed rounds.
    pub rounds: u32,
    /// Final architecture state.
    pub final_state: GameState,
    /// Ordered move receipts.
    pub receipts: Vec<MoveReceipt>,
    /// Final receipt-chain head.
    pub chain_head: String,
    /// Same-coordinate replay identity.
    pub replay_key: String,
    /// Unmet goal descriptions.
    pub unmet_goals: Vec<String>,
    /// External actuation remains false.
    pub actuation_performed: bool,
}

/// Standing local to deterministic self-play.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum SelfPlayStanding {
    /// All scenario goals, receipts, and replay checks are closed.
    Alive,
    /// Legal bounded execution occurred, but one or more goals remain open.
    PartialAlive,
    /// Scenario admission or execution was refused.
    Blocked,
}

/// Self-play admission, execution, and verification failures.
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum SelfPlayViolation {
    /// Required identity was empty.
    #[error("empty identity at {0}")]
    EmptyIdentity(String),
    /// A scenario identity was duplicated.
    #[error("duplicate scenario {0}")]
    DuplicateScenario(String),
    /// A policy actor was duplicated inside one scenario.
    #[error("duplicate policy for {actor:?} in {scenario}")]
    DuplicatePolicy {
        /// Scenario identity.
        scenario: String,
        /// Actor identity.
        actor: ActorRole,
    },
    /// An action identity was duplicated inside one scenario.
    #[error("duplicate action {action} in {scenario}")]
    DuplicateAction {
        /// Scenario identity.
        scenario: String,
        /// Action identity.
        action: String,
    },
    /// A flattened row referenced an unknown scenario.
    #[error("unknown scenario {0}")]
    UnknownScenario(String),
    /// A flattened row referenced an unknown policy.
    #[error("unknown policy {actor:?} in {scenario}")]
    UnknownPolicy {
        /// Scenario identity.
        scenario: String,
        /// Actor identity.
        actor: ActorRole,
    },
    /// A flattened row referenced an unknown action.
    #[error("unknown action {action} in {scenario}")]
    UnknownAction {
        /// Scenario identity.
        scenario: String,
        /// Action identity.
        action: String,
    },
    /// Scenario has no use cases.
    #[error("scenario {0} covers no use cases")]
    MissingUseCases(String),
    /// Scenario has no bounded rounds.
    #[error("scenario {0} has zero max_rounds")]
    ZeroRoundCeiling(String),
    /// Scenario has no policies or actions.
    #[error("scenario {0} has no executable policy/action closure")]
    MissingExecutionClosure(String),
    /// An action has no effects or evidence.
    #[error("action {action} in {scenario} lacks {missing}")]
    IncompleteAction {
        /// Scenario identity.
        scenario: String,
        /// Action identity.
        action: String,
        /// Missing contract surface.
        missing: String,
    },
    /// An action actor has no policy.
    #[error("action {action} in {scenario} has no actor policy")]
    MissingActorPolicy {
        /// Scenario identity.
        scenario: String,
        /// Action identity.
        action: String,
    },
    /// An invariant was false before play.
    #[error("initial invariant failed in {0}")]
    InitialInvariant(String),
    /// A selected move would violate an invariant.
    #[error("action {action} would violate invariant in {scenario}")]
    InvariantRefused {
        /// Scenario identity.
        scenario: String,
        /// Action identity.
        action: String,
    },
    /// Authority required by an action was not admitted to the actor.
    #[error("action {action} refused authority {authority}")]
    AuthorityRefused {
        /// Action identity.
        action: String,
        /// Missing authority.
        authority: String,
    },
    /// A receipt predecessor or digest did not verify.
    #[error("receipt chain mismatch at index {0}")]
    ReceiptChainMismatch(usize),
    /// Re-execution did not reproduce the report coordinate.
    #[error("deterministic replay mismatch for {0}")]
    ReplayMismatch(String),
}

impl SelfPlayManifest {
    /// Compile flattened ggen projection rows into admitted scenarios.
    pub fn compile(&self) -> Result<Vec<SelfPlayScenario>, SelfPlayViolation> {
        require_identity(&self.program_id, "program_id")?;
        let mut scenarios = BTreeMap::<String, SelfPlayScenario>::new();

        for declaration in &self.scenarios {
            require_identity(&declaration.id, "scenario.id")?;
            if scenarios.contains_key(&declaration.id) {
                return Err(SelfPlayViolation::DuplicateScenario(declaration.id.clone()));
            }
            scenarios.insert(
                declaration.id.clone(),
                SelfPlayScenario {
                    id: declaration.id.clone(),
                    use_cases: BTreeSet::new(),
                    max_rounds: declaration.max_rounds,
                    initial_metrics: BTreeMap::new(),
                    policies: Vec::new(),
                    actions: Vec::new(),
                    invariants: Vec::new(),
                    goals: Vec::new(),
                },
            );
        }

        for row in &self.scenario_use_cases {
            scenario_mut(&mut scenarios, &row.scenario_id)?
                .use_cases
                .insert(row.use_case);
        }

        for row in &self.initial_metrics {
            scenario_mut(&mut scenarios, &row.scenario_id)?
                .initial_metrics
                .insert(row.metric, row.value);
        }

        for row in &self.policies {
            let scenario = scenario_mut(&mut scenarios, &row.scenario_id)?;
            if scenario
                .policies
                .iter()
                .any(|policy| policy.actor == row.actor)
            {
                return Err(SelfPlayViolation::DuplicatePolicy {
                    scenario: row.scenario_id.clone(),
                    actor: row.actor,
                });
            }
            scenario.policies.push(ActorPolicy {
                actor: row.actor,
                weights: BTreeMap::new(),
                authorities: BTreeSet::new(),
                minimum_utility: row.minimum_utility,
            });
        }

        for row in &self.policy_weights {
            policy_mut(&mut scenarios, &row.scenario_id, row.actor)?
                .weights
                .insert(row.metric, row.weight);
        }

        for row in &self.policy_authorities {
            require_identity(&row.authority, "policy_authority")?;
            policy_mut(&mut scenarios, &row.scenario_id, row.actor)?
                .authorities
                .insert(row.authority.clone());
        }

        for row in &self.actions {
            require_identity(&row.id, "action.id")?;
            let scenario = scenario_mut(&mut scenarios, &row.scenario_id)?;
            if scenario.actions.iter().any(|action| action.id == row.id) {
                return Err(SelfPlayViolation::DuplicateAction {
                    scenario: row.scenario_id.clone(),
                    action: row.id.clone(),
                });
            }
            scenario.actions.push(ActionSpec {
                id: row.id.clone(),
                actor: row.actor,
                choice_group: row.choice_group.clone(),
                guards: Vec::new(),
                effects: Vec::new(),
                required_authorities: BTreeSet::new(),
                evidence: BTreeSet::new(),
                broker_intent: row.broker_intent,
            });
        }

        for row in &self.guards {
            action_mut(&mut scenarios, &row.scenario_id, &row.action_id)?
                .guards
                .push(MetricConstraint {
                    metric: row.metric,
                    comparison: row.comparison,
                    threshold: row.threshold,
                });
        }

        for row in &self.effects {
            action_mut(&mut scenarios, &row.scenario_id, &row.action_id)?
                .effects
                .push(MetricEffect {
                    metric: row.metric,
                    delta: row.delta,
                });
        }

        for row in &self.action_authorities {
            require_identity(&row.authority, "action_authority")?;
            action_mut(&mut scenarios, &row.scenario_id, &row.action_id)?
                .required_authorities
                .insert(row.authority.clone());
        }

        for row in &self.action_evidence {
            require_identity(&row.evidence, "action_evidence")?;
            action_mut(&mut scenarios, &row.scenario_id, &row.action_id)?
                .evidence
                .insert(row.evidence.clone());
        }

        for row in &self.invariants {
            scenario_mut(&mut scenarios, &row.scenario_id)?
                .invariants
                .push(MetricConstraint {
                    metric: row.metric,
                    comparison: row.comparison,
                    threshold: row.threshold,
                });
        }

        for row in &self.goals {
            scenario_mut(&mut scenarios, &row.scenario_id)?
                .goals
                .push(MetricConstraint {
                    metric: row.metric,
                    comparison: row.comparison,
                    threshold: row.threshold,
                });
        }

        let mut compiled: Vec<_> = scenarios.into_values().collect();
        compiled.sort_by(|left, right| left.id.cmp(&right.id));
        for scenario in &mut compiled {
            scenario.policies.sort_by_key(|policy| policy.actor);
            scenario
                .actions
                .sort_by(|left, right| left.id.cmp(&right.id));
            validate_scenario(scenario)?;
        }
        Ok(compiled)
    }
}

/// Execute one scenario to a fixed point or round ceiling.
pub fn run_scenario(scenario: &SelfPlayScenario) -> Result<SelfPlayReport, SelfPlayViolation> {
    validate_scenario(scenario)?;
    let mut state = GameState {
        round: 0,
        metrics: scenario.initial_metrics.clone(),
        executed_actions: BTreeSet::new(),
        chosen_groups: BTreeSet::new(),
    };
    if !scenario
        .invariants
        .iter()
        .all(|constraint| constraint.holds(&state.metrics))
    {
        return Err(SelfPlayViolation::InitialInvariant(scenario.id.clone()));
    }

    let mut receipts = Vec::new();
    let mut chain_head = zero_hash();
    let mut fixed_point = false;

    for round in 1..=scenario.max_rounds {
        state.round = round;
        let mut moved = false;
        for policy in &scenario.policies {
            let Some((action, utility)) = select_move(scenario, policy, &state)? else {
                continue;
            };
            let pre_state_hash = state_hash(&state);
            apply_action(scenario, policy, action, &mut state)?;
            let post_state_hash = state_hash(&state);
            let mut receipt = MoveReceipt {
                schema: SELF_PLAY_RECEIPT_SCHEMA.to_owned(),
                scenario_id: scenario.id.clone(),
                round,
                actor: policy.actor,
                action_id: action.id.clone(),
                utility,
                pre_state_hash,
                post_state_hash,
                predecessor: chain_head.clone(),
                receipt_hash: String::new(),
                evidence: action.evidence.clone(),
                broker_intent_emitted: action.broker_intent,
                actuation_performed: false,
            };
            receipt.receipt_hash = receipt_digest(&receipt);
            chain_head = receipt.receipt_hash.clone();
            receipts.push(receipt);
            moved = true;
        }
        if !moved {
            fixed_point = true;
            break;
        }
    }

    let unmet_goals = unmet_goals(scenario, &state.metrics);
    let standing = if unmet_goals.is_empty() && fixed_point {
        SelfPlayStanding::Alive
    } else if receipts.is_empty() {
        SelfPlayStanding::Blocked
    } else {
        SelfPlayStanding::PartialAlive
    };
    let replay_key = replay_key(scenario, &state, &chain_head);

    Ok(SelfPlayReport {
        schema: SELF_PLAY_REPORT_SCHEMA.to_owned(),
        scenario_id: scenario.id.clone(),
        use_cases: scenario.use_cases.clone(),
        standing,
        fixed_point,
        rounds: state.round,
        final_state: state,
        receipts,
        chain_head,
        replay_key,
        unmet_goals,
        actuation_performed: false,
    })
}

/// Execute every scenario in stable identity order.
pub fn run_suite(scenarios: &[SelfPlayScenario]) -> Result<Vec<SelfPlayReport>, SelfPlayViolation> {
    let mut ordered = scenarios.to_vec();
    ordered.sort_by(|left, right| left.id.cmp(&right.id));
    ordered.iter().map(run_scenario).collect()
}

/// Independently verify a report by checking its chain and exact replay.
pub fn verify_report(
    scenario: &SelfPlayScenario, report: &SelfPlayReport,
) -> Result<(), SelfPlayViolation> {
    let mut predecessor = zero_hash();
    for (index, receipt) in report.receipts.iter().enumerate() {
        if receipt.predecessor != predecessor
            || receipt.receipt_hash != receipt_digest(receipt)
            || receipt.actuation_performed
        {
            return Err(SelfPlayViolation::ReceiptChainMismatch(index));
        }
        predecessor = receipt.receipt_hash.clone();
    }
    if predecessor != report.chain_head || report.actuation_performed {
        return Err(SelfPlayViolation::ReceiptChainMismatch(
            report.receipts.len(),
        ));
    }

    let replay = run_scenario(scenario)?;
    if replay.chain_head != report.chain_head
        || replay.replay_key != report.replay_key
        || replay.final_state != report.final_state
        || replay.standing != report.standing
        || replay.receipts != report.receipts
    {
        return Err(SelfPlayViolation::ReplayMismatch(scenario.id.clone()));
    }
    Ok(())
}

/// Verify every report against its scenario by stable identity.
pub fn verify_suite(
    scenarios: &[SelfPlayScenario], reports: &[SelfPlayReport],
) -> Result<(), SelfPlayViolation> {
    let by_id: BTreeMap<_, _> = scenarios
        .iter()
        .map(|scenario| (&scenario.id, scenario))
        .collect();
    for report in reports {
        let scenario = by_id
            .get(&report.scenario_id)
            .copied()
            .ok_or_else(|| SelfPlayViolation::UnknownScenario(report.scenario_id.clone()))?;
        verify_report(scenario, report)?;
    }
    if reports.len() != scenarios.len() {
        return Err(SelfPlayViolation::ReplayMismatch(
            "suite_cardinality".to_owned(),
        ));
    }
    Ok(())
}

fn validate_scenario(scenario: &SelfPlayScenario) -> Result<(), SelfPlayViolation> {
    require_identity(&scenario.id, "scenario.id")?;
    if scenario.use_cases.is_empty() {
        return Err(SelfPlayViolation::MissingUseCases(scenario.id.clone()));
    }
    if scenario.max_rounds == 0 {
        return Err(SelfPlayViolation::ZeroRoundCeiling(scenario.id.clone()));
    }
    if scenario.policies.is_empty() || scenario.actions.is_empty() {
        return Err(SelfPlayViolation::MissingExecutionClosure(
            scenario.id.clone(),
        ));
    }

    let mut actors = BTreeSet::new();
    for policy in &scenario.policies {
        if !actors.insert(policy.actor) {
            return Err(SelfPlayViolation::DuplicatePolicy {
                scenario: scenario.id.clone(),
                actor: policy.actor,
            });
        }
    }

    let mut actions = BTreeSet::new();
    for action in &scenario.actions {
        require_identity(&action.id, "action.id")?;
        if !actions.insert(&action.id) {
            return Err(SelfPlayViolation::DuplicateAction {
                scenario: scenario.id.clone(),
                action: action.id.clone(),
            });
        }
        if !actors.contains(&action.actor) {
            return Err(SelfPlayViolation::MissingActorPolicy {
                scenario: scenario.id.clone(),
                action: action.id.clone(),
            });
        }
        if action.effects.is_empty() {
            return Err(SelfPlayViolation::IncompleteAction {
                scenario: scenario.id.clone(),
                action: action.id.clone(),
                missing: "effects".to_owned(),
            });
        }
        if action.evidence.is_empty() {
            return Err(SelfPlayViolation::IncompleteAction {
                scenario: scenario.id.clone(),
                action: action.id.clone(),
                missing: "evidence".to_owned(),
            });
        }
    }
    Ok(())
}

fn select_move<'a>(
    scenario: &'a SelfPlayScenario, policy: &ActorPolicy, state: &GameState,
) -> Result<Option<(&'a ActionSpec, i64)>, SelfPlayViolation> {
    let mut candidates = Vec::new();
    for action in scenario
        .actions
        .iter()
        .filter(|action| action.actor == policy.actor)
    {
        if state.executed_actions.contains(&action.id)
            || action
                .choice_group
                .as_ref()
                .is_some_and(|group| state.chosen_groups.contains(group))
            || !action
                .guards
                .iter()
                .all(|guard| guard.holds(&state.metrics))
        {
            continue;
        }
        if let Some(authority) = action
            .required_authorities
            .iter()
            .find(|authority| !policy.authorities.contains(*authority))
        {
            return Err(SelfPlayViolation::AuthorityRefused {
                action: action.id.clone(),
                authority: authority.clone(),
            });
        }
        let projected = projected_metrics(&state.metrics, &action.effects);
        if !scenario
            .invariants
            .iter()
            .all(|constraint| constraint.holds(&projected))
        {
            continue;
        }
        let utility = action.effects.iter().fold(0_i64, |total, effect| {
            total.saturating_add(
                policy
                    .weights
                    .get(&effect.metric)
                    .copied()
                    .unwrap_or_default()
                    .saturating_mul(effect.delta),
            )
        });
        if utility >= policy.minimum_utility {
            candidates.push((action, utility));
        }
    }
    candidates.sort_by(
        |(left_action, left_utility), (right_action, right_utility)| {
            right_utility
                .cmp(left_utility)
                .then_with(|| left_action.id.cmp(&right_action.id))
        },
    );
    Ok(candidates.into_iter().next())
}

fn apply_action(
    scenario: &SelfPlayScenario, policy: &ActorPolicy, action: &ActionSpec, state: &mut GameState,
) -> Result<(), SelfPlayViolation> {
    for authority in &action.required_authorities {
        if !policy.authorities.contains(authority) {
            return Err(SelfPlayViolation::AuthorityRefused {
                action: action.id.clone(),
                authority: authority.clone(),
            });
        }
    }
    let projected = projected_metrics(&state.metrics, &action.effects);
    if !scenario
        .invariants
        .iter()
        .all(|constraint| constraint.holds(&projected))
    {
        return Err(SelfPlayViolation::InvariantRefused {
            scenario: scenario.id.clone(),
            action: action.id.clone(),
        });
    }
    state.metrics = projected;
    state.executed_actions.insert(action.id.clone());
    if let Some(group) = &action.choice_group {
        state.chosen_groups.insert(group.clone());
    }
    Ok(())
}

fn projected_metrics(
    current: &BTreeMap<Metric, i64>, effects: &[MetricEffect],
) -> BTreeMap<Metric, i64> {
    let mut projected = current.clone();
    for effect in effects {
        let value = projected.entry(effect.metric).or_default();
        *value = value.saturating_add(effect.delta);
    }
    projected
}

fn unmet_goals(scenario: &SelfPlayScenario, metrics: &BTreeMap<Metric, i64>) -> Vec<String> {
    scenario
        .goals
        .iter()
        .filter(|goal| !goal.holds(metrics))
        .map(|goal| format!("{:?} {:?} {}", goal.metric, goal.comparison, goal.threshold))
        .collect()
}

fn state_hash(state: &GameState) -> String {
    let bytes = serde_json::to_vec(state).expect("GameState serialization is infallible");
    blake3::hash(&bytes).to_hex().to_string()
}

fn receipt_digest(receipt: &MoveReceipt) -> String {
    let mut canonical = receipt.clone();
    canonical.receipt_hash.clear();
    let bytes = serde_json::to_vec(&canonical).expect("MoveReceipt serialization is infallible");
    blake3::hash(&bytes).to_hex().to_string()
}

fn replay_key(scenario: &SelfPlayScenario, state: &GameState, chain_head: &str) -> String {
    let bytes = serde_json::to_vec(&(scenario, state, chain_head))
        .expect("self-play replay coordinate serialization is infallible");
    blake3::hash(&bytes).to_hex().to_string()
}

fn zero_hash() -> String {
    "0".repeat(64)
}

fn require_identity(value: &str, scope: &str) -> Result<(), SelfPlayViolation> {
    if value.trim().is_empty() {
        Err(SelfPlayViolation::EmptyIdentity(scope.to_owned()))
    } else {
        Ok(())
    }
}

fn scenario_mut<'a>(
    scenarios: &'a mut BTreeMap<String, SelfPlayScenario>, scenario_id: &str,
) -> Result<&'a mut SelfPlayScenario, SelfPlayViolation> {
    scenarios
        .get_mut(scenario_id)
        .ok_or_else(|| SelfPlayViolation::UnknownScenario(scenario_id.to_owned()))
}

fn policy_mut<'a>(
    scenarios: &'a mut BTreeMap<String, SelfPlayScenario>, scenario_id: &str, actor: ActorRole,
) -> Result<&'a mut ActorPolicy, SelfPlayViolation> {
    scenario_mut(scenarios, scenario_id)?
        .policies
        .iter_mut()
        .find(|policy| policy.actor == actor)
        .ok_or_else(|| SelfPlayViolation::UnknownPolicy {
            scenario: scenario_id.to_owned(),
            actor,
        })
}

fn action_mut<'a>(
    scenarios: &'a mut BTreeMap<String, SelfPlayScenario>, scenario_id: &str, action_id: &str,
) -> Result<&'a mut ActionSpec, SelfPlayViolation> {
    scenario_mut(scenarios, scenario_id)?
        .actions
        .iter_mut()
        .find(|action| action.id == action_id)
        .ok_or_else(|| SelfPlayViolation::UnknownAction {
            scenario: scenario_id.to_owned(),
            action: action_id.to_owned(),
        })
}
