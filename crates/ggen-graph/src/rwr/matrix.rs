//! Ross-Weill-Robertson foundation-for-execution maturity contract.

use serde::{Deserialize, Serialize};

/// The five enterprise-architecture stages used by this crate.
///
/// Stages one through four are the canonical Ross-Weill-Robertson sequence.
/// Stage five is the later MIT CISR extension in which modular capabilities
/// participate in a governed digital ecosystem.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(u8)]
pub enum MaturityLevel {
    /// Locally optimized applications and processes.
    BusinessSilos = 1,
    /// Shared technology standards.
    StandardizedTechnology = 2,
    /// Standardized and integrated core processes and data.
    OptimizedCore = 3,
    /// Reusable business components over the optimized core.
    BusinessModularity = 4,
    /// Governed participation in a digital ecosystem.
    DigitalEcosystem = 5,
}

impl MaturityLevel {
    /// Return the stable level number.
    #[must_use]
    pub const fn number(self) -> u8 {
        self as u8
    }
}

/// The major RWR architecture concern to which a dimension belongs.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(u8)]
pub enum RwrDomain {
    /// Integration, standardization, and decision rights.
    OperatingModel = 1,
    /// The one-page enterprise architecture core diagram.
    CoreDiagram = 2,
    /// The operational backbone and reusable digital platform.
    DigitizedPlatform = 3,
    /// Governance, project management, and linking mechanisms.
    EngagementModel = 4,
    /// Reliability, agility, and economic consequences.
    ValueRealization = 5,
    /// Chatman execution-law extension: machinery, automation, autonomics, receipts.
    ExecutionControl = 6,
}

/// Proof surfaces admitted by the maturity assessor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(u8)]
pub enum EvidenceSurface {
    /// Observed execution crossed a real boundary.
    Execution = 1,
    /// Durable state was observed before or after execution.
    State = 2,
    /// A process trace, lifecycle, or ordered transition was observed.
    Process = 3,
    /// Evidence binds a cause to its consequence.
    Causality = 4,
    /// Operational telemetry was captured from execution.
    Telemetry = 5,
    /// Decision rights or policy enforcement were observed.
    Governance = 6,
    /// A partner, channel, or ecosystem boundary was crossed.
    External = 7,
    /// Reuse, unit cost, cycle time, or another value measure was observed.
    Economics = 8,
}

/// Every dimension required for a full RWR Level-5 foundation in ggen.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(u8)]
pub enum Dimension {
    /// Required process and data integration is explicit and executable.
    ProcessIntegration = 1,
    /// Required process standardization is explicit and executable.
    ProcessStandardization = 2,
    /// Enterprise and local decision rights are explicit and enforced.
    OperatingModelDecisionRights = 3,
    /// Core processes are named, ordered, executable, and observable.
    CoreProcesses = 4,
    /// Shared enterprise data has canonical identity and semantics.
    SharedData = 5,
    /// Linking and automation technologies connect the core without bypasses.
    LinkingAutomation = 6,
    /// Customer and partner channels normalize into the same core contracts.
    CustomerPartnerChannels = 7,
    /// Technology standards are shared and mechanically enforced.
    TechnologyStandardization = 8,
    /// The optimized core executes integrated processes over shared data.
    OperationalBackbone = 9,
    /// Business components are reusable without rebuilding the core.
    ReusableBusinessComponents = 10,
    /// External ecosystem interfaces are bounded, versioned, and governed.
    EcosystemInterfaces = 11,
    /// Enterprise governance allocates decision rights and refuses exceptions.
    EnterpriseGovernance = 12,
    /// Projects carry executable scope, acceptance, ownership, and receipts.
    ProjectManagement = 13,
    /// Projects are mechanically linked to architecture and governance.
    ArchitectureLinkingMechanisms = 14,
    /// Execution is reliable and transparent enough to operate predictably.
    ReliabilityTransparency = 15,
    /// New offerings can be assembled from reusable capabilities quickly.
    StrategicAgility = 16,
    /// The platform proves reuse, cost, throughput, or value consequences.
    EconomicValue = 17,
    /// The machinery can construct bounded actions from admitted state.
    MachineryClosure = 18,
    /// Repetitive execution proceeds without manual intervention.
    AutomationClosure = 19,
    /// The system monitors, analyzes, plans, executes, and learns in a closed loop.
    AutonomicClosure = 20,
    /// Every consequence is receipted, verifiable, and replay-protected.
    ReceiptReplay = 21,
}

/// Stable ordered set of all full-matrix dimensions.
pub const ALL_DIMENSIONS: [Dimension; 21] = [
    Dimension::ProcessIntegration,
    Dimension::ProcessStandardization,
    Dimension::OperatingModelDecisionRights,
    Dimension::CoreProcesses,
    Dimension::SharedData,
    Dimension::LinkingAutomation,
    Dimension::CustomerPartnerChannels,
    Dimension::TechnologyStandardization,
    Dimension::OperationalBackbone,
    Dimension::ReusableBusinessComponents,
    Dimension::EcosystemInterfaces,
    Dimension::EnterpriseGovernance,
    Dimension::ProjectManagement,
    Dimension::ArchitectureLinkingMechanisms,
    Dimension::ReliabilityTransparency,
    Dimension::StrategicAgility,
    Dimension::EconomicValue,
    Dimension::MachineryClosure,
    Dimension::AutomationClosure,
    Dimension::AutonomicClosure,
    Dimension::ReceiptReplay,
];

/// Version of the mechanized full maturity contract.
pub const MATRIX_VERSION: &str = "rwr-ggen-level5/v1";

/// Required proof contract for one dimension.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct DimensionContract {
    /// Dimension being assessed.
    pub dimension: Dimension,
    /// RWR concern that owns the dimension.
    pub domain: RwrDomain,
    /// Human-readable Level-5 outcome.
    pub level5_outcome: &'static str,
    /// Minimum distinct proof surfaces required for Level 5.
    pub required_surfaces: &'static [EvidenceSurface],
}

/// Return the complete contract for a dimension.
#[must_use]
pub const fn contract(dimension: Dimension) -> DimensionContract {
    use Dimension as D;
    use EvidenceSurface as S;
    use RwrDomain as R;

    match dimension {
        D::ProcessIntegration => DimensionContract {
            dimension,
            domain: R::OperatingModel,
            level5_outcome: "Core transactions share state and consequences across enterprise boundaries.",
            required_surfaces: &[S::Execution, S::State, S::Causality],
        },
        D::ProcessStandardization => DimensionContract {
            dimension,
            domain: R::OperatingModel,
            level5_outcome: "Equivalent operating units execute one governed process contract.",
            required_surfaces: &[S::Execution, S::Process, S::State],
        },
        D::OperatingModelDecisionRights => DimensionContract {
            dimension,
            domain: R::OperatingModel,
            level5_outcome: "Enterprise and local discretion are explicit, executable, and refusal-bearing.",
            required_surfaces: &[S::Governance, S::Process, S::Causality],
        },
        D::CoreProcesses => DimensionContract {
            dimension,
            domain: R::CoreDiagram,
            level5_outcome: "The core process graph is executable, observable, and complete for the bounded scope.",
            required_surfaces: &[S::Process, S::Execution, S::State],
        },
        D::SharedData => DimensionContract {
            dimension,
            domain: R::CoreDiagram,
            level5_outcome: "Canonical data identities and semantics are shared by every participating component.",
            required_surfaces: &[S::State, S::Execution, S::Causality],
        },
        D::LinkingAutomation => DimensionContract {
            dimension,
            domain: R::CoreDiagram,
            level5_outcome: "Events, policies, and components are linked by executable automation rather than manual handoffs.",
            required_surfaces: &[S::Execution, S::Process, S::Causality],
        },
        D::CustomerPartnerChannels => DimensionContract {
            dimension,
            domain: R::CoreDiagram,
            level5_outcome: "Channels and partners enter through versioned contracts and normalize into the shared core.",
            required_surfaces: &[S::External, S::Execution, S::State],
        },
        D::TechnologyStandardization => DimensionContract {
            dimension,
            domain: R::DigitizedPlatform,
            level5_outcome: "Technology standards are reusable platform contracts with machine-enforced exceptions.",
            required_surfaces: &[S::Governance, S::State, S::Execution],
        },
        D::OperationalBackbone => DimensionContract {
            dimension,
            domain: R::DigitizedPlatform,
            level5_outcome: "Integrated core processes and shared data execute reliably as an operational backbone.",
            required_surfaces: &[S::Execution, S::State, S::Telemetry],
        },
        D::ReusableBusinessComponents => DimensionContract {
            dimension,
            domain: R::DigitizedPlatform,
            level5_outcome: "New offerings compose reusable business components without modifying the optimized core.",
            required_surfaces: &[S::Execution, S::State, S::Economics],
        },
        D::EcosystemInterfaces => DimensionContract {
            dimension,
            domain: R::DigitizedPlatform,
            level5_outcome: "External participants consume bounded interfaces while governance and evidence remain internal.",
            required_surfaces: &[S::External, S::Governance, S::Execution],
        },
        D::EnterpriseGovernance => DimensionContract {
            dimension,
            domain: R::EngagementModel,
            level5_outcome: "Enterprise priorities, standards, exceptions, and refusals are encoded as executable policy.",
            required_surfaces: &[S::Governance, S::Process, S::Causality],
        },
        D::ProjectManagement => DimensionContract {
            dimension,
            domain: R::EngagementModel,
            level5_outcome: "Projects carry deterministic outcomes, owners, acceptance commands, evidence, and receipts.",
            required_surfaces: &[S::Process, S::Execution, S::Governance],
        },
        D::ArchitectureLinkingMechanisms => DimensionContract {
            dimension,
            domain: R::EngagementModel,
            level5_outcome: "Every project consumes, preserves, or lawfully extends the enterprise architecture.",
            required_surfaces: &[S::Governance, S::Process, S::Execution],
        },
        D::ReliabilityTransparency => DimensionContract {
            dimension,
            domain: R::ValueRealization,
            level5_outcome: "Operational outcomes and failures are predictable, observable, and externally auditable.",
            required_surfaces: &[S::Telemetry, S::State, S::Execution],
        },
        D::StrategicAgility => DimensionContract {
            dimension,
            domain: R::ValueRealization,
            level5_outcome: "A new bounded offering is assembled from existing capabilities without core redesign.",
            required_surfaces: &[S::Economics, S::Execution, S::Process],
        },
        D::EconomicValue => DimensionContract {
            dimension,
            domain: R::ValueRealization,
            level5_outcome: "Reuse, automation, throughput, and marginal-cost consequences are measured from execution.",
            required_surfaces: &[S::Economics, S::State, S::Causality],
        },
        D::MachineryClosure => DimensionContract {
            dimension,
            domain: R::ExecutionControl,
            level5_outcome: "Admitted state deterministically produces bounded executable actions.",
            required_surfaces: &[S::Execution, S::State, S::Causality],
        },
        D::AutomationClosure => DimensionContract {
            dimension,
            domain: R::ExecutionControl,
            level5_outcome: "A trigger proceeds through policy, execution, and receipt without a manual step.",
            required_surfaces: &[S::Execution, S::Process, S::Causality],
        },
        D::AutonomicClosure => DimensionContract {
            dimension,
            domain: R::ExecutionControl,
            level5_outcome: "Monitor, analyze, plan, execute, and knowledge update converge or type-refuse.",
            required_surfaces: &[S::Execution, S::Telemetry, S::Causality],
        },
        D::ReceiptReplay => DimensionContract {
            dimension,
            domain: R::ExecutionControl,
            level5_outcome: "Every consequence carries a cryptographic receipt and duplicate replay is refused.",
            required_surfaces: &[S::Execution, S::State, S::Causality],
        },
    }
}

/// Return every contract in stable matrix order.
#[must_use]
pub fn all_contracts() -> Vec<DimensionContract> {
    ALL_DIMENSIONS.iter().copied().map(contract).collect()
}
