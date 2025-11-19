//! Six Sigma DMAIC framework for ontology quality management
//!
//! This module implements comprehensive Six Sigma methodologies for RDF ontology quality,
//! including DMAIC cycles, statistical process control, and capability indices.
//!
//! # Components
//!
//! - `dmaic`: Define-Measure-Analyze-Improve-Control framework
//! - `spc`: Statistical Process Control charts for generation consistency
//! - `capability`: Cp/Cpk capability indices for template precision
//! - `dpmo`: Defects Per Million Opportunities tracking
//! - `training`: Black belt training data structures for AI quality agents
//!
//! # Six Sigma Quality Levels
//!
//! | Sigma Level | DPMO | Yield % |
//! |-------------|------|---------|
//! | 1σ | 691,462 | 30.9% |
//! | 2σ | 308,538 | 69.1% |
//! | 3σ | 66,807 | 93.3% |
//! | 4σ | 6,210 | 99.38% |
//! | 5σ | 233 | 99.977% |
//! | 6σ | 3.4 | 99.99966% |

pub mod capability;
pub mod dmaic;
pub mod dpmo;
pub mod spc;
pub mod training;

pub use capability::{CapabilityIndex, ProcessCapability};
pub use dmaic::{
    AnalyzePhase, ControlPhase, DefinePhase, DmaicCycle, ImprovePhase, MeasurePhase,
};
pub use dpmo::{DefectTracker, DpmoCalculator, OpportunityType};
pub use spc::{ControlChart, ControlLimits, ProcessMetric, SpcChart};
pub use training::{BlackBeltData, QualityAgent, TrainingExample};
