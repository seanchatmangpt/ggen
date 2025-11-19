//! # Value Stream Mapping Module
//!
//! End-to-end value stream analysis for semantic code generation pipelines.
//! Tracks lead time, process efficiency, bottlenecks, and stakeholder touchpoints
//! from ontology design through deployment.
//!
//! ## Features
//!
//! - **Stage Tracking**: Monitor each pipeline stage with detailed metrics
//! - **Lead Time Analysis**: Track total time through value stream
//! - **Efficiency Ratios**: Calculate process efficiency and value-added ratios
//! - **Bottleneck Detection**: Identify and analyze performance constraints
//! - **Swim Lane Analysis**: Track stakeholder interactions and handoffs
//! - **Future State Mapping**: Define moonshot 2028 targets and improvement initiatives
//! - **RDF Integration**: Store and query VSM data using semantic ontology
//!
//! ## Architecture
//!
//! ```text
//! ValueStream
//!   ├── Stages (with metrics)
//!   ├── Transitions (handoff times)
//!   ├── Swim Lanes (stakeholder roles)
//!   ├── Touchpoints (interactions)
//!   ├── Bottlenecks (constraints)
//!   └── Future State (improvement targets)
//! ```
//!
//! ## Example Usage
//!
//! ```rust,no_run
//! use ggen_domain::vsm::{ValueStream, Stage, StageType, Metrics};
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let mut stream = ValueStream::new("semantic-generation");
//!
//! // Track stage execution
//! let mut stage = Stage::new(StageType::OntologyDesign);
//! stage.start();
//! // ... do work ...
//! stage.end();
//!
//! stream.add_stage(stage);
//!
//! // Analyze bottlenecks
//! let bottlenecks = stream.detect_bottlenecks()?;
//! for bottleneck in bottlenecks {
//!     println!("Bottleneck in {}: {}", bottleneck.stage, bottleneck.severity);
//! }
//!
//! // Calculate efficiency
//! let efficiency = stream.calculate_efficiency();
//! println!("Process efficiency: {:.2}%", efficiency * 100.0);
//! # Ok(())
//! # }
//! ```

pub mod analysis;
pub mod bottleneck;
pub mod flow;
pub mod future_state;
pub mod metrics;
pub mod stage;
pub mod swim_lane;
pub mod touchpoint;
pub mod visualization;

pub use analysis::{AnalysisReport, EfficiencyAnalysis};
pub use bottleneck::{Bottleneck, BottleneckSeverity, BottleneckType};
pub use flow::{Transition, ValueStream};
pub use future_state::{FutureState, MoonshotTarget};
pub use metrics::{Metrics, TimingMetrics};
pub use stage::{Stage, StageType};
pub use swim_lane::{StakeholderRole, SwimLane};
pub use touchpoint::{Touchpoint, TouchpointType};
pub use visualization::{
    BottleneckChart, EfficiencyChart, LeadTimeChart, SwimLaneView, VisualizationData,
};
