// Legacy step definitions (flat command structure)
pub mod cli_steps;
pub mod common_steps;
pub mod determinism_steps;
pub mod installation_steps;
pub mod marketplace_steps;
pub mod multilang_steps;
pub mod quickstart_steps;
pub mod rdf_steps;
pub mod template_steps;

// New noun-verb command step definitions
pub mod graph_steps;
pub mod market_noun_verb_steps;
pub mod project_steps;
pub mod template_management_steps;

// Re-export step modules for cucumber registration
#[allow(unused_imports)]
pub use cli_steps::*;
#[allow(unused_imports)]
pub use common_steps::*;
#[allow(unused_imports)]
pub use determinism_steps::*;
#[allow(unused_imports)]
pub use installation_steps::*;
#[allow(unused_imports)]
pub use marketplace_steps::*;
#[allow(unused_imports)]
pub use multilang_steps::*;
#[allow(unused_imports)]
pub use quickstart_steps::*;
#[allow(unused_imports)]
pub use rdf_steps::*;
#[allow(unused_imports)]
pub use template_steps::*;

// Re-export new noun-verb step modules
#[allow(unused_imports)]
pub use graph_steps::*;
#[allow(unused_imports)]
pub use market_noun_verb_steps::*;
#[allow(unused_imports)]
pub use project_steps::*;
#[allow(unused_imports)]
pub use template_management_steps::*;
