pub mod config_policy;
pub mod evidence_policy;
pub mod test_policy;
pub mod path_policy;
pub mod tool_adapter;
pub mod receipt_policy;
pub mod decision;

use serde::Serialize;

#[derive(Debug, Clone, Serialize)]
pub struct Violation {
    pub pattern: String,
    pub location: String,
    pub rule: String,
}
