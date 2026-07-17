#![allow(missing_docs)]
#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(
    clippy::must_use_candidate,
    clippy::needless_lifetimes,
    clippy::return_self_not_must_use
)]

use serde_json::Value;

pub trait SectorStack: Send + Sync {
    fn name(&self) -> &str;
    fn validate_artifact(&self, artifact: &Value) -> Result<(), String>;
}

#[derive(Debug, Clone)]
pub enum MergeStrategy {
    Strict,
    Lenient,
    Precedence(Vec<String>),
}

pub struct ProcessIntelligenceSector;
impl SectorStack for ProcessIntelligenceSector {
    fn name(&self) -> &str {
        "process_intelligence"
    }
    fn validate_artifact(&self, artifact: &Value) -> Result<(), String> {
        if artifact.is_null() {
            return Err("ProcessIntelligenceSector: artifact must not be null".to_string());
        }
        match artifact {
            Value::Object(map) => {
                if map.is_empty() {
                    return Err(
                        "ProcessIntelligenceSector: artifact object must not be empty".to_string(),
                    );
                }
                Ok(())
            }
            Value::Array(_) => Err(
                "ProcessIntelligenceSector: artifact must be a JSON object, got array".to_string(),
            ),
            Value::Bool(_) => Err(
                "ProcessIntelligenceSector: artifact must be a JSON object, got bool".to_string(),
            ),
            Value::Number(_) => Err(
                "ProcessIntelligenceSector: artifact must be a JSON object, got number".to_string(),
            ),
            Value::String(_) => Err(
                "ProcessIntelligenceSector: artifact must be a JSON object, got string".to_string(),
            ),
            Value::Null => unreachable!("null already handled above"),
        }
    }
}
