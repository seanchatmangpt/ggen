use super::ea_common::{load_graph_from_root, refusal_body};
use crate::error::{ErrorCategory, McpError};
use ggen_abb_sbb::{replay, Generator, ManufactureReceipt};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct EaReplayParams {
    pub root: String,
    #[serde(default)] pub graph_path: Option<String>,
    pub receipt: Value,
    #[serde(default)] pub generator_id: Option<String>,
    #[serde(default)] pub generator_version: Option<String>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct EaReplayResult {
    pub ok: bool,
    #[serde(skip_serializing_if = "Option::is_none")] pub artifacts: Option<Vec<Value>>,
    #[serde(skip_serializing_if = "Option::is_none")] pub receipt: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")] pub refusal: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")] pub refusal_detail: Option<String>,
}

pub fn ea_replay(params: &EaReplayParams) -> Result<EaReplayResult, McpError> {
    let g = load_graph_from_root(&params.root, params.graph_path.as_deref())?;
    let receipt: ManufactureReceipt = serde_json::from_value(params.receipt.clone()).map_err(|e| McpError::new(ErrorCategory::Unsupported, e.to_string()))?;
    let gen = Generator {
        id: params.generator_id.clone().unwrap_or_else(|| "ggen-mcp".into()),
        version: params.generator_version.clone().unwrap_or_else(|| env!("CARGO_PKG_VERSION").into()),
    };
    match replay(&receipt, &g, &gen) {
        Ok(m) => Ok(EaReplayResult {
            ok: true,
            artifacts: Some(m.artifacts.iter().map(serde_json::to_value).collect::<Result<_,_>>().map_err(|e| McpError::new(crate::error::ErrorCategory::Internal, e.to_string()))?),
            receipt: Some(serde_json::to_value(&m.receipt).map_err(|e| McpError::new(crate::error::ErrorCategory::Internal, e.to_string()))?),
            refusal: None,
            refusal_detail: None,
        }),
        Err(r) => { let b = refusal_body(&r); Ok(EaReplayResult { ok: false, artifacts: None, receipt: None, refusal: Some(b.refusal), refusal_detail: Some(b.detail) }) }
    }
}
