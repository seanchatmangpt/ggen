//! `ggen_ea_plan`
use super::ea_common::{load_graph_from_root, parse_authority, refusal_body};
use crate::error::McpError;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::Value;
#[derive(Debug, Deserialize, JsonSchema)]
pub struct EaPlanParams {
    pub root: String,
    #[serde(default)]
    pub graph_path: Option<String>,
    #[serde(default)]
    pub abb: Option<String>,
    #[serde(default)]
    pub authority: Option<String>,
}
#[derive(Debug, Serialize, JsonSchema)]
pub struct EaPlanResult {
    pub ok: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub decision: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub refusal: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub refusal_detail: Option<String>,
}
pub fn ea_plan(params: &EaPlanParams) -> Result<EaPlanResult, McpError> {
    let g = load_graph_from_root(&params.root, params.graph_path.as_deref())?;
    let authority = parse_authority(params.authority.as_deref())?;
    let abb = params
        .abb
        .clone()
        .unwrap_or_else(|| "abb:event-ingest".to_string());
    match ggen_abb_sbb::plan(&g, &abb, authority) {
        Ok(decision) => Ok(EaPlanResult {
            ok: true,
            decision: Some(serde_json::to_value(decision).map_err(|e| {
                crate::error::McpError::new(crate::error::ErrorCategory::Internal, e.to_string())
            })?),
            refusal: None,
            refusal_detail: None,
        }),
        Err(refusal) => {
            let b = refusal_body(&refusal);
            Ok(EaPlanResult {
                ok: false,
                decision: None,
                refusal: Some(b.refusal),
                refusal_detail: Some(b.detail),
            })
        }
    }
}
