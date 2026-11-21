//! `ggen_ea_verify_receipt`
use super::ea_common::refusal_body;
use crate::error::{ErrorCategory, McpError};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::Value;
#[derive(Debug, Deserialize, JsonSchema)]
pub struct EaVerifyReceiptParams {
    #[serde(default)]
    pub receipt_path: Option<String>,
    #[serde(default)]
    pub receipt: Option<Value>,
}
#[derive(Debug, Serialize, JsonSchema)]
pub struct EaVerifyReceiptResult {
    pub ok: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub receipt: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub refusal: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub refusal_detail: Option<String>,
}
pub fn ea_verify_receipt(
    params: &EaVerifyReceiptParams,
) -> Result<EaVerifyReceiptResult, McpError> {
    let r: ggen_abb_sbb::ManufactureReceipt = if let Some(v) = &params.receipt {
        serde_json::from_value(v.clone())
            .map_err(|e| McpError::new(ErrorCategory::Unsupported, e.to_string()))?
    } else if let Some(path) = &params.receipt_path {
        let raw = std::fs::read_to_string(path)
            .map_err(|e| McpError::new(ErrorCategory::NotFound, e.to_string()))?;
        serde_json::from_str(&raw)
            .map_err(|e| McpError::new(ErrorCategory::Unsupported, e.to_string()))?
    } else {
        return Err(McpError::new(
            ErrorCategory::Unsupported,
            "one of receipt or receipt_path is required".to_string(),
        ));
    };
    match ggen_abb_sbb::verify_receipt(&r) {
        Ok(()) => Ok(EaVerifyReceiptResult {
            ok: true,
            receipt: Some(
                serde_json::json!({"receipt_digest": r.receipt_digest, "graph_digest": r.graph_digest}),
            ),
            refusal: None,
            refusal_detail: None,
        }),
        Err(refusal) => {
            let b = refusal_body(&refusal);
            Ok(EaVerifyReceiptResult {
                ok: false,
                receipt: None,
                refusal: Some(b.refusal),
                refusal_detail: Some(b.detail),
            })
        }
    }
}
