use super::ea_common::{load_graph_from_root, parse_authority, parse_generation, refusal_body};
use crate::error::McpError;
use ggen_abb_sbb::{admit, manufacture, Generator};
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct EaManufactureParams {
    pub root: String,
    #[serde(default)]
    pub graph_path: Option<String>,
    #[serde(default)]
    pub abb: Option<String>,
    #[serde(default)]
    pub sbb: Option<String>,
    #[serde(default)]
    pub authority: Option<String>,
    #[serde(default)]
    pub expected_graph_digest: Option<String>,
    #[serde(default)]
    pub generation: Option<String>,
    #[serde(default)]
    pub baseline_generation: Option<String>,
    #[serde(default)]
    pub generator_id: Option<String>,
    #[serde(default)]
    pub generator_version: Option<String>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct EaManufactureResult {
    pub ok: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub artifacts: Option<Vec<Value>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub receipt: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub refusal: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub refusal_detail: Option<String>,
}

pub fn ea_manufacture(params: &EaManufactureParams) -> Result<EaManufactureResult, McpError> {
    let g = load_graph_from_root(&params.root, params.graph_path.as_deref())?;
    let req = ggen_abb_sbb::Request {
        abb: params
            .abb
            .clone()
            .unwrap_or_else(|| "abb:event-ingest".into()),
        sbb: params
            .sbb
            .clone()
            .unwrap_or_else(|| "sbb:ingest-0000".into()),
        requested_authority: parse_authority(params.authority.as_deref())?,
        expected_graph_digest: params.expected_graph_digest.clone(),
        receipt_generation: parse_generation(params.generation.as_deref(), "generation")?,
        artifact_baseline_generation: parse_generation(
            params.baseline_generation.as_deref(),
            "baseline_generation",
        )?,
    };
    let gen = Generator {
        id: params
            .generator_id
            .clone()
            .unwrap_or_else(|| "ggen-mcp".into()),
        version: params
            .generator_version
            .clone()
            .unwrap_or_else(|| env!("CARGO_PKG_VERSION").into()),
    };
    let ad = match admit(&g, &req) {
        Ok(a) => a,
        Err(r) => {
            let b = refusal_body(&r);
            return Ok(EaManufactureResult {
                ok: false,
                artifacts: None,
                receipt: None,
                refusal: Some(b.refusal),
                refusal_detail: Some(b.detail),
            });
        }
    };
    match manufacture(&ad, &gen) {
        Ok(m) => Ok(EaManufactureResult {
            ok: true,
            artifacts: Some(
                m.artifacts
                    .iter()
                    .map(serde_json::to_value)
                    .collect::<Result<_, _>>()
                    .map_err(|e| {
                        McpError::new(crate::error::ErrorCategory::Internal, e.to_string())
                    })?,
            ),
            receipt: Some(serde_json::to_value(&m.receipt).map_err(|e| {
                McpError::new(crate::error::ErrorCategory::Internal, e.to_string())
            })?),
            refusal: None,
            refusal_detail: None,
        }),
        Err(r) => {
            let b = refusal_body(&r);
            Ok(EaManufactureResult {
                ok: false,
                artifacts: None,
                receipt: None,
                refusal: Some(b.refusal),
                refusal_detail: Some(b.detail),
            })
        }
    }
}
