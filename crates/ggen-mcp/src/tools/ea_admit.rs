//! `ggen_ea_admit`
use super::ea_common::{load_graph_from_root, parse_authority, parse_generation, refusal_body};
use crate::error::McpError;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};
use serde_json::Value;
#[derive(Debug, Deserialize, JsonSchema)]
pub struct EaAdmitParams {
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
}
#[derive(Debug, Serialize, JsonSchema)]
pub struct EaAdmitResult {
    pub ok: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub admitted: Option<Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub refusal: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub refusal_detail: Option<String>,
}
fn admitted_json(ad: &ggen_abb_sbb::Admitted) -> Value {
    serde_json::json!({"graph_digest": ad.graph_digest(), "sbb": ad.sbb().id, "sbb_digest": ad.sbb_digest()})
}
pub fn ea_admit(params: &EaAdmitParams) -> Result<EaAdmitResult, McpError> {
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
    match ggen_abb_sbb::admit(&g, &req) {
        Ok(ad) => Ok(EaAdmitResult {
            ok: true,
            admitted: Some(admitted_json(&ad)),
            refusal: None,
            refusal_detail: None,
        }),
        Err(refusal) => {
            let b = refusal_body(&refusal);
            Ok(EaAdmitResult {
                ok: false,
                admitted: None,
                refusal: Some(b.refusal),
                refusal_detail: Some(b.detail),
            })
        }
    }
}
