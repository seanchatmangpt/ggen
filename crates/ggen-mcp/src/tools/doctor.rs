use ggen_engine::verbs::handlers::handle_doctor_in;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::{ErrorCategory, McpError};
use crate::project_root::resolve_root;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct DoctorParams {
    pub root: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct DoctorResult {
    pub ok: bool,
    pub healthy: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checks: Option<serde_json::Value>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    pub unsupported: Vec<&'static str>,
    pub escalate_to: Vec<&'static str>,
}

pub fn doctor(params: &DoctorParams) -> Result<DoctorResult, McpError> {
    let root = resolve_root(&params.root)?;
    match handle_doctor_in(&root) {
        Ok(report) => {
            let healthy = report
                .get("healthy")
                .and_then(serde_json::Value::as_bool)
                .unwrap_or(false);
            Ok(DoctorResult {
                ok: healthy,
                healthy,
                checks: report.get("checks").cloned(),
                summary: (!healthy).then(|| failure_summary(&report)),
                unsupported: vec!["doctor inspect", "doctor domain"],
                escalate_to: if healthy {
                    vec![]
                } else {
                    vec![
                        "ggen_sync_dry_run",
                        "ggen_receipt_verify",
                        "ggen_check_project",
                    ]
                },
            })
        }
        Err(e) => Err(McpError::new(ErrorCategory::GraphLoadError, e.to_string())),
    }
}

fn failure_summary(report: &serde_json::Value) -> String {
    let Some(checks) = report.get("checks").and_then(serde_json::Value::as_object) else {
        return "doctor reported unhealthy".to_string();
    };
    let parts: Vec<String> = checks
        .iter()
        .filter_map(|(n, c)| {
            (c.get("status").and_then(serde_json::Value::as_str) == Some("fail")).then(|| {
                format!(
                    "{n}: {}",
                    c.get("detail")
                        .and_then(serde_json::Value::as_str)
                        .unwrap_or("")
                )
            })
        })
        .collect();
    if parts.is_empty() {
        "doctor reported unhealthy".to_string()
    } else {
        format!("doctor failing checks: {}", parts.join("; "))
    }
}
