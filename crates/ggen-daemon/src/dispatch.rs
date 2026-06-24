use std::{path::Path, sync::Arc};
use tokio::process::Command;
use tracing::{info, warn, instrument};
use serde::{Deserialize, Serialize};
use crate::{error::Result, state::DaemonState};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DispatchResult {
    pub dispatch_iri: String,
    pub spec_manifest: String,
    pub exit_code: i32,
    pub stdout_tail: String,
    pub stderr_tail: String,
    pub success: bool,
}

/// Invoke `ggen sync --spec <manifest>` as a subprocess.
/// Captures stdout/stderr (last 2 KB each) and records in DaemonState.
#[instrument(skip(state), fields(manifest = %spec_manifest))]
pub async fn dispatch_bundle(
    dispatch_iri: &str,
    spec_manifest: &str,
    working_dir: &Path,
    state: &Arc<DaemonState>,
) -> Result<DispatchResult> {
    info!("dispatching bundle {}", spec_manifest);
    let run_id = state.record_start(dispatch_iri, spec_manifest).await?;

    let output = Command::new("ggen")
        .args(["sync", "--spec", spec_manifest])
        .current_dir(working_dir)
        .output()
        .await
        .map_err(|e| crate::error::DaemonError::Io(e))?;

    let exit_code = output.status.code().unwrap_or(-1);
    let stdout_tail = tail_bytes(&output.stdout, 2048);
    let stderr_tail = tail_bytes(&output.stderr, 2048);
    let success = output.status.success();

    if success {
        info!("dispatch succeeded: exit={}", exit_code);
    } else {
        warn!("dispatch failed: exit={} stderr={}", exit_code, &stderr_tail[..stderr_tail.len().min(200)]);
    }

    state.record_finish(run_id, exit_code, &stdout_tail, &stderr_tail).await?;

    Ok(DispatchResult {
        dispatch_iri: dispatch_iri.to_owned(),
        spec_manifest: spec_manifest.to_owned(),
        exit_code,
        stdout_tail,
        stderr_tail,
        success,
    })
}

fn tail_bytes(bytes: &[u8], max: usize) -> String {
    let start = bytes.len().saturating_sub(max);
    String::from_utf8_lossy(&bytes[start..]).into_owned()
}
