use super::*;

impl ProgramReceipt {
    fn issue(
        operation: &str, report: &Report, previous_digest: &str, artifacts: Vec<String>,
    ) -> Result<Self> {
        let body = ReceiptBody {
            schema: RECEIPT_SCHEMA,
            operation,
            manifest_digest: &report.manifest_digest,
            report_digest: &report.report_digest,
            previous_digest,
            artifacts: &artifacts,
        };
        Ok(Self {
            schema: RECEIPT_SCHEMA.to_string(),
            operation: operation.to_string(),
            manifest_digest: report.manifest_digest.clone(),
            report_digest: report.report_digest.clone(),
            previous_digest: previous_digest.to_string(),
            artifacts,
            digest_algorithm: "blake3".to_string(),
            digest: digest_json(&body)?,
        })
    }

    fn valid(&self) -> Result<bool> {
        Ok(self.schema == RECEIPT_SCHEMA
            && self.digest_algorithm == "blake3"
            && self.digest
                == digest_json(&ReceiptBody {
                    schema: RECEIPT_SCHEMA,
                    operation: &self.operation,
                    manifest_digest: &self.manifest_digest,
                    report_digest: &self.report_digest,
                    previous_digest: &self.previous_digest,
                    artifacts: &self.artifacts,
                })?)
    }
}

fn paths(output: &Path) -> (PathBuf, PathBuf, PathBuf) {
    (
        output.join("vision-2030-report.json"),
        output.join("vision-2030-intent.json"),
        output.join("vision-2030-result.json"),
    )
}

fn write_json<T: Serialize>(path: &Path, value: &T) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|error| {
            NounVerbError::execution_error(format!("cannot create {}: {error}", parent.display()))
        })?;
    }
    let temporary = path.with_extension("tmp");
    let bytes = serde_json::to_vec_pretty(value).map_err(|error| {
        NounVerbError::execution_error(format!("cannot serialize {}: {error}", path.display()))
    })?;
    fs::write(&temporary, bytes).map_err(|error| {
        NounVerbError::execution_error(format!("cannot write {}: {error}", temporary.display()))
    })?;
    fs::rename(&temporary, path).map_err(|error| {
        NounVerbError::execution_error(format!("cannot replace {}: {error}", path.display()))
    })
}

fn read_json<T>(path: &Path) -> Result<T>
where
    T: for<'de> Deserialize<'de>,
{
    let bytes = fs::read(path).map_err(|error| {
        NounVerbError::execution_error(format!("cannot read {}: {error}", path.display()))
    })?;
    serde_json::from_slice(&bytes).map_err(|error| {
        NounVerbError::execution_error(format!("cannot parse {}: {error}", path.display()))
    })
}

fn previous_digest(path: &Path) -> Result<String> {
    if !path.is_file() {
        return Ok("GENESIS".to_string());
    }
    let receipt: ProgramReceipt = read_json(path)?;
    if !receipt.valid()? {
        return Err(NounVerbError::execution_error(format!(
            "existing receipt {} is invalid",
            path.display()
        )));
    }
    Ok(receipt.digest)
}

pub(super) fn issue(manifest: &Path, output: &Path) -> Result<Value> {
    let report = evaluation::evaluate(manifest)?;
    let (report_path, intent_path, result_path) = paths(output);
    let intent = ProgramReceipt::issue(
        "vision-2030-evaluate-intent",
        &report,
        &previous_digest(&result_path)?,
        vec!["vision-2030-report.json".to_string()],
    )?;
    write_json(&intent_path, &intent)?;
    write_json(&report_path, &report)?;
    let result = ProgramReceipt::issue(
        "vision-2030-evaluate-result",
        &report,
        &intent.digest,
        vec![
            "vision-2030-report.json".to_string(),
            "vision-2030-intent.json".to_string(),
        ],
    )?;
    write_json(&result_path, &result)?;
    Ok(json!({
        "standing": report.standing,
        "achieved": report.achieved,
        "phase_change_multiplier": report.phase_change_multiplier,
        "report": report_path,
        "intent_receipt": intent_path,
        "result_receipt": result_path,
        "receipt_digest": result.digest
    }))
}

pub(super) fn replay(manifest: &Path, output: &Path) -> Result<Value> {
    let report = evaluation::evaluate(manifest)?;
    let (report_path, intent_path, result_path) = paths(output);
    let stored: Report = read_json(&report_path)?;
    let intent: ProgramReceipt = read_json(&intent_path)?;
    let result: ProgramReceipt = read_json(&result_path)?;
    let matches = report_digest(&stored)? == stored.report_digest
        && intent.valid()?
        && result.valid()?
        && result.previous_digest == intent.digest
        && intent.manifest_digest == report.manifest_digest
        && intent.report_digest == report.report_digest
        && stored.manifest_digest == report.manifest_digest
        && stored.report_digest == report.report_digest
        && result.manifest_digest == report.manifest_digest
        && result.report_digest == report.report_digest;
    Ok(json!({
        "schema": "ggen.vision2030.replay.v1",
        "status": if matches { "REPLAY_MATCH" } else { "REPLAY_DIVERGED" },
        "matches": matches,
        "manifest_digest": report.manifest_digest,
        "report_digest": report.report_digest,
        "receipt_digest": result.digest
    }))
}
