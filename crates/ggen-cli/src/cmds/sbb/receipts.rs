use super::*;

impl Receipt {
    fn issue(
        operation: &str, report: &Report, previous: &str, artifacts: Vec<String>,
    ) -> Result<Self> {
        let body = ReceiptBody {
            schema: RECEIPT_SCHEMA,
            operation,
            manifest_digest: &report.manifest_digest,
            report_digest: &report.report_digest,
            previous_digest: previous,
            artifacts: &artifacts,
        };
        Ok(Self {
            schema: RECEIPT_SCHEMA.to_string(),
            operation: operation.to_string(),
            manifest_digest: report.manifest_digest.clone(),
            report_digest: report.report_digest.clone(),
            previous_digest: previous.to_string(),
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

fn receipt_paths(output: &Path) -> (PathBuf, PathBuf, PathBuf) {
    (
        output.join("density-report.json"),
        output.join("density-intent.json"),
        output.join("density-result.json"),
    )
}

fn previous_digest(path: &Path) -> Result<String> {
    if !path.is_file() {
        return Ok("GENESIS".to_string());
    }
    let receipt: Receipt = serde_json::from_slice(&fs::read(path).map_err(|error| {
        NounVerbError::execution_error(format!("cannot read {}: {error}", path.display()))
    })?)
    .map_err(|error| {
        NounVerbError::execution_error(format!("cannot parse {}: {error}", path.display()))
    })?;
    if !receipt.valid()? {
        return Err(NounVerbError::execution_error(format!(
            "existing receipt {} is invalid",
            path.display()
        )));
    }
    Ok(receipt.digest)
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

pub(super) fn issue(manifest: &Path, output: &Path) -> Result<Value> {
    let report = evaluation::evaluate(manifest)?;
    let (report_path, intent_path, result_path) = receipt_paths(output);
    let intent = Receipt::issue(
        "density-evaluate-intent",
        &report,
        &previous_digest(&result_path)?,
        vec!["density-report.json".to_string()],
    )?;
    write_json(&intent_path, &intent)?;
    write_json(&report_path, &report)?;
    let result = Receipt::issue(
        "density-evaluate-result",
        &report,
        &intent.digest,
        vec![
            "density-report.json".to_string(),
            "density-intent.json".to_string(),
        ],
    )?;
    write_json(&result_path, &result)?;
    Ok(json!({
        "standing": report.standing,
        "claim_ceiling": report.claim_ceiling,
        "report": report_path,
        "intent_receipt": intent_path,
        "result_receipt": result_path,
        "receipt_digest": result.digest
    }))
}

pub(super) fn replay(manifest: &Path, output: &Path) -> Result<Value> {
    let report = evaluation::evaluate(manifest)?;
    let (report_path, intent_path, result_path) = receipt_paths(output);
    let stored: Report = read_json(&report_path)?;
    let intent: Receipt = read_json(&intent_path)?;
    let result: Receipt = read_json(&result_path)?;
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
        "schema": "ggen.sbb.capability-density-replay.v1",
        "status": if matches { "REPLAY_MATCH" } else { "REPLAY_DIVERGED" },
        "matches": matches,
        "manifest_digest": report.manifest_digest,
        "report_digest": report.report_digest,
        "receipt_digest": result.digest
    }))
}
