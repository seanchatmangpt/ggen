use super::*;

impl Receipt {
    pub(super) fn issue(
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

    pub(super) fn valid(&self) -> Result<bool> {
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

pub(super) fn write_json<T: Serialize>(path: &Path, value: &T) -> Result<()> {
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

pub(super) fn receipt_paths(output: &Path) -> (PathBuf, PathBuf, PathBuf) {
    (
        output.join("density-report.json"),
        output.join("density-intent.json"),
        output.join("density-result.json"),
    )
}

pub(super) fn previous_digest(path: &Path) -> Result<String> {
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
