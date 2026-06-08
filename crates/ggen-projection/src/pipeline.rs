use crate::mapping::{CustomizationMap, ProjectionMap};
use crate::receipt::ReceiptIndex;
use std::path::{Path, PathBuf};

pub struct StagingGate {
    pub output_dir: PathBuf,
    pub receipt_index: ReceiptIndex,
}

impl StagingGate {
    pub fn new(output_dir: PathBuf, receipt_index: ReceiptIndex) -> Self {
        Self {
            output_dir,
            receipt_index,
        }
    }

    pub fn check_write(&self, rel_path: &Path, force: bool) -> Result<(), anyhow::Error> {
        if force {
            return Ok(());
        }
        let full_path = self.output_dir.join(rel_path);
        if !full_path.exists() {
            return Ok(());
        }
        let content = std::fs::read(&full_path)?;
        let current_hash = blake3::hash(&content).to_hex().to_string();
        let key = rel_path.to_string_lossy().into_owned();
        if let Some(receipt) = self.receipt_index.receipts.get(&key) {
            if receipt.blake3_hash != current_hash {
                return Err(anyhow::anyhow!(
                    "Staging gate refusal: local edits in {:?} are dirty",
                    rel_path
                ));
            }
        } else {
            return Err(anyhow::anyhow!(
                "Staging gate refusal: untracked file {:?} is dirty",
                rel_path
            ));
        }
        Ok(())
    }
}

pub fn sync(
    output_dir: &Path, proj_map: &ProjectionMap, cust_map: &CustomizationMap,
    receipts: &ReceiptIndex,
) -> Result<(), anyhow::Error> {
    std::fs::create_dir_all(output_dir)?;

    let pm_path = output_dir.join("projection-map.json");
    std::fs::write(&pm_path, serde_json::to_string_pretty(proj_map)?)?;

    let cm_path = output_dir.join("customization-map.json");
    std::fs::write(&cm_path, serde_json::to_string_pretty(cust_map)?)?;

    let r_path = output_dir.join("receipts.json");
    std::fs::write(&r_path, serde_json::to_string_pretty(receipts)?)?;

    let marker_path = output_dir.join(".sync_marker");
    std::fs::write(&marker_path, b"sync_active")?;
    Ok(())
}
