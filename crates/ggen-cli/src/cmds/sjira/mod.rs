//! Semantic Jira noun — the CLI surface over `ggen-sjira`'s admission
//! kernel (native port of the ggen_igniter `SemanticJira` core slice).
//!
//! One verb this slice: `admit` — read a WorkOrder JSON file, admit it
//! through `ggen_sjira::admit_work_order`, and print the admitted order
//! with its stamped authority and derived digests, or the typed refusal
//! with remediation text.

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use serde_json::Value;
use std::path::PathBuf;

/// `ggen sjira admit <file>`: admit one WorkOrder JSON file without
/// creating authority. The error path carries the kernel's typed refusal
/// Display (including remediation text).
#[verb]
pub fn admit(
    #[arg(index = 1)] file: PathBuf,
) -> Result<Value> {
    let bytes = std::fs::read(&file).map_err(|e| {
        NounVerbError::argument_error(format!(
            "cannot read work order {}: {e}",
            file.display()
        ))
    })?;

    let raw: Value = serde_json::from_slice(&bytes).map_err(|e| {
        NounVerbError::argument_error(format!(
            "not valid JSON in {}: {e}",
            file.display()
        ))
    })?;

    match ggen_sjira::admit_work_order(&raw) {
        Ok(admitted) => Ok(serde_json::json!({
            "admitted": true,
            "authority": admitted.authority,
            "work_order_digest": admitted.work_order_digest,
            "definition_digest": admitted.definition_digest,
            "identity": admitted.work_order.identity,
            "standing": admitted.work_order.standing.as_str(),
        })),
        Err(refusal) => Err(NounVerbError::argument_error(format!(
            "REFUSED:SEMANTIC_JIRA: {refusal}"
        ))),
    }
}
