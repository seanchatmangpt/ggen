//! Provenance binding for source-law behavior reconstructed through `ggen-legacy`.
//!
//! `ggen-legacy` is evidence authority, not a runtime dependency. The executable
//! path remains `ggen` + `lsp-max`; these coordinates make the observed legacy
//! contract replayable without importing the predecessor into production.

use lsp_max::lsp_types::{Diagnostic, NumberOrString};
use serde_json::{json, Value};

/// Contract schema carried in every GGEN-SRC diagnostic.
pub const CONTRACT_SCHEMA: &str = "ggen.lsp.source-law.contract.v1";
/// Immutable `ggen-legacy` repository head inspected for this reconstruction.
pub const GGEN_LEGACY_HEAD: &str = "70e599a599fedb7c62c965377cc2f80df1fa01ec";
/// Stable predecessor coordinate admitted by `ggen-legacy` Project 001.
pub const LEGACY_GGEN_COORDINATE: &str = "0f39227c102e0ac7519f0f27561356227a518653";
/// `lsp-max` package family selected by the `ggen` lockfile.
pub const LSP_MAX_PACKAGE_VERSION: &str = "26.7.3";
/// Human-readable contract identity.
pub const CONTRACT_ID: &str = "ggen-legacy:GGEN-SRC";

/// Attach source, authority, and runtime identities to an LSP diagnostic.
///
/// Existing analyzer data is preserved under `prior_data`; provenance never
/// overwrites a more specific payload.
pub fn attach(diagnostic: &mut Diagnostic) {
    let code = match diagnostic.code.as_ref() {
        Some(NumberOrString::String(value)) => value.clone(),
        Some(NumberOrString::Number(value)) => value.to_string(),
        None => "GGEN-UNKNOWN".to_string(),
    };
    let prior_data = diagnostic.data.take();
    diagnostic.data = Some(json!({
        "schema": CONTRACT_SCHEMA,
        "contract_id": CONTRACT_ID,
        "diagnostic_code": code,
        "authority": {
            "product": "ggen",
            "legacy_evidence_repository": "seanchatmangpt/ggen-legacy",
            "legacy_evidence_head": GGEN_LEGACY_HEAD,
            "legacy_ggen_coordinate": LEGACY_GGEN_COORDINATE
        },
        "runtime": {
            "package": "lsp-max",
            "version": LSP_MAX_PACKAGE_VERSION
        },
        "prior_data": prior_data
    }));
}

/// Return the attached provenance object when it matches this contract schema.
#[must_use]
pub fn provenance(diagnostic: &Diagnostic) -> Option<&Value> {
    let data = diagnostic.data.as_ref()?;
    (data.get("schema")?.as_str()? == CONTRACT_SCHEMA).then_some(data)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn attaches_all_three_repository_roles() {
        let mut diagnostic = Diagnostic {
            code: Some(NumberOrString::String("GGEN-SRC-004".to_string())),
            ..Diagnostic::default()
        };
        attach(&mut diagnostic);
        let data = provenance(&diagnostic).expect("contract provenance");
        assert_eq!(data["authority"]["product"], "ggen");
        assert_eq!(
            data["authority"]["legacy_evidence_repository"],
            "seanchatmangpt/ggen-legacy"
        );
        assert_eq!(data["runtime"]["package"], "lsp-max");
        assert_eq!(data["runtime"]["version"], LSP_MAX_PACKAGE_VERSION);
    }

    #[test]
    fn preserves_prior_analyzer_data() {
        let mut diagnostic = Diagnostic {
            data: Some(json!({"route": "repair-1"})),
            ..Diagnostic::default()
        };
        attach(&mut diagnostic);
        assert_eq!(
            provenance(&diagnostic).expect("contract")["prior_data"]["route"],
            "repair-1"
        );
    }
}
