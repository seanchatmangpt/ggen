//! ui-layout-court adapter (ECOSYSTEM-SPEC-27.1.1 Gate 1 D5).
//!
//! **zcode-cli** owns the live `ui-layout-court.ts` runner and is **out of this
//! workspace** (live TypeScript court standing: `UNSUPPORTED` / `BLOCKED`).
//! This module is the **ggen-side contract surface**: it loads layout-node
//! fixtures in the shape zcode-cli would hand across and feeds them into
//! [`crate::layout::taffy_aot`] without re-implementing geometry law.
//!
//! Photolithography stays non-crown-`ALIVE` until a mounted zcode-cli (or
//! igniter) executes against an `AotSealReceipt`. Until then this adapter +
//! Chicago court prove the in-repo seam: nodes → Taffy AOT → typed
//! [`LayoutRefusal`] admit/refuse.

use serde::{Deserialize, Serialize};

use super::taffy_aot::{
    preflight_column_six_widths, AotColumn, AotSealReceipt, LayoutRefusal, WCAG_1412_TEXT_SPACING,
};

/// Schema id zcode-cli / fixtures must declare for Gate-1 column trees.
pub const LAYOUT_COURT_SCHEMA_V1: &str = "ggen.layout.aot.column/v1";

/// Court noun mirrored from zcode-cli `ui-layout-court.ts`.
pub const LAYOUT_COURT_NOUN: &str = "ui-layout-court";

/// Wire fixture: layout nodes + optional expected seal digest.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LayoutCourtCase {
    pub court: String,
    pub schema: String,
    /// WCAG 1.4.12 spacing factor; Gate-1 courts use 1.5.
    pub text_spacing: f32,
    pub column: AotColumn,
    /// When present, must match the blake3 hex digest of the seal receipt.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub expected_seal_digest: Option<String>,
}

/// Admit receipt bound to a seal digest (consumable by zcode-cli).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LayoutCourtReceipt {
    pub seal: AotSealReceipt,
    /// Lowercase hex blake3 over the canonical seal payload.
    pub seal_digest: String,
}

/// Typed refusals for the ui-layout-court adapter (fail-closed).
#[derive(Debug, Clone, PartialEq)]
pub enum LayoutCourtRefusal {
    /// Geometry law refused via Taffy AOT ([`LayoutRefusal`]: overflow, overlap, touch, …).
    Geometry(LayoutRefusal),
    SchemaUnsupported { schema: String },
    CourtMismatch { court: String },
    DigestMismatch { expected: String, actual: String },
    Parse { message: String },
}

/// Standing for one court run — never silent-pass on broken geometry.
#[derive(Debug, Clone, PartialEq)]
pub enum LayoutCourtStanding {
    Admitted(LayoutCourtReceipt),
    Refused(LayoutCourtRefusal),
}

impl LayoutCourtStanding {
    pub fn is_admitted(&self) -> bool {
        matches!(self, Self::Admitted(_))
    }
    pub fn is_refused(&self) -> bool {
        matches!(self, Self::Refused(_))
    }
}

pub fn parse_layout_court_case(json: &str) -> Result<LayoutCourtCase, LayoutCourtRefusal> {
    serde_json::from_str(json).map_err(|e| LayoutCourtRefusal::Parse {
        message: e.to_string(),
    })
}

pub fn seal_digest(seal: &AotSealReceipt) -> String {
    let payload = format!(
        "v1|spacing={:.6}|widths={}|leaves={}",
        seal.text_spacing,
        seal.widths_px.iter().map(|w| w.to_string()).collect::<Vec<_>>().join(","),
        seal.leaf_count
    );
    hex::encode(blake3::hash(payload.as_bytes()).as_bytes())
}

pub fn run_layout_court(case: &LayoutCourtCase) -> LayoutCourtStanding {
    if case.court != LAYOUT_COURT_NOUN {
        return LayoutCourtStanding::Refused(LayoutCourtRefusal::CourtMismatch {
            court: case.court.clone(),
        });
    }
    if case.schema != LAYOUT_COURT_SCHEMA_V1 {
        return LayoutCourtStanding::Refused(LayoutCourtRefusal::SchemaUnsupported {
            schema: case.schema.clone(),
        });
    }
    let seal = match preflight_column_six_widths(&case.column, case.text_spacing) {
        Ok(seal) => seal,
        Err(refusal) => {
            return LayoutCourtStanding::Refused(LayoutCourtRefusal::Geometry(refusal));
        }
    };
    let digest = seal_digest(&seal);
    if let Some(expected) = case.expected_seal_digest.as_deref() {
        if expected != digest {
            return LayoutCourtStanding::Refused(LayoutCourtRefusal::DigestMismatch {
                expected: expected.to_string(),
                actual: digest,
            });
        }
    }
    LayoutCourtStanding::Admitted(LayoutCourtReceipt { seal, seal_digest: digest })
}

pub fn run_layout_court_json(json: &str) -> LayoutCourtStanding {
    match parse_layout_court_case(json) {
        Ok(case) => run_layout_court(&case),
        Err(refusal) => LayoutCourtStanding::Refused(refusal),
    }
}

pub fn gate1_text_spacing() -> f32 {
    WCAG_1412_TEXT_SPACING
}

#[cfg(test)]
mod unit {
    use super::*;
    use crate::layout::taffy_aot::AotLeaf;

    #[test]
    fn unit_valid_column_admits_under_gate1_spacing() {
        let case = LayoutCourtCase {
            court: LAYOUT_COURT_NOUN.into(),
            schema: LAYOUT_COURT_SCHEMA_V1.into(),
            text_spacing: WCAG_1412_TEXT_SPACING,
            column: AotColumn {
                gap_px: 8.0,
                children: vec![
                    AotLeaf {
                        id: "body".into(),
                        width_px: 200.0,
                        height_px: 32.0,
                        is_text: true,
                        is_interactive: false,
                    },
                    AotLeaf {
                        id: "cta".into(),
                        width_px: 120.0,
                        height_px: 44.0,
                        is_text: false,
                        is_interactive: true,
                    },
                ],
            },
            expected_seal_digest: None,
        };
        assert!(run_layout_court(&case).is_admitted());
    }
}
