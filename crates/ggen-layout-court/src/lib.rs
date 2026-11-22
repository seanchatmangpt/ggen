//! ggen-layout-court — CLI + lib subject for zcode-cli `UiLayoutCourtAdapter`.
//!
//! Exit taxonomy (matches adapter):
//! - 0 PASS — spatial / digest court admitted
//! - 1 FAIL_REFUSED — geometry or digest refusal
//! - 2 SUBJECT_MISSING — path absent, unreadable, or empty

#![forbid(unsafe_code)]

pub mod metrics;
pub mod text_spacing;

use std::fs;
use std::path::Path;

use ggen_graph::layout::taffy_aot::AotSealReceipt;
use ggen_graph::layout::ui_layout::{
    consume_aot_seal, run_layout_court_json, LayoutCourtCase, UiLayoutRefusal,
};
use serde::Deserialize;

use crate::metrics::{
    parse_product_subject, run_product_subject, ProductRefusal, VIEWPORTS,
};

pub const EXIT_PASS: i32 = 0;
pub const EXIT_REFUSED: i32 = 1;
pub const EXIT_SUBJECT_MISSING: i32 = 2;

#[derive(Debug, Clone, PartialEq)]
pub enum CourtStanding {
    Alive { kind: &'static str, detail: String },
    Refused { code: String, detail: String },
    SubjectMissing { detail: String },
}

impl CourtStanding {
    pub fn exit_code(&self) -> i32 {
        match self {
            Self::Alive { .. } => EXIT_PASS,
            Self::Refused { .. } => EXIT_REFUSED,
            Self::SubjectMissing { .. } => EXIT_SUBJECT_MISSING,
        }
    }
}

#[derive(Debug, Deserialize)]
struct SealConsumeFixture {
    seal: SealWire,
    pinned_digest: String,
}

#[derive(Debug, Deserialize)]
struct SealWire {
    text_spacing: f32,
    widths_px: Vec<u32>,
    leaf_count: usize,
    digest: String,
}

pub fn parse_viewports_csv(csv: Option<&str>) -> Vec<f32> {
    let raw = csv
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .map(|s| s.to_string())
        .or_else(|| std::env::var("UI_LAYOUT_VIEWPORTS").ok())
        .unwrap_or_default();
    if raw.is_empty() {
        return VIEWPORTS.to_vec();
    }
    let mut out = Vec::new();
    for part in raw.split(',') {
        let t = part.trim();
        if t.is_empty() {
            continue;
        }
        if let Ok(v) = t.parse::<f32>() {
            if v > 0.0 {
                out.push(v);
            }
        }
    }
    if out.is_empty() {
        VIEWPORTS.to_vec()
    } else {
        out
    }
}

pub fn load_subject_text(path: &Path) -> Result<String, CourtStanding> {
    if !path.exists() {
        return Err(CourtStanding::SubjectMissing {
            detail: format!("subject path missing: {}", path.display()),
        });
    }
    let text = fs::read_to_string(path).map_err(|e| CourtStanding::SubjectMissing {
        detail: format!("subject unreadable ({}): {e}", path.display()),
    })?;
    if text.trim().is_empty() {
        return Err(CourtStanding::SubjectMissing {
            detail: format!("subject empty: {}", path.display()),
        });
    }
    Ok(text)
}

pub fn run_subject_json(json: &str, viewports: &[f32]) -> CourtStanding {
    if serde_json::from_str::<LayoutCourtCase>(json).is_ok()
        || (json.contains("\"court\"")
            && json.contains("\"schema\"")
            && json.contains("\"column\""))
    {
        match run_layout_court_json(json) {
            Ok(receipt) => {
                return CourtStanding::Alive {
                    kind: "layout_court_case",
                    detail: format!("seal_digest={}", receipt.digest),
                };
            }
            Err(UiLayoutRefusal::Parse { .. }) => {}
            Err(e) => {
                return CourtStanding::Refused {
                    code: e.code().into(),
                    detail: format!("{e:?}"),
                };
            }
        }
    }

    run_product_or_seal(json, viewports)
}

fn run_product_or_seal(json: &str, viewports: &[f32]) -> CourtStanding {
    if let Ok(fix) = serde_json::from_str::<SealConsumeFixture>(json) {
        let seal = AotSealReceipt {
            text_spacing: fix.seal.text_spacing,
            widths_px: fix.seal.widths_px,
            leaf_count: fix.seal.leaf_count,
            digest: fix.seal.digest,
        };
        return match consume_aot_seal(Some(&seal), Some(fix.pinned_digest.as_str())) {
            Ok(receipt) => CourtStanding::Alive {
                kind: "aot_seal_consume",
                detail: format!("seal_digest={}", receipt.digest),
            },
            Err(e) => CourtStanding::Refused {
                code: e.code().into(),
                detail: format!("{e:?}"),
            },
        };
    }

    match parse_product_subject(json) {
        Ok(subject) => match run_product_subject(&subject, viewports) {
            Ok(receipt) => CourtStanding::Alive {
                kind: "product_subject",
                detail: format!(
                    "root={} nodes={} viewports={:?}",
                    receipt.root, receipt.node_count, receipt.viewports_px
                ),
            },
            Err(e) => CourtStanding::Refused {
                code: e.code().into(),
                detail: format!("{e:?}"),
            },
        },
        Err(ProductRefusal::Parse { message }) => CourtStanding::Refused {
            code: "REFUSED_LAYOUT_PARSE".into(),
            detail: message,
        },
        Err(e) => CourtStanding::Refused {
            code: e.code().into(),
            detail: format!("{e:?}"),
        },
    }
}

pub fn run_subject_path(path: &Path, viewports: &[f32]) -> CourtStanding {
    match load_subject_text(path) {
        Ok(text) => run_subject_json(&text, viewports),
        Err(standing) => standing,
    }
}

pub fn parse_cli_args(args: &[String]) -> Result<(std::path::PathBuf, Vec<f32>), CourtStanding> {
    let mut subject: Option<std::path::PathBuf> = None;
    let mut viewports_csv: Option<String> = None;
    let mut i = 0usize;
    while i < args.len() {
        let a = &args[i];
        if a == "--subject" || a == "-s" {
            let path = args.get(i + 1).ok_or_else(|| CourtStanding::SubjectMissing {
                detail: "layout_court requires <subject.json> after --subject/-s".into(),
            })?;
            subject = Some(std::path::PathBuf::from(path));
            i += 2;
            continue;
        }
        if a == "--viewports" {
            let csv = args.get(i + 1).ok_or_else(|| CourtStanding::Refused {
                code: "REFUSED_LAYOUT_PARSE".into(),
                detail: "--viewports requires a CSV value".into(),
            })?;
            viewports_csv = Some(csv.clone());
            i += 2;
            continue;
        }
        if a.starts_with('-') {
            return Err(CourtStanding::Refused {
                code: "REFUSED_LAYOUT_PARSE".into(),
                detail: format!("unknown flag: {a}"),
            });
        }
        if subject.is_none() {
            subject = Some(std::path::PathBuf::from(a));
        }
        i += 1;
    }

    let path = subject.ok_or_else(|| CourtStanding::SubjectMissing {
        detail: "layout_court requires <subject.json> after -- (full court JSON with metrics+spacing)"
            .into(),
    })?;
    Ok((path, parse_viewports_csv(viewports_csv.as_deref())))
}

#[cfg(test)]
mod unit {
    use super::*;
    use ggen_graph::layout::taffy_aot::{
        aot_seal_digest, preflight_column_six_widths, AotColumn, AotLeaf, WCAG_1412_TEXT_SPACING,
    };
    use ggen_graph::layout::ui_layout::{LAYOUT_COURT_NOUN, LAYOUT_COURT_SCHEMA_V1};

    fn green_column_json() -> String {
        serde_json::json!({
            "court": LAYOUT_COURT_NOUN,
            "schema": LAYOUT_COURT_SCHEMA_V1,
            "text_spacing": WCAG_1412_TEXT_SPACING,
            "column": {
                "gap_px": 8.0,
                "children": [
                    {
                        "id": "body",
                        "width_px": 200.0,
                        "height_px": 32.0,
                        "is_text": true,
                        "is_interactive": false
                    },
                    {
                        "id": "cta",
                        "width_px": 120.0,
                        "height_px": 44.0,
                        "is_text": false,
                        "is_interactive": true
                    }
                ]
            }
        })
        .to_string()
    }

    #[test]
    fn missing_subject_path_is_subject_missing_exit_2() {
        let standing = run_subject_path(Path::new("/tmp/ggen-layout-court-ABSENT.json"), VIEWPORTS);
        assert_eq!(standing.exit_code(), EXIT_SUBJECT_MISSING);
    }

    #[test]
    fn layout_court_case_admits_alive() {
        let standing = run_subject_json(&green_column_json(), VIEWPORTS);
        assert_eq!(standing.exit_code(), EXIT_PASS);
    }

    #[test]
    fn stale_seal_digest_refuses() {
        let column = AotColumn {
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
        };
        let seal = preflight_column_six_widths(&column, WCAG_1412_TEXT_SPACING)
            .expect("green column seals");
        let stale = aot_seal_digest(9.9, &seal.widths_px, seal.leaf_count);
        let json = serde_json::json!({
            "seal": {
                "text_spacing": seal.text_spacing,
                "widths_px": seal.widths_px,
                "leaf_count": seal.leaf_count,
                "digest": stale
            },
            "pinned_digest": stale
        })
        .to_string();
        let standing = run_subject_json(&json, VIEWPORTS);
        assert_eq!(standing.exit_code(), EXIT_REFUSED);
    }

    #[test]
    fn cli_without_subject_is_subject_missing() {
        let err = parse_cli_args(&[]).expect_err("empty argv");
        assert_eq!(err.exit_code(), EXIT_SUBJECT_MISSING);
    }
}
