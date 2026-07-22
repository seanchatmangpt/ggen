//! Generated from RDF certificate-kind individuals. Do not edit.

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CertificateKind {
    Admission,
    Plan,
    Projection,
    Authority,
    Environment,
    OpenReceipt,
    CloseReceipt,
    Replay,
    ConsequenceAdmission,
    StandingClosure,
}

pub const ALL: &[CertificateKind] = &[
    CertificateKind::Admission,
    CertificateKind::Plan,
    CertificateKind::Projection,
    CertificateKind::Authority,
    CertificateKind::Environment,
    CertificateKind::OpenReceipt,
    CertificateKind::CloseReceipt,
    CertificateKind::Replay,
    CertificateKind::ConsequenceAdmission,
    CertificateKind::StandingClosure,
];

pub const LABELS: &[(&str, &str)] = &[
    ("Admission", "Admission certificate"),
    ("Plan", "Plan certificate"),
    ("Projection", "Projection certificate"),
    ("Authority", "Authority certificate"),
    ("Environment", "Environment certificate"),
    ("OpenReceipt", "Open receipt"),
    ("CloseReceipt", "Close receipt"),
    ("Replay", "Replay certificate"),
    ("ConsequenceAdmission", "Consequence admission certificate"),
    ("StandingClosure", "Standing closure certificate"),
];
