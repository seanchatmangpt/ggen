#![forbid(unsafe_code)]

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Standing {
    Admitted,
    Refused,
    Generated,
    Verified,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ChapterArtifact {
    pub chapter: u16,
    pub title: &'static str,
    pub standing: Standing,
    pub evidence: &'static [&'static str],
}

pub const ARTIFACT: ChapterArtifact = ChapterArtifact {
    chapter: 0,
    title: "Appendix A — Canonical Pack Directory Layout",
    standing: Standing::Verified,
    evidence: &["shape", "consumer", "proof", "idempotency", "receipt"],
};

#[test]
fn chapter_artifact_has_non_vacuous_evidence() {
    assert!(ARTIFACT.evidence.len() >= 5);
    assert_eq!(ARTIFACT.chapter, 0);
}
