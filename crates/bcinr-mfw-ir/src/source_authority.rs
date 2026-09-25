//! Projected-ephemeral source-authority contract.
use crate::Digest;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProjectionBinding {
    pub semantic_source: Digest,
    pub manufacturer: Digest,
    pub projection_profile: Digest,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProjectionEvidence {
    pub binding: ProjectionBinding,
    pub artifact: Digest,
    pub regenerated_artifact: Digest,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ProjectionRefusal {
    SemanticSourceMismatch,
    ManufacturerMismatch,
    ProjectionProfileMismatch,
    ArtifactDrift,
}

pub fn admit_projection(expected: ProjectionBinding, observed: ProjectionEvidence) -> Result<Digest, ProjectionRefusal> {
    if observed.binding.semantic_source != expected.semantic_source { return Err(ProjectionRefusal::SemanticSourceMismatch); }
    if observed.binding.manufacturer != expected.manufacturer { return Err(ProjectionRefusal::ManufacturerMismatch); }
    if observed.binding.projection_profile != expected.projection_profile { return Err(ProjectionRefusal::ProjectionProfileMismatch); }
    if observed.artifact != observed.regenerated_artifact { return Err(ProjectionRefusal::ArtifactDrift); }
    Ok(observed.artifact)
}

#[cfg(test)]
mod tests {
    use super::*;
    fn d(s: &str) -> Digest { Digest::hash(s.as_bytes()) }
    fn binding() -> ProjectionBinding { ProjectionBinding { semantic_source: d("source"), manufacturer: d("manufacturer"), projection_profile: d("profile") } }

    #[test]
    fn exact_projection_is_admitted() {
        let b = binding(); let a = d("artifact");
        assert_eq!(admit_projection(b, ProjectionEvidence { binding: b, artifact: a, regenerated_artifact: a }), Ok(a));
    }

    #[test]
    fn edited_projection_is_refused() {
        let b = binding();
        assert_eq!(admit_projection(b, ProjectionEvidence { binding: b, artifact: d("edited"), regenerated_artifact: d("generated") }), Err(ProjectionRefusal::ArtifactDrift));
    }

    #[test]
    fn substituted_source_is_refused() {
        let b = binding(); let a = d("artifact");
        let changed = ProjectionBinding { semantic_source: d("other"), ..b };
        assert_eq!(admit_projection(b, ProjectionEvidence { binding: changed, artifact: a, regenerated_artifact: a }), Err(ProjectionRefusal::SemanticSourceMismatch));
    }
}
