//! Path-level enforcement for writes that can weaken or bypass evidence policy.
//! This policy is deliberately structural: it classifies the target before content
//! inspection so adapters can share one exact decision surface.
use super::Violation;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PathClass {
    PythonSubject,
    Workflow,
    HookConfig,
    ProjectConfig,
    RustManifest,
    GeneratedProjection,
    Receipt,
    Ordinary,
}

pub fn classify(path: &str) -> PathClass {
    let p = path.replace('\\', "/");
    if p.contains("/generated/") || p.starts_with("generated/") || p.contains("/gen/") {
        return PathClass::GeneratedProjection;
    }
    if p.contains("receipts/") || p.contains("/ocel/") { return PathClass::Receipt; }
    if (p.ends_with(".yml") || p.ends_with(".yaml")) && p.contains(".github/workflows/") {
        return PathClass::Workflow;
    }
    if p.ends_with(".pre-commit-config.yaml") { return PathClass::HookConfig; }
    if p.ends_with("pyproject.toml") { return PathClass::ProjectConfig; }
    if p.ends_with("Cargo.toml") { return PathClass::RustManifest; }
    if p.ends_with(".py") && (p.contains("src/") || p.contains("tests/")) {
        return PathClass::PythonSubject;
    }
    PathClass::Ordinary
}

pub fn check_write(path: &str, content: &str) -> Vec<Violation> {
    let class = classify(path);
    let mut out = Vec::new();
    if class == PathClass::GeneratedProjection {
        out.push(Violation {
            pattern: "generated projection manual write".into(),
            location: path.into(),
            rule: "Generated projections must be rematerialized from their canonical source, not hand-edited.".into(),
        });
    }
    if matches!(class, PathClass::Workflow | PathClass::HookConfig | PathClass::ProjectConfig) {
        for needle in ["continue-on-error: true", "|| true", "SKIP_TRUTH_GATE", "disable_truth_gate", "fail-fast: false"] {
            if content.contains(needle) {
                out.push(Violation {
                    pattern: needle.into(),
                    location: path.into(),
                    rule: "Control-plane configuration may not weaken truth-gate enforcement.".into(),
                });
            }
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test] fn generated_is_fenced() {
        assert_eq!(classify("ontology/generated/runtime.ttl"), PathClass::GeneratedProjection);
        assert!(!check_write("ontology/generated/runtime.ttl", "x").is_empty());
    }
    #[test] fn receipts_are_distinct() {
        assert_eq!(classify("receipts/run.json"), PathClass::Receipt);
    }
    #[test] fn workflow_bypass_is_rejected() {
        assert!(!check_write(".github/workflows/ci.yml", "continue-on-error: true").is_empty());
    }
}
