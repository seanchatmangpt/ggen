//! Compliance validation framework (FISMA, FedRAMP, SOC 2, HIPAA, 21 CFR Part 11, NIST 800-53, DFARS)

pub mod controls;
pub mod evidence;
pub mod frameworks;

pub use controls::{Control, ControlResult, ControlStatus};
pub use evidence::ComplianceEvidence;
pub use frameworks::{ComplianceFramework, ComplianceResult, FrameworkType};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_exports() {
        let _ = std::any::type_name::<ComplianceFramework>();
        let _ = std::any::type_name::<FrameworkType>();
    }
}
