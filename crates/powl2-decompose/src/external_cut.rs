use serde::{Deserialize, Serialize};

use crate::powl::Powl;

/// Stable typed refusals for External Cuts.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ExternalCutRefusal {
    /// A required external cut is missing from the geometry.
    ExternalCutUndeclared,
    /// The external cut references an invalid type.
    ExternalCutTypeMismatch {
        /// Expected type description.
        expected: String,
        /// Actual type description.
        actual: String,
    },
    /// The external cut violates authority boundaries.
    ExternalCutAuthorityMismatch,
    /// The inner POWL region of the external cut is not admitted.
    PowlRegionNotAdmitted,
}

impl std::fmt::Display for ExternalCutRefusal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExternalCutUndeclared => write!(f, "EXTERNAL_CUT_UNDECLARED"),
            Self::ExternalCutTypeMismatch { expected, actual } => {
                write!(
                    f,
                    "EXTERNAL_CUT_TYPE_MISMATCH: expected {}, found {}",
                    expected, actual
                )
            }
            Self::ExternalCutAuthorityMismatch => write!(f, "EXTERNAL_CUT_AUTHORITY_MISMATCH"),
            Self::PowlRegionNotAdmitted => write!(f, "POWL_REGION_NOT_ADMITTED"),
        }
    }
}

impl std::error::Error for ExternalCutRefusal {}

/// Validates that an external cut identifies a valid POWL region whose boundary leaves the current process cell.
pub fn validate_external_cut(cut: &Powl) -> Result<(), ExternalCutRefusal> {
    match cut {
        Powl::ExternalCut {
            region,
            projection,
            renderer,
        } => {
            if projection.is_empty() || renderer.is_empty() {
                return Err(ExternalCutRefusal::ExternalCutUndeclared);
            }

            // We must ensure that the region is admitted.
            validate_region(region)?;

            Ok(())
        }
        _ => Err(ExternalCutRefusal::ExternalCutTypeMismatch {
            expected: "ExternalCut".to_string(),
            actual: "Other POWL Variant".to_string(),
        }),
    }
}

fn validate_region(region: &Powl) -> Result<(), ExternalCutRefusal> {
    match region {
        Powl::Leaf(_) => Ok(()),
        Powl::PartialOrder { children, .. } => {
            if children.is_empty() {
                return Err(ExternalCutRefusal::PowlRegionNotAdmitted);
            }
            for child in children {
                validate_region(child)?;
            }
            Ok(())
        }
        Powl::Choice { children, .. } => {
            if children.is_empty() {
                return Err(ExternalCutRefusal::PowlRegionNotAdmitted);
            }
            for child in children {
                validate_region(child)?;
            }
            Ok(())
        }
        Powl::ExternalCut { .. } => {
            // Nested external cuts inside an external cut region are not directly admitted
            // without explicit authority bounds resolution.
            Err(ExternalCutRefusal::PowlRegionNotAdmitted)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_external_cut() {
        let leaf = Powl::Leaf(Some("task1".to_string()));
        let cut = Powl::ExternalCut {
            region: Box::new(leaf),
            projection: "SELECT * { ?s ?p ?o }".to_string(),
            renderer: "tera_template_x".to_string(),
        };

        assert_eq!(validate_external_cut(&cut), Ok(()));
    }

    #[test]
    fn test_missing_projection() {
        let leaf = Powl::Leaf(Some("task1".to_string()));
        let cut = Powl::ExternalCut {
            region: Box::new(leaf),
            projection: "".to_string(),
            renderer: "tera_template_x".to_string(),
        };

        assert_eq!(
            validate_external_cut(&cut),
            Err(ExternalCutRefusal::ExternalCutUndeclared)
        );
    }

    #[test]
    fn test_nested_external_cut_rejected() {
        let inner_cut = Powl::ExternalCut {
            region: Box::new(Powl::Leaf(Some("task".to_string()))),
            projection: "q".to_string(),
            renderer: "t".to_string(),
        };
        let outer_cut = Powl::ExternalCut {
            region: Box::new(inner_cut),
            projection: "q".to_string(),
            renderer: "t".to_string(),
        };

        assert_eq!(
            validate_external_cut(&outer_cut),
            Err(ExternalCutRefusal::PowlRegionNotAdmitted)
        );
    }

    #[test]
    fn test_wrong_type() {
        let leaf = Powl::Leaf(Some("task1".to_string()));
        assert_eq!(
            validate_external_cut(&leaf),
            Err(ExternalCutRefusal::ExternalCutTypeMismatch {
                expected: "ExternalCut".to_string(),
                actual: "Other POWL Variant".to_string()
            })
        );
    }
}
